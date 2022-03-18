module LanguageServer.IdePurescript.Build
  ( collectByFirst
  , fullBuild
  , launchRebuildAndSendDiagnostics
  , maybeRebuildAndSendDiagnostics
  , positionToRange
  , rebuildAndSendDiagnostics
  )
  where

import Prelude
import Data.Array (filter, mapMaybe, notElem, uncons)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (over, un)
import Data.Nullable (toNullable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, catchError, finally, joinFiber, suspendAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import IdePurescript.Build (Command(Command), build, rebuild)
import IdePurescript.PscErrors (PscResult(..))
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import LanguageServer.IdePurescript.Config (addNpmPath, buildCommand, censorCodes, codegenTargets)
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.Server (loadAll)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.IdePurescript.Util (launchAffLog)
import LanguageServer.Protocol.Handlers (publishDiagnostics, sendDiagnosticsBegin, sendDiagnosticsEnd)
import LanguageServer.Protocol.TextDocument (TextDocument, getUri, getVersion)
import LanguageServer.Protocol.Types (Connection, Diagnostic(Diagnostic), DocumentStore, DocumentUri(..), Position(Position), Range(Range), Settings)
import LanguageServer.Protocol.Uri (uriToFilename)
import Node.Path (resolve)
import PscIde.Command (RebuildError(RebuildError))
import PscIde.Command as PC

positionToRange :: PC.RangePosition -> Range
positionToRange ({ startLine, startColumn, endLine, endColumn }) =
  Range
    { start: Position { line: startLine - 1, character: startColumn - 1 }
    , end: Position { line: endLine - 1, character: endColumn - 1 }
    }

type DiagnosticResult
  = { pscErrors :: Array RebuildError, diagnostics :: Object (Array Diagnostic) }

emptyDiagnostics :: DiagnosticResult
emptyDiagnostics = { pscErrors: [], diagnostics: Object.empty }

collectByFirst :: forall a. Array (Tuple (Maybe String) a) -> Object (Array a)
collectByFirst x = Object.fromFoldableWith (<>) $ mapMaybe f x
  where
  f (Tuple (Just a) b) = Just (Tuple a [ b ])
  f _ = Nothing

convertDiagnostics :: String -> Settings -> PscResult -> Effect DiagnosticResult
convertDiagnostics projectRoot settings (PscResult { warnings, errors }) =
  diagnostics
    <#>
      { diagnostics: _
      , pscErrors: errors <> warnings'
      }
  where
  diagnostics :: Effect (Object (Array Diagnostic))
  diagnostics = do
    diags <- allDiagnostics
    pure $ collectByFirst diags

  allDiagnostics :: Effect (Array (Tuple (Maybe String) Diagnostic))
  allDiagnostics =
    traverse (convertDiagnostic true) errors
      <> traverse (convertDiagnostic false) warnings'

  warnings' = censorWarnings settings warnings
  dummyRange =
    Range
      { start: Position { line: 1, character: 1 }
      , end: Position { line: 1, character: 1 }
      }

  convertDiagnostic :: Boolean -> RebuildError -> Effect (Tuple (Maybe String) Diagnostic)
  convertDiagnostic isError (RebuildError { errorCode, position, message, filename }) = do
    resolvedFile <- traverse (resolve [ projectRoot ]) filename
    pure
      $ Tuple resolvedFile
          ( Diagnostic
              { range: maybe dummyRange positionToRange position
              , severity: toNullable $ Just $ if isError then 1 else 2
              , code: toNullable $ Just $ errorCode
              , source: toNullable $ Just "PureScript"
              , message
              }
          )

getDiagnostics :: DocumentUri -> Settings -> ServerState -> Aff DiagnosticResult
getDiagnostics uri settings state = do
  filename <- liftEffect $ uriToFilename uri
  let targets = codegenTargets settings
  case state of
    ServerState { port: Just port, root: Just root } -> do
      { errors } <- rebuild port filename targets
      liftEffect $ convertDiagnostics root settings errors
    _ -> pure emptyDiagnostics

censorWarnings :: Settings -> Array RebuildError -> Array RebuildError
censorWarnings settings = filter (flip notElem codes <<< getCode)
  where
  getCode (RebuildError { errorCode }) = errorCode
  codes = censorCodes settings

foreign import parseShellQuote :: String -> Array String

fullBuild :: Notify -> DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff (Either String DiagnosticResult)
fullBuild logCb _ settings state _ = do
  let command = parseShellQuote $ buildCommand settings
  case state, uncons command of
    ServerState { port: maybePort, root: Just directory }, Just { head: cmd, tail: args } -> do
      build logCb { command: Command cmd args, directory, useNpmDir: addNpmPath settings }
        >>= either (pure <<< Left) \{ errors } -> do
            liftEffect $ logCb Info "Build complete"
            case maybePort of
              Nothing -> liftEffect $ logCb Error $ "Couldn't reload modules, no ide server port"
              Just port -> do
                attempt (loadAll port)
                  >>= case _ of
                      Left e -> liftEffect $ logCb Error $ "Error reloading modules: " <> show e
                      Right (Left msg) -> liftEffect $ logCb Error $ "Error message from IDE server reloading modules: " <> msg
                      _ -> liftEffect $ logCb Info "Reloaded modules"
            liftEffect $ Right <$> convertDiagnostics directory settings errors
    _, Nothing ->
      pure $ Left "Error parsing build command"
    ServerState { port, root }, _ -> do
      pure $ Left $ "Error running build: port=" <> show port <> ", root=" <> show root

-- | Builds module and provides diagnostics
rebuildAndSendDiagnostics ::
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Notify ->
  TextDocument ->
  Aff Unit
rebuildAndSendDiagnostics config conn state notify document = do
  let uri = getUri document
  c <- liftEffect $ Ref.read config
  s <- liftEffect $ Ref.read state
  when (Config.fastRebuild c) do
    liftEffect $ sendDiagnosticsBegin conn
    { pscErrors, diagnostics } <- getDiagnostics uri c s
    filename <- liftEffect $ uriToFilename uri
    let fileDiagnostics = fromMaybe [] $ Object.lookup filename diagnostics
    liftEffect do
      notify Info
        $ "Built with "
        <> show (Array.length fileDiagnostics)
        <> "/"
        <> show (Array.length pscErrors)
        <> " issues for file: "
        <> show filename
        <> ", all diagnostic files: "
        <> show (Object.keys diagnostics)
      let nonFileDiagnostics = Object.delete filename diagnostics
      when (Object.size nonFileDiagnostics > 0) do
        notify Info $ "Unmatched diagnostics: " <> show nonFileDiagnostics
      Ref.write
        ( over ServerState
            ( \s1 ->
                s1
                  { diagnostics = Object.insert (un DocumentUri uri) pscErrors (s1.diagnostics)
                  , modulesFile = Nothing -- Force reload of modules on next request
                  }
            )
            s
        )
        state
      publishDiagnostics conn
        { uri
        , diagnostics: fileDiagnostics
        }
      sendDiagnosticsEnd conn

-- | As `rebuildAndSendDiagnostics` but only allow one running rebuild at once, saving the fiber of the currently running rebuild in state
maybeRebuildAndSendDiagnostics ::
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Notify ->
  TextDocument ->
  Aff Boolean
maybeRebuildAndSendDiagnostics config conn state notify document = do
  runningRebuild <- liftEffect $ (_.runningRebuild <<< un ServerState) <$> Ref.read state
  version <- liftEffect $ getVersion document
  let documentUri = getUri document
  liftEffect $ notify Info $ "Running rebuild: " <> maybe "None" (\{ uri, version } -> show { uri, version }) runningRebuild
  case runningRebuild of
    Just _rebuildFiber -> do
      -- TODO or requeue after the fiber completes ? Cancel ?
      liftEffect $ notify Info $ "Rebuild requested for " <> show documentUri <> " when already running one, ignoring"
      pure false
    Nothing -> do
      fiber <-
        suspendAff
          $ finally (liftEffect $ Ref.modify_ (over ServerState $ _ { runningRebuild = Nothing }) state)
              (rebuildAndSendDiagnostics config conn state notify document)
      liftEffect do
        notify Info "Launched rebuild run"
        Ref.modify_ (over ServerState $ _ { runningRebuild = Just { fiber, uri: documentUri, version } }) state
      joinFiber fiber
      pure true

-- | See `maybeRebuildAndSendDiagnostics`
launchRebuildAndSendDiagnostics ::
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Notify ->
  TextDocument ->
  Effect Unit
launchRebuildAndSendDiagnostics config conn state notify document = void $ launchAffLog notify $ maybeRebuildAndSendDiagnostics config conn state notify document
