module LanguageServer.IdePurescript.Main
  ( main
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (or)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing, maybe, maybe')
import Data.Newtype (over, un, unwrap)
import Data.Nullable (toMaybe, toNullable)
import Data.Nullable as Nullable
import Data.Profunctor.Strong (first)
import Data.String (Pattern(..), contains)
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), apathize, attempt, delay, forkAff, launchAff_, try)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign, unsafeToForeign)
import Foreign.JSON (parseJSON)
import Foreign.Object (Object)
import Foreign.Object as Object
import IdePurescript.Modules (getModulesForFileTemp, initialModulesState)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import LanguageServer.IdePurescript.Assist (addClause, caseSplit, fillTypedHole, fixTypo)
import LanguageServer.IdePurescript.Build as Build
import LanguageServer.IdePurescript.Clean (clean)
import LanguageServer.IdePurescript.CodeActions (getActions, onReplaceAllSuggestions, onReplaceSuggestion)
import LanguageServer.IdePurescript.CodeLenses (getCodeLenses)
import LanguageServer.IdePurescript.CodeLenses as CodeLenses
import LanguageServer.IdePurescript.Commands (addClauseCmd, addCompletionImportCmd, addModuleImportCmd, buildCmd, caseSplitCmd, cleanCmd, cmdName, commands, fixTypoCmd, getAvailableModulesCmd, replaceAllSuggestionsCmd, replaceSuggestionCmd, restartPscIdeCmd, searchCmd, sortImportsCmd, startPscIdeCmd, stopPscIdeCmd, typedHoleExplicitCmd)
import LanguageServer.IdePurescript.Completion (getCompletions)
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.FileTypes as FileTypes
import LanguageServer.IdePurescript.FoldingRanges (getFoldingRanges)
import LanguageServer.IdePurescript.Formatting (getFormattedDocument)
import LanguageServer.IdePurescript.Imports (addCompletionImport, addModuleImport', getAllModules, reformatImports)
import LanguageServer.IdePurescript.References (getReferences)
import LanguageServer.IdePurescript.Search (search)
import LanguageServer.IdePurescript.Server as Server
import LanguageServer.IdePurescript.Symbols (getDefinition, getDocumentSymbols, getWorkspaceSymbols)
import LanguageServer.IdePurescript.Tooltips (getTooltips)
import LanguageServer.IdePurescript.Types (CommandHandler, ServerState(..), ServerStateRec)
import LanguageServer.IdePurescript.Util (launchAffLog)
import LanguageServer.IdePurescript.WatchedFiles (handleDidChangeWatchedFiles)
import LanguageServer.Protocol.Console (error, info, log, warn)
import LanguageServer.Protocol.DocumentStore (getDocument, onDidChangeContent, onDidCloseDocument, onDidSaveDocument)
import LanguageServer.Protocol.Handlers (onCodeAction, onCodeLens, onCompletion, onDefinition, onDidChangeConfiguration, onDidChangeWatchedFiles, onDocumentFormatting, onDocumentSymbol, onExecuteCommand, onFoldingRanges, onHover, onReferences, onShutdown, onWorkspaceSymbol, sendCleanBegin, sendCleanEnd)
import LanguageServer.Protocol.Setup (InitParams(..), getConfiguration, initConnection, initDocumentStore)
import LanguageServer.Protocol.TextDocument (getText)
import LanguageServer.Protocol.Types (Connection, DocumentStore, DocumentUri, Settings, TextDocumentIdentifier(..))
import LanguageServer.Protocol.Uri (uriToFilename)
import LanguageServer.Protocol.Window (createWorkDoneProgress, showError, showWarningWithActions, workBegin, workDone)
import LanguageServer.Protocol.Workspace (codeLensRefresh)
import Literals.Null (null)
import Node.Encoding as Encoding
import Node.FS.Sync as FSSync
import Node.Path (resolve)
import Node.Process as Process
import Unsafe.Coerce (unsafeCoerce)

defaultServerState :: ServerState
defaultServerState =
  ServerState
    { port: Nothing
    , deactivate: pure unit
    , root: Nothing
    , conn: Nothing
    , modules: initialModulesState
    , modulesFile: Nothing
    --
    , runningRebuild: Nothing
    --
    , fastRebuildQueue: Map.empty
    , diagnosticsQueue: Map.empty
    , fullBuildWaiting: Nothing
    , rebuildRunning: Nothing
    --
    , savedCacheDb: Nothing
    , revertCacheDbTimeout: Nothing
    --
    , diagnostics: Map.empty
    , clientCapabilities: Nothing
    , parsedModules: Map.empty
    }

modifyState ::
  Ref ServerState -> (ServerStateRec -> ServerStateRec) -> Effect ServerState
modifyState state mod =
  Ref.modify (over ServerState mod) state

modifyState_ ::
  Ref ServerState -> (ServerStateRec -> ServerStateRec) -> Effect Unit
modifyState_ state mod =
  void $ modifyState state mod

readState ::
  ∀ a.
  Ref ServerState -> (ServerStateRec -> a) -> Effect a
readState state get =
  get <$> un ServerState <$> Ref.read state

type CmdLineArguments
  = { config :: Maybe String
    , filename :: Maybe String
    , version :: Boolean
    }

-- | Parses command line arguments  passed to process.argv
parseArgs :: Array String -> Maybe CmdLineArguments
parseArgs allArgs = go 0 defaultArgs
  where
  args = Array.drop 2 allArgs

  defaultArgs = { config: Nothing, filename: Nothing, version: false }

  go i c =
    case args !! i of
      Just "--config" -> case args !! (i + 1) of
        Just conf -> go (i + 2) (c { config = Just conf })
        Nothing -> Nothing
      Just "--log" -> case args !! (i + 1) of
        Just filename -> go (i + 2) (c { filename = Just filename })
        Nothing -> Nothing
      Just "--version" -> go (i + 1) (c { version = true })
      -- stdio etc
      Just _ -> go (i + 1) c
      Nothing -> Just c

updateModules ::
  Ref ServerState -> DocumentStore -> DocumentUri -> Aff (Maybe ServerState)
updateModules state documents uri =
  liftEffect (Ref.read state)
    >>= case _ of
        ServerState { port: Just port, modulesFile }
          | modulesFile /= Just uri -> do
            maybeDoc <- liftEffect $ getDocument documents uri
            for (Nullable.toMaybe maybeDoc) \doc -> do
              text <- liftEffect $ getText doc
              path <- liftEffect $ uriToFilename uri
              modules <- getModulesForFileTemp port path text
              liftEffect
                $ modifyState state
                    _ { modules = modules, modulesFile = Just uri }
        _ -> pure Nothing

mkRunHandler ::
  Ref Foreign ->
  Ref ServerState ->
  DocumentStore ->
  String ->
  forall a b.
  (b -> Maybe DocumentUri) ->
  (Settings -> ServerState -> b -> Aff a) ->
  b ->
  Effect (Promise a)
mkRunHandler config state documents _handlerName maybeGetDocUri f b =
  Promise.fromAff do
    -- Should not allow any js files through
    let mayUri = do
                uri <- maybeGetDocUri b
                case FileTypes.uriToRelevantFileType uri of
                  FileTypes.JavaScriptFile -> Nothing
                  _ -> pure uri

    case mayUri of
      Nothing -> 
        -- TODO :(
        -- Returning null here rather than throwing an exception which will show up in output. Likely null is actually
        -- valid return from all handlers and types can be adjusted; alternatively perhaps these handlers can only be registered
        -- for .purs while getting changes for others.
        pure $ unsafeCoerce null
      Just _ -> do
        c <- liftEffect $ Ref.read config
        ms <- maybe (pure Nothing) (updateModules state documents) mayUri
        s <- maybe' (\_ -> liftEffect $ Ref.read state) pure ms
        f c s b

-- | Extracts document uri value
getTextDocUri ::
  forall r.
  { textDocument :: TextDocumentIdentifier | r } -> Maybe DocumentUri
getTextDocUri = (Just <<< _.uri <<< un TextDocumentIdentifier <<< _.textDocument)

-- | This mutes buggy warning coming from purs-ide, just to keep the output clean.
-- | The issue: https://github.com/purescript/purescript/issues/3377
muteReexportsWarn ::
  (Connection → String → Effect Unit) -> Connection -> String -> Effect Unit
muteReexportsWarn logFn con str =
  if str # contains (Pattern "Failed to resolve reexports for Type.") then
    pure unit
  else
    logFn con str

mkNotify ::
  Maybe String -> Ref ServerState -> Notify
mkNotify logFile state l s = do
  readState state _.conn
    >>= maybe (pure unit)
        ( flip
            case l of
              Success -> log
              Info -> muteReexportsWarn info
              Warning -> warn
              Error -> error
            s
        )
  case logFile of
    Just filename ->
      FSSync.appendTextFile Encoding.UTF8 filename ("[" <> show l <> "] " <> s <> "\n")
    Nothing -> pure unit

-- | Stops IDE server
mkStopPscIdeServer :: Ref ServerState -> Notify -> Aff Unit
mkStopPscIdeServer state notify = do
  quit <- liftEffect (_.deactivate <$> unwrap <$> Ref.read state)
  quit
  liftEffect do
    Ref.modify_
      (over ServerState $ _ { port = Nothing, deactivate = pure unit, modules = initialModulesState, runningRebuild = Nothing })
      state
    notify Success "Stopped IDE server"

-- | Reads workspace root from state
buildWarningDialog ::
  Ref Foreign -> Connection -> Ref ServerState -> (ErrorLevel -> String -> Effect Unit) -> String -> Aff Unit
buildWarningDialog config conn state notify msg = do
  let buildOption = "Build project"
  action <-
    showWarningWithActions conn
      ( msg
          <> ". \n\nEnsure project is built with the same purs version as the IDE server is using"
      )
      [ buildOption ]
  when (action == Just buildOption) do
    liftEffect $ info conn $ "Building by request from warning dialog"
    --Build.buildProject config conn state notify true documents []
    liftEffect $ Build.requestFullBuild config conn state notify

-- | Tries to start IDE server at workspace root
mkStartPscIdeServer :: Ref Foreign -> Connection -> Ref ServerState -> Notify -> Aff Unit
mkStartPscIdeServer config conn state notify = do
  liftEffect $ notify Info "Starting IDE server"
  progressReporter <- createWorkDoneProgress conn
  liftEffect $ workBegin progressReporter { title: "Starting PureScript IDE server" }
  rootPath <- liftEffect $ Build.getWorkspaceRoot state
  settings <- liftEffect $ Ref.read config
  startRes <- Server.startServer' settings rootPath notify notify
  Server.retry notify 6 case startRes of
    { port: Just port, quit } -> do
      Server.loadAll port
        >>= case _ of
            Left msg
              | String.contains (Pattern "Version mismatch for the externs") msg -> do
                liftEffect $ info conn $ "Error loading modules: " <> msg
                buildWarningDialog config conn state notify
                  $ msg
                  <> ". Ensure project is built with the same purs version as the IDE server is using"

            Left msg ->
              liftEffect
                $ notify Info
                $ "Non-fatal error loading modules: "
                <> msg
            _ -> pure unit
      liftEffect do
        Ref.modify_
          (over ServerState $ _ { port = Just port, deactivate = quit })
          state
        ServerState { clientCapabilities } <- liftEffect $ Ref.read state
        when (CodeLenses.supportsRefresh clientCapabilities) do
          codeLensRefresh conn
    _ -> pure unit
  liftEffect $ workDone progressReporter
  liftEffect $ Build.checkBuildTasks config conn state notify

connect :: Ref ServerState -> Effect Connection
connect state =
  initConnection
    commands \({ params: InitParams { rootPath, rootUri, capabilities }, conn }) -> do
    Process.argv >>= \args -> log conn $ "Starting with args: " <> show args
    root <- case toMaybe rootUri, toMaybe rootPath of
      Just uri, _ -> Just <$> uriToFilename uri
      _, Just path -> pure $ Just path
      Nothing, Nothing -> pure Nothing
    workingRoot <- maybe Process.cwd pure root
    modifyState_ state
      _
        { root = Just workingRoot
        , clientCapabilities = Just capabilities
        }
    ( \(Tuple dir root') ->
        log conn ("Starting with cwd: " <> dir <> " and using root path: " <> root')
    )
      =<< Tuple
      <$> Process.cwd
      <*> pure workingRoot
    Ref.modify_ (over ServerState $ _ { conn = Just conn }) state

-- | Deletes output from previous build
cleanProject :: Connection -> Foreign -> Aff Unit
cleanProject conn config = do
  liftEffect $ sendCleanBegin conn
  liftEffect $ info conn "Started cleaning compiled output"
  clean config
    >>= case _ of
        Left err ->
          liftEffect do
            error conn err
            showError conn err
        Right msg ->
          liftEffect do
            log conn $ msg
  liftEffect $ info conn "Finished cleaning compiled output"
  liftEffect $ sendCleanEnd conn

-- | Starts PscIDE Server if autoStart enabled in config.
autoStartPcsIdeServer ::
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Notify ->
  Aff Unit
autoStartPcsIdeServer config conn state notify = do
  let workspaceRoot = Build.getWorkspaceRoot state
  let launchAff = void <<< launchAffLog notify
  let startPscIdeServer = mkStartPscIdeServer config conn state notify
  let resolvePath p = workspaceRoot >>= \root -> resolve [ root ] p
  -- Ensure we only run once
  c <- liftEffect $ Ref.read config
  when (Config.autoStartPscIde c)
    $ do
        startPscIdeServer
        hasPackageFile <- liftEffect $ 
          or
            <$> traverse (FSSync.exists <=< resolvePath)
                [ "bower.json", "psc-package.json", "spago.dhall", "spago.yaml", "flake.nix", "shell.nix" ]
        envIdeSources <- Server.getEnvPursIdeSources
        when (not hasPackageFile && isNothing envIdeSources) do
          liftEffect
            $ showError conn
                ( "It doesn't look like the workspace root is a PureScript project"
                    <> "(has bower.json/psc-package.json/spago.dhall/flake.nix/shell.nix)."
                    <> "The PureScript project should be opened as a root workspace folder."
                )
        outputDir <- liftEffect $ Build.getOutputDir config state
        exists <- liftEffect $ FSSync.exists outputDir
        unless exists
          $ liftEffect
          $ launchAff do
              let message = "Output directory does not exist at '" <> outputDir <> "'"
              liftEffect $ info conn message
              buildWarningDialog config conn state notify
                $ message
                <> ". Ensure project is built, or check configuration of output directory and build command."

-- | Puts event handlers
handleEvents ::
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  DocumentStore -> Notify -> Effect Unit
handleEvents config conn state documents notify = do
  let
    runHandler = mkRunHandler config state documents
    stopPscIdeServer = mkStopPscIdeServer state notify
  onCompletion conn
    $ runHandler
        "onCompletion" getTextDocUri (getCompletions notify documents)
  onDefinition conn
    $ runHandler
        "onDefinition" getTextDocUri (getDefinition notify documents)
  onDocumentSymbol conn
    $ runHandler
        "onDocumentSymbol" getTextDocUri getDocumentSymbols
  onWorkspaceSymbol conn
    $ runHandler
        "onWorkspaceSymbol" (const Nothing) getWorkspaceSymbols
  onFoldingRanges conn
    $ runHandler
        "onFoldingRanges" getTextDocUri (getFoldingRanges notify documents)
  onDocumentFormatting conn
    $ runHandler
        "onDocumentFormatting" getTextDocUri (getFormattedDocument notify documents)
  onReferences conn
    $ runHandler
        "onReferences" getTextDocUri (getReferences documents)
  onHover conn
    $ runHandler
        "onHover" getTextDocUri (getTooltips documents)
  onCodeAction conn
    $ runHandler
        "onCodeAction" getTextDocUri (getActions documents)
  onCodeLens conn
    $ runHandler
        "onCodeLens" getTextDocUri (getCodeLenses notify state documents)
  onShutdown conn $ Promise.fromAff stopPscIdeServer
  --
  onDidChangeWatchedFiles
    conn
    $ launchAff_
    <<< handleDidChangeWatchedFiles config conn state documents
  --
  onDidChangeContent documents
    $ \{ document } -> do
        Build.handleDocumentChange config conn state notify document documents
  --
  onDidSaveDocument documents
    $ \{ document } -> do
        Build.handleDocumentSave config conn state notify document documents
  --
  onDidCloseDocument documents
    $ \{ document } -> do
        Build.handleDocumentClose config conn state notify document documents

handleConfig ::
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Maybe Foreign -> Notify -> Aff Unit
handleConfig config conn state cmdLineConfig notify = do
  let launchAff = void <<< launchAffLog notify
  gotConfig :: AVar Unit <- AVar.empty
  let
    setConfig :: String -> Foreign -> Aff Unit
    setConfig source newConfig = do
      liftEffect do
        log conn $ "Got new config (" <> source <> ")"
        Ref.write newConfig config
      AVar.tryPut unit gotConfig
        >>= case _ of
            true -> pure unit
            false -> liftEffect $ log conn "Not starting server, already started"
  liftEffect
    $ onDidChangeConfiguration conn
    $ \{ settings } ->
        launchAff $ setConfig "client push" settings
  _ <-
    forkAff do
      -- Ensure we only run once
      AVar.read gotConfig
      autoStartPcsIdeServer config conn state notify
  -- 1. Config on command line - go immediately
  maybe (pure unit) (setConfig "command line") cmdLineConfig
  delay (Milliseconds 50.0)
  -- 2. Config may be pushed immediately
  got1 <- AVar.isFilled <$> AVar.status gotConfig
  unless got1 do
    -- 3. Fetch config via pull (waited 50ms as at least in vscode, not ready immediately)
    initialConfig <- attempt $ getConfiguration conn
    case initialConfig of
      Right ic ->
        setConfig "by request" ic
      Left error -> do
        liftEffect $ log conn $ "Failed to request settings: " <> show error
        -- 4. Wait some time longer for possible config push, then proceed with no config
        delay (Milliseconds 200.0)
        got2 <- AVar.isFilled <$> AVar.status gotConfig
        unless got2 do
          liftEffect $ notify Warning "Proceeding with no config received"
          void $ AVar.tryPut unit gotConfig

-- | Registers commands handlers
handleCommands ::
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  DocumentStore ->
  Notify ->
  Effect Unit
handleCommands config conn state documents notify = do
  let
    onBuild _ _ _ _ = liftEffect $ Build.requestFullBuild config conn state notify
    onClean _ c _ _ = cleanProject conn c
    stopPscIdeServer = mkStopPscIdeServer state notify
    startPscIdeServer = mkStartPscIdeServer config conn state notify
    restartPscIdeServer = do
      apathize stopPscIdeServer
      startPscIdeServer
  let noResult = unsafeToForeign $ toNullable Nothing
  let
    voidHandler :: forall a. CommandHandler a -> CommandHandler Foreign
    voidHandler h d c s a =
      try (h d c s a)
        >>= case _ of
            Left err -> do
              liftEffect $ notify Error $ show err
              pure noResult
            Right _ -> pure noResult
    simpleHandler h _ _ _ _ = h $> noResult
  let
    handlers :: Object (CommandHandler Foreign)
    handlers =
      Object.fromFoldable
        $ first cmdName
        <$>
          [ caseSplitCmd /\ voidHandler caseSplit
          , addClauseCmd /\ voidHandler addClause
          , replaceSuggestionCmd /\ voidHandler onReplaceSuggestion
          , replaceAllSuggestionsCmd /\ voidHandler onReplaceAllSuggestions
          , buildCmd /\ voidHandler onBuild
          , cleanCmd /\ voidHandler onClean
          , addCompletionImportCmd /\ addCompletionImport notify
          , addModuleImportCmd /\ voidHandler (addModuleImport' notify)
          , sortImportsCmd /\ reformatImports notify
          , startPscIdeCmd /\ simpleHandler startPscIdeServer
          , stopPscIdeCmd /\ simpleHandler stopPscIdeServer
          , restartPscIdeCmd /\ simpleHandler restartPscIdeServer
          , getAvailableModulesCmd /\ getAllModules notify
          , searchCmd /\ search
          , fixTypoCmd /\ fixTypo notify
          , typedHoleExplicitCmd /\ voidHandler (fillTypedHole notify)
          ]
  onExecuteCommand conn
    $ \{ command, arguments } ->
        Promise.fromAff do
          c <- liftEffect $ Ref.read config
          s <- liftEffect $ Ref.read state
          case Object.lookup command handlers of
            Just handler -> handler documents c s arguments
            Nothing -> do
              liftEffect $ error conn $ "Unknown command: " <> command
              pure noResult

foreign import version :: Effect String

-- | main function parses the CLI arguments
-- | and calls main' with parsed args to launch effects
main :: Effect Unit
main = do
  maybeArgs <- parseArgs <$> Process.argv
  case maybeArgs of
    Nothing -> do
      Console.error "Error parsing args"
      Process.exit 1
    Just { version: true } -> do
      v <- version
      Console.log v
    Just args -> do
      maybe (pure unit) (flip (FSSync.writeTextFile Encoding.UTF8) "Starting logging...\n") args.filename
      let
        config' = case args.config of
          Just c -> either (const Nothing) Just $ runExcept $ parseJSON c
          Nothing -> Nothing
      main' { config: config', filename: args.filename }

main' ::
  { config :: Maybe Foreign
  , filename :: Maybe String
  } ->
  Effect Unit
main' { filename: logFile, config: cmdLineConfig } = do
  state <- Ref.new defaultServerState
  config <- Ref.new (unsafeToForeign {})
  conn <- connect state
  documents <- initDocumentStore conn
  let notify = mkNotify logFile state
  handleEvents config conn state documents notify
  handleCommands config conn state documents notify
  void $ launchAffLog notify $ handleConfig config conn state cmdLineConfig notify
  plsVersion <- version
  log conn $ "PureScript Language Server started (" <> plsVersion <> ")"
