module LanguageServer.IdePurescript.Build
  ( getOutputDir
  , getWorkspaceRoot
  , handleDocumentChange
  , handleDocumentSave
  , handleDocumentClose
  , positionToRange
  , requestFullBuild
  , checkBuildTasks
  ) where

import Prelude

import Control.Monad.Except (catchError)
import Control.Parallel (parSequence_)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array (filter, mapMaybe, notElem, uncons)
import Data.Array as Array
import Data.DateTime as DT
import Data.Either (Either(..), either, hush)
import Data.Foldable (for_, or)
import Data.Int (toNumber)
import Data.JSDate as JSDate
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust, isNothing)
import Data.Newtype (over, un, unwrap)
import Data.Nullable (notNull, toNullable)
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
import Foreign (Foreign)
import IdePurescript.Build (Command(Command), build, rebuild)
import IdePurescript.PscErrors (PscResult(..))
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.Server (loadAll)
import LanguageServer.IdePurescript.Types (DiagnosticState, RebuildRunning(..), ServerState(..), ServerStateRec)
import LanguageServer.Protocol.Console (error, log)
import LanguageServer.Protocol.Handlers (publishDiagnostics, sendDiagnosticsBegin, sendDiagnosticsEnd)
import LanguageServer.Protocol.TextDocument (TextDocument, getText, getUri, getVersion)
import LanguageServer.Protocol.Types (Connection, Diagnostic(Diagnostic), DocumentUri, Position(Position), Range(Range), Settings)
import LanguageServer.Protocol.Uri (filenameToUri, uriToFilename)
import LanguageServer.Protocol.Window (createWorkDoneProgress, showError, workBegin, workDone)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (resolve)
import Node.Process as Process
import PscIde.Command (RebuildError(..))
import PscIde.Command as PC
import PureScript.CST as CST
import PureScript.CST.Types (Module, ModuleName(..))

positionToRange :: PC.RangePosition -> Range
positionToRange ({ startLine, startColumn, endLine, endColumn }) =
  Range
    { start: Position { line: startLine - 1, character: startColumn - 1 }
    , end: Position { line: endLine - 1, character: endColumn - 1 }
    }

getWorkspaceRoot :: Ref ServerState -> Effect String
getWorkspaceRoot state = do
  root <- readState state _.root
  maybe Process.cwd pure root

getOutputDir :: Ref Foreign -> Ref ServerState -> Effect String
getOutputDir config state = do
  c <- Ref.read config
  root <- getWorkspaceRoot state
  resolve [ root ]
    $ Config.effectiveOutputDirectory c

getOutputDir_ :: Foreign -> ServerState -> Effect String
getOutputDir_ cfg (ServerState st) = do
  wsRoot <- maybe Process.cwd pure st.root
  resolve [ wsRoot ]
    $ Config.effectiveOutputDirectory cfg

-- BUILD EXTERNAL API
requestFullBuild ::
  Ref Foreign -> Connection -> Ref ServerState -> Notify -> Effect Unit
requestFullBuild config conn state notify = do
  modifyState_ state _ { fullBuildWaiting = Just { progress: true } }
  checkBuildTasks config conn state notify

{-| Main logic of running build tasks.

This function deals with managing state related to launching
and running all kinds of rebuilds (full, fast, diagnostics).
-}
checkBuildTasks ::
  Ref Foreign -> Connection -> Ref ServerState -> Notify -> Effect Unit
checkBuildTasks config conn state notify = do
  st <- readState state identity
  cfg <- Ref.read config
  when (serverIsReady st) do
    case st.rebuildRunning of
      Just FullBuild ->
        pure unit

      Just (FastRebuild running) ->
        runFastRebuilds st running

      Just (DiagnosticsRebuild running) ->
        runDiagnostics cfg st running

      Nothing ->
        if not Map.isEmpty st.diagnosticsQueue then
          runDiagnostics cfg st Map.empty
        else if not Map.isEmpty st.fastRebuildQueue then
          runFastRebuilds st Map.empty
        else
          runFull cfg st
  where
  serverIsReady st = isJust st.port
  runAgain _ = checkBuildTasks config conn state notify

  needRevert cfg =
    not Config.noFsDiagnostics cfg && Config.revertExternsAndCacheDb cfg

  rebuildWithDiag =
    rebuildWithDiagnostics config conn state notify
  rebuildWithCodegen = rebuildWithDiag false
  rebuildNoCodegen = rebuildWithDiag true

  runDiagnostics cfg st running =
    let
      docs = Map.difference st.diagnosticsQueue running
    in
      if Map.isEmpty docs then
        pure unit
      else do
        modifyState_ state \s ->
          s
            { rebuildRunning = Just $ DiagnosticsRebuild (Map.union docs running)
            , diagnosticsQueue = Map.difference s.diagnosticsQueue docs
            }
        launchAff_ do
          when (needRevert cfg) $ saveCacheDb config conn state
          traverse_ rebuildNoCodegen docs
          {- debounce diagnostics -}
          delay (Milliseconds $ toNumber $ Config.diagnosticsOnTypeDebounce cfg)
          liftEffect do
            modifyState_ state (finishDiagnosticsRunning docs)
            {- revert cache-db if nothing happens after a timeout -}
            revertCacheDbAfterTimeout conn state (Config.cacheDbRevertTimeout cfg)
            runAgain unit

  runFastRebuilds st running =
    let
      docs = Map.difference st.fastRebuildQueue running
    in
      if Map.isEmpty docs then
        pure unit
      else do
        modifyState_ state \s ->
          s
            { rebuildRunning = Just $ FastRebuild (Map.union docs running)
            , fastRebuildQueue = Map.difference s.fastRebuildQueue docs
            }
        launchAff_ do
          revertCacheDb conn state
          traverse_ rebuildWithCodegen docs
          liftEffect do
            modifyState_ state (finishFastRebuildRunning docs)
            runAgain unit

  runFull cfg st =
    case st.fullBuildWaiting of
      Nothing ->
        pure unit
      Just { progress } -> do
        modifyState_ state _ { rebuildRunning = Just FullBuild }
        launchAff_ do
          when (needRevert cfg) $ revertCacheDb conn state
          fullBuildWithDiagnostics config conn state notify progress
          liftEffect do
            modifyState_ state _ { rebuildRunning = Nothing }
            runAgain unit
        {- set waiting to Nothing after debounce timeout:
        other full build request made (e.g., consequent saves)
        during this timeout will be ignored -}
        launchAff_ do
          delay (Milliseconds 100.0)
          liftEffect $ modifyState_ state _ { fullBuildWaiting = Nothing }

isLibSourceFile :: String -> Boolean
isLibSourceFile path =
  or
    $ [ ".spago", "bower_components" ]
    <#> (flip String.contains) path
    <<< String.Pattern

{-| Document content change handler. -}
handleDocumentChange ::
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Notify ->
  TextDocument ->
  Effect Unit
handleDocumentChange config conn state notify document = do
  _ <- parseModuleDocument state document
  cfg <- Ref.read config
  version <- getVersion document
  let
    isLibSource = isLibSourceFile $ unwrap $ getUri document
    doHandle =
      case version of
        1.0 ->
          Config.diagnosticsOnOpen cfg
            && ( Config.revertExternsAndCacheDb cfg
                  || Config.noFsDiagnostics cfg
                  {- exclude rebuild for just opened library modules -}
                  || not isLibSource
              )
        _ ->
          Config.diagnosticsOnType cfg
  when doHandle do
    addToDiagnosticsQueue state document
    checkBuildTasks config conn state notify

{-| Document save handler. -}
handleDocumentSave ::
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Notify ->
  TextDocument ->
  Effect Unit
handleDocumentSave config conn state notify document = do
  cfg <- Ref.read config
  when (Config.fastRebuild cfg)
    $ addToFastRebuildQueue state document
  when (Config.fullBuildOnSave cfg)
    $ modifyState_ state
        _
          { fullBuildWaiting =
            Just
              { progress: Config.fullBuildOnSaveProgress cfg
              }
          }
  checkBuildTasks config conn state notify

handleDocumentClose ::
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Notify ->
  TextDocument ->
  Effect Unit
handleDocumentClose _ conn state notify document = do
  filename <- uriToFilename uri
  notify Info $ "Handling document close event: " <> show filename
  _ <- removeDocumentFromQueues state document
  modifyState_ state \s ->
    s
      { parsedModules = Map.delete uri s.parsedModules
      , diagnostics = Map.delete uri s.diagnostics
      }
  {- Remove diagnostics from the editor if the file was deleted. -}
  launchAff_ do
    exists <- FS.exists filename
    unless exists do
      liftEffect
        $ publishDiagnostics conn
            { uri
            , diagnostics: []
            }
  where
  uri = getUri document

-- STATE HELPERS
readState ::
  ∀ a.
  Ref ServerState -> (ServerStateRec -> a) -> Effect a
readState state get =
  get <$> un ServerState <$> Ref.read state

modifyState ::
  Ref ServerState -> (ServerStateRec -> ServerStateRec) -> Effect ServerState
modifyState state mod =
  Ref.modify (over ServerState mod) state

modifyState_ ::
  Ref ServerState -> (ServerStateRec -> ServerStateRec) -> Effect Unit
modifyState_ state mod =
  void $ modifyState state mod

addToFastRebuildQueue :: Ref ServerState -> TextDocument -> Effect Unit
addToFastRebuildQueue state document =
  modifyState_ state \s ->
    s
      { fastRebuildQueue = Map.insert (getUri document) document s.fastRebuildQueue
      }

finishFastRebuildRunning :: Map DocumentUri TextDocument -> ServerStateRec -> ServerStateRec
finishFastRebuildRunning docs st =
  case st.rebuildRunning of
    Just (FastRebuild running) ->
      let
        left = Map.difference running docs
      in
        if Map.isEmpty left then
          st { rebuildRunning = Nothing }
        else
          st { rebuildRunning = Just $ FastRebuild left }
    _ ->
      st

addToDiagnosticsQueue :: Ref ServerState -> TextDocument -> Effect Unit
addToDiagnosticsQueue state document =
  modifyState_ state \s ->
    s
      { diagnosticsQueue = Map.insert (getUri document) document s.diagnosticsQueue
      }

finishDiagnosticsRunning :: Map DocumentUri TextDocument -> ServerStateRec -> ServerStateRec
finishDiagnosticsRunning docs st =
  case st.rebuildRunning of
    Just (DiagnosticsRebuild running) ->
      let
        left = Map.difference running docs
      in
        if Map.isEmpty left then
          st { rebuildRunning = Nothing }
        else
          st { rebuildRunning = Just $ DiagnosticsRebuild left }
    _ ->
      st

removeDocumentFromQueues :: Ref ServerState -> TextDocument -> Effect Unit
removeDocumentFromQueues state document =
  modifyState_ state \s ->
    s
      { fastRebuildQueue = Map.delete (getUri document) s.fastRebuildQueue
      , diagnosticsQueue = Map.delete (getUri document) s.diagnosticsQueue
      }

-- CACHE DB
{- Reads cache-db.json and puts it into state, so it can be reverted later.
-}
saveCacheDb :: Ref Foreign -> Connection -> Ref ServerState -> Aff Unit
saveCacheDb config conn state = do
  st <- liftEffect $ readState state identity
  liftEffect $ clearCacheDbRevert state
  when (isNothing st.savedCacheDb) do
    liftEffect $ log conn "Saving cache-db state"
    cacheDbPath <-
      liftEffect
        $ getOutputDir config state
        <#> \output -> output <> "/" <> "cache-db.json"
    cacheDb <- do
      attempt do
        _ <- FS.stat cacheDbPath
        FS.readTextFile UTF8 cacheDbPath
          <#> \source ->
              { path: cacheDbPath
              , source
              }
    liftEffect
      $ modifyState_ state
          _
            { savedCacheDb = hush $ cacheDb
            }

revertCacheDb :: Connection -> Ref ServerState -> Aff Unit
revertCacheDb conn state = do
  st <- liftEffect $ readState state identity
  case st.savedCacheDb of
    Just { path: cacheDbPath, source } -> do
      liftEffect $ log conn "Reverting cache-db state"
      liftEffect $ clearCacheDbRevert state
      flip catchError
        ( \err ->
            liftEffect $ log conn $ show err
        ) do
        FS.writeTextFile UTF8 cacheDbPath source
      liftEffect
        $ modifyState_ state
            _ { savedCacheDb = Nothing }
    Nothing ->
      pure unit

clearCacheDbRevert :: Ref ServerState -> Effect Unit
clearCacheDbRevert state = do
  st <- readState state identity
  case st.revertCacheDbTimeout of
    Just id -> do
      clearTimeout id
      modifyState_ state _ { revertCacheDbTimeout = Nothing }
    Nothing -> pure unit

revertCacheDbAfterTimeout :: Connection -> Ref ServerState -> Int -> Effect Unit
revertCacheDbAfterTimeout conn state timeout = do
  clearCacheDbRevert state
  st <- liftEffect $ readState state identity
  when (isJust st.savedCacheDb) do
    id <-
      setTimeout timeout (launchAff_ $ revertCacheDb conn state)
    modifyState_ state
      _ { revertCacheDbTimeout = Just id }

-- DIAGNOSTICS STUFF
type DiagnosticResult
  = { pscErrors :: Array RebuildError
    , diagnostics :: Map DocumentUri (Array Diagnostic)
    }

emptyDiagnostics :: DiagnosticResult
emptyDiagnostics = { pscErrors: [], diagnostics: Map.empty }

collectByFirst :: forall a. Array (Maybe DocumentUri /\ a) -> Map DocumentUri (Array a)
collectByFirst x = Map.fromFoldableWith (<>) $ mapMaybe f x
  where
  f (Just a /\ b) = Just (a /\ [ b ])
  f _ = Nothing

convertDiagnostics ::
  String -> Settings -> PscResult -> Effect DiagnosticResult
convertDiagnostics projectRoot settings (PscResult { warnings, errors }) =
  diagnostics
    <#>
      { diagnostics: _
      , pscErrors: errors <> warnings'
      }
  where
  diagnostics = do
    diags <- allDiagnostics
    pure $ collectByFirst diags

  allDiagnostics =
    traverse (convertDiagnostic true) errors
      <> traverse (convertDiagnostic false) warnings'

  warnings' = censorWarnings settings warnings
  dummyRange =
    Range
      { start: Position { line: 1, character: 1 }
      , end: Position { line: 1, character: 1 }
      }

  convertDiagnostic isError
    (RebuildError { errorCode, position, message, filename }) = do
    uri <- traverse (resolve [ projectRoot ] >>> (=<<) filenameToUri) filename
    pure
      $ uri
      /\ ( Diagnostic
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
  let targets = Config.codegenTargets settings
  case state of
    ServerState { port: Just port, root: Just root } -> do
      { errors } <- rebuild port filename (Just filename) targets
      liftEffect $ convertDiagnostics root settings errors
    _ -> pure emptyDiagnostics

type DestPair
  = { org :: String, tmp :: String }

type DestFiles
  = { source :: DestPair, foreign :: Maybe DestPair }

newtype ForeignExt
  = ForeignExt String

getForeignExt :: String -> ForeignExt
getForeignExt ext =
  ForeignExt
    $ "."
    <> fromMaybe ext (String.stripPrefix (String.Pattern ".") ext)

getDestFiles :: String -> ForeignExt -> Aff DestFiles
getDestFiles filename (ForeignExt foreignExt) = do
  tmpDir <- liftEffect $ getOsTmpDir
  foreignFile <-
    case foreignPath of
      Just fp ->
        FS.exists fp
          <#> \exists -> if exists then Just fp else Nothing
      _ -> pure Nothing
  pure
    { source:
        { org: filename
        , tmp: tmpDir <> "/" <> hash <> ".purs"
        }
    , foreign:
        foreignFile
          <#> \org ->
              { org
              , tmp: tmpDir <> "/" <> hash <> foreignExt
              }
    }
  where
  hash = (getHash filename)
  foreignPath =
    String.stripSuffix (String.Pattern ".purs") filename
      <#> (flip (<>) foreignExt)

hasSevereErrors :: PscResult -> Boolean
hasSevereErrors (PscResult { errors }) =
  not Array.null errors

{- Gets diagnostics for a module without code generation.

By:

- copying a source.purs file to temporary location
- copying there a foreign module file
- sending temporary file path as filename and real location as actual filename

Currently the problem with ide rebuild:

- When file source code contains no severe errors, *externs.cbor* and *cache-db.json*
  will be modified even though we didn't ask to make any code generation.

- So, in case of revert is enabled via config, we make a hack:
  - before rebuild: copy externs.cbor file
  - after rebuild: revert previous version back

- https://github.com/purescript/purescript/pull/4178 PR is trying to resolve
  this.

-}
getDiagnosticsOnType ::
  TextDocument -> Settings -> ServerState -> (Maybe ModuleName) -> Aff DiagnosticResult
getDiagnosticsOnType document cfg state moduleName = do
  let
    targets =
      if Config.diagnosticsCodegen cfg then
        Config.codegenTargets cfg
      else
        Just []
  let uri = getUri document
  case state of
    ServerState { port: Just port, root: Just root } -> do
      files <- do
        filename <- liftEffect $ uriToFilename uri
        getDestFiles filename (getForeignExt $ Config.foreignExt cfg)
      currentText <- liftEffect $ getText document
      externs <- saveExtern currentText
      FS.writeTextFile UTF8 files.source.tmp currentText
      case files.foreign of
        Just { org, tmp } ->
          Promise.toAffE $ copyFile org tmp
        Nothing ->
          pure unit
      { errors } <-
        rebuild port files.source.tmp (Just files.source.org) targets
      diagResult <-
        liftEffect $ convertDiagnostics root cfg errors
      {- clean up tmp files -}
      parSequence_
        [ void $ attempt $ FS.unlink files.source.tmp
        , maybe (pure unit) (void <<< attempt <<< FS.unlink) (files.foreign <#> _.tmp)
        , if (hasSevereErrors errors) then
            pure unit
          else
            revertExtern externs
        ]
      pure diagResult
    _ ->
      pure emptyDiagnostics
  where

  revertExtern saved = do
    case saved of
      Just externs ->
        Promise.toAffE
          $ copyFile externs.saved externs.org
      _ ->
        pure unit
    case saved of
      Just externs ->
        void $ FS.unlink externs.saved
      _ ->
        pure unit

  shouldRevert currentText =
    case Config.revertExternsAndCacheDb cfg of
      false -> pure false
      true -> do
        {- revert only if document has not been changed in the editor and content on
        the disk is the same -}
        version <- liftEffect $ getVersion document
        if (version == 1.0) then do
          orgPath <- liftEffect $ uriToFilename (getUri document)
          contentOnDisk <- attempt $ FS.readTextFile UTF8 orgPath
          pure $ either (const false) (eq currentText) contentOnDisk
        else
          pure false

  saveExtern currentText = do
    case moduleName of
      Nothing ->
        pure Nothing
      Just (ModuleName mn) -> do
        should <- shouldRevert currentText
        if not should then
          pure Nothing
        else do
          output <- liftEffect $ getOutputDir_ cfg state
          let getPath fName = output <> "/" <> mn <> "/" <> fName
          let
            externs =
              { org: getPath "externs.cbor"
              , saved: getPath "externs.saved.cbor"
              }
          exists <- FS.exists externs.org
          if exists then do
            Promise.toAffE
              $ copyFile externs.org externs.saved
            pure
              $ Just externs
          else
            pure Nothing

unwrapModuleName :: CST.RecoveredParserResult Module -> Maybe (ModuleName)
unwrapModuleName r =
  case r of
    CST.ParseSucceeded m ->
      Just $ (unwrap (unwrap (unwrap m).header).name).name
    _ ->
      Nothing

getNoFsDiagnostics :: TextDocument -> Settings -> ServerState -> Aff DiagnosticResult
getNoFsDiagnostics document settings state = do
  --let targets = Just []
  -- need to send default targets to enable foreign check
  let targets = Config.codegenTargets settings
  let uri = getUri document
  case state of
    ServerState { port: Just port, root: Just root } -> do
      filename <- liftEffect $ uriToFilename uri
      currentText <- liftEffect $ getText document
      { errors } <- rebuild port ("data:" <> currentText) (Just filename) targets
      liftEffect $ convertDiagnostics root settings errors
    _ -> pure emptyDiagnostics

censorWarnings :: Settings -> Array RebuildError -> Array RebuildError
censorWarnings settings = filter (flip notElem codes <<< getCode)
  where
  getCode (RebuildError { errorCode }) = errorCode
  codes = Config.censorCodes settings

foreign import parseShellQuote :: String -> Array String
foreign import getOsTmpDir :: Effect String
foreign import getHash :: String -> String

foreign import copyFile :: String -> String -> Effect (Promise Unit)

fullBuild ::
  Notify -> Settings -> ServerState -> Aff (Either String DiagnosticResult)
fullBuild logCb settings state = do
  let command = parseShellQuote $ Config.buildCommand settings
  case state, uncons command of
    ServerState { port: maybePort, root: Just directory }, Just { head: cmd, tail: args } -> do
      build logCb
        { command: Command cmd args
        , directory
        , useNpmDir: Config.addNpmPath settings
        }
        >>= either (pure <<< Left) \{ errors } -> do
            liftEffect $ logCb Info "Build complete"
            case maybePort of
              Nothing ->
                liftEffect
                  $ logCb Error
                  $ "Couldn't reload modules, no ide server port"

              Just port -> do
                pure unit
                attempt (loadAll port)
                  >>= case _ of
                      Left e ->
                        liftEffect $ logCb Error $ "Error reloading modules: " <> show e

                      Right (Left msg) ->
                        liftEffect
                          $ logCb Error
                          $ "Error message from IDE server reloading modules: "
                          <> msg
                      _ ->
                        liftEffect $ logCb Info "Reloaded modules"
            liftEffect $ Right <$> convertDiagnostics directory settings errors

    _, Nothing ->
      pure $ Left "Error parsing build command"
    ServerState { port, root }, _ -> do
      pure
        $ Left
        $ "Error running build: port="
        <> show port
        <> ", root="
        <> show root

-- | Builds module and provides diagnostics
rebuildWithDiagnostics ::
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Notify ->
  Boolean ->
  TextDocument ->
  Aff Unit
rebuildWithDiagnostics
  config conn state notify noCodegen document = do
  let uri = getUri document
  c <- liftEffect $ Ref.read config
  s <- liftEffect $ Ref.read state
  liftEffect $ sendDiagnosticsBegin conn
  startTime <- liftEffect $ JSDate.now <#> JSDate.toDateTime
  { pscErrors, diagnostics } <-
    if noCodegen then do
      if Config.noFsDiagnostics c then
        getNoFsDiagnostics document c s
      else do
        -- may use getMainModule from IdePurescript.Modules instead?
        mn <-
          liftEffect do
            readState state _.parsedModules
            <#> Map.lookup (getUri document)
            <#> (map $ _.parsed >>> unwrapModuleName)
            <#> join
        getDiagnosticsOnType document c s mn
    else do
      getDiagnostics uri c s
  liftEffect do
    endTime <- JSDate.now <#> JSDate.toDateTime
    notify Info
      $ "Getting diagnostics took: "
      <> ( show
            $ (DT.diff <$> endTime <*> startTime :: Maybe Milliseconds)
        )
  liftEffect do
    let fileDiagnostics = fromMaybe [] $ Map.lookup uri diagnostics
    filename <- uriToFilename uri
    allFiles <-
      traverse uriToFilename
        $ (Set.toUnfoldable $ Map.keys diagnostics :: Array _)
    notify Info
      $ "Built with "
      <> show (Array.length fileDiagnostics)
      <> "/"
      <> show (Array.length pscErrors)
      <> " issues for file: "
      <> show filename
      <> ", all diagnostic files: "
      <> show allFiles
    let nonFileDiagnostics = Map.delete uri diagnostics
    when (Map.size nonFileDiagnostics > 0) do
      notify Info $ "Unmatched diagnostics: " <> show nonFileDiagnostics
    modifyState_ state
      ( \s1 ->
          s1
            { diagnostics =
              Map.insert
                uri
                { errors: pscErrors, diagnostics: fileDiagnostics, onType: true }
                (s1.diagnostics)
            , modulesFile = Nothing -- Force reload of modules on next request
            }
      )
    publishDiagnostics conn
      { uri
      , diagnostics: fileDiagnostics
      }
    sendDiagnosticsEnd conn

parseModuleDocument ::
  Ref ServerState ->
  TextDocument ->
  Effect (CST.RecoveredParserResult Module)
parseModuleDocument state document = do
  v <- getVersion document
  text <- getText document
  let res = CST.parseModule text
  modifyState_ state
    ( \s ->
        s
          { modulesFile = Nothing
          , parsedModules =
            Map.insert (getUri document)
              { version: v, parsed: res, document } s.parsedModules
          }
    )
  pure res

{-| Updates previous diagnostic state with new results.

  We are leaving the last results of onType diagnostics for modules, because
  they are still should be actual and full build result may just not contain
  anything for a particular module.
-}
updateDiagnostics ::
  DiagnosticState -> DiagnosticState -> DiagnosticState
updateDiagnostics prevDiag newDiag =
  Map.union prevFiltered newDiag
  where
  prevFiltered =
    Map.filter (\d -> d.onType) prevDiag

{-| Performs full build.
-}
fullBuildWithDiagnostics ::
  Ref Foreign ->
  Connection ->
  Ref ServerState ->
  Notify ->
  Boolean ->
  Aff Unit
fullBuildWithDiagnostics config conn state notify withProgress = do
  let workspaceRoot = getWorkspaceRoot state
  progressReporter <-
    if withProgress then
      Just <$> createWorkDoneProgress conn
    else
      pure Nothing
  liftEffect do
    fromMaybe (pure unit)
      $ progressReporter
      <#> flip workBegin { title: "Building PureScript" }
    sendDiagnosticsBegin conn
  st <- liftEffect $ Ref.read state
  cfg <- liftEffect $ Ref.read config
  fullBuild notify cfg st
    >>= case _ of
        Right { pscErrors, diagnostics } ->
          liftEffect do
            log conn $ "Built with " <> (show $ Array.length pscErrors) <> " issues"
            pscErrorsMap <-
              collectByFirst
                <$> traverse
                    ( \err@(RebuildError { filename }) -> do
                        projectRoot <- workspaceRoot
                        uri <-
                          traverse
                            (resolve [ projectRoot ] >>> (=<<) filenameToUri) filename
                        pure $ Tuple uri err
                    )
                    pscErrors
            let
              newDiagState =
                Map.mapMaybeWithKey
                  ( \file errors ->
                      Just
                        { errors
                        , diagnostics:
                            fromMaybe [] $ Map.lookup file diagnostics
                        , onType: false
                        }
                  )
                  pscErrorsMap
            prevDiag <- readState state _.diagnostics
            let actualDiag = updateDiagnostics prevDiag newDiagState
            let
              nonErrorFiles :: Array DocumentUri
              nonErrorFiles =
                Set.toUnfoldable
                  $ Map.keys
                  --$ Map.difference prevErrors pscErrorsMap
                  $ Map.difference prevDiag actualDiag
            nonErrorFilesNames <- traverse uriToFilename nonErrorFiles
            log conn $ "Removing old diagnostics for: " <> show nonErrorFilesNames
            for_ nonErrorFiles \uri ->
              publishDiagnostics conn
                { uri
                , diagnostics: []
                }
            modifyState_ state _ { diagnostics = actualDiag }
            for_ (Map.toUnfoldable diagnostics :: Array _) \(uri /\ fileDiagnostics) -> do
              filename <- uriToFilename uri
              log conn $ "Publishing diagnostics for: " <> show filename
              publishDiagnostics conn
                { uri
                , diagnostics: fileDiagnostics
                }
            {- after full build purs-ide reloads state from the disk

            -}
            -- when (Config.noFsDiagnostics cfg) do
            --   parsedModules <- readState state _.parsedModules
            --   for_ (parsedModules) \{ document } ->
            --     addToDiagnosticsQueue state document
            {- show error dialog if there where errors during full build -}
            errorFiles <-
              traverse uriToFilename
                $ Set.toUnfoldable
                $ Map.keys
                $ Map.filter
                    ( isJust
                        <<< Array.find
                            (\(Diagnostic d) -> d.severity == notNull 1)
                    )
                    diagnostics
            case errorFiles of
              [] -> pure unit
              files ->
                showError conn
                  $ "Build failed with errors in files: "
                  <> String.joinWith ", " files
        Left err ->
          liftEffect do
            error conn err
            showError conn err
  liftEffect do
    sendDiagnosticsEnd conn
    fromMaybe (pure unit)
      $ progressReporter
      <#> workDone
