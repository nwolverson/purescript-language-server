module LanguageServer.IdePurescript.Main where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Promise (Promise, fromAff)
import Data.Array (length, (!!), (\\))
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (for_, or)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe, maybe')
import Data.Newtype (over, un, unwrap)
import Data.Nullable (toMaybe, toNullable)
import Data.Profunctor.Strong (first)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), apathize, attempt, delay, forkAff, launchAff_, runAff_, try)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Foreign (Foreign, unsafeToForeign)
import Foreign.JSON (parseJSON)
import Foreign.Object (Object)
import Foreign.Object as Object
import IdePurescript.Modules (Module, getModulesForFileTemp, initialModulesState)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import LanguageServer.Console (error, info, log, warn)
import LanguageServer.DocumentStore (getDocument, onDidChangeContent, onDidSaveDocument)
import LanguageServer.Handlers (onCodeAction, onCompletion, onDefinition, onDidChangeConfiguration, onDidChangeWatchedFiles, onDocumentSymbol, onExecuteCommand, onFoldingRanges, onDocumentFormatting, onHover, onReferences, onShutdown, onWorkspaceSymbol, publishDiagnostics, sendDiagnosticsBegin, sendDiagnosticsEnd)
import LanguageServer.IdePurescript.Assist (addClause, caseSplit, fillTypedHole, fixTypo)
import LanguageServer.IdePurescript.Build (collectByFirst, fullBuild, getDiagnostics)
import LanguageServer.IdePurescript.CodeActions (getActions, onReplaceAllSuggestions, onReplaceSuggestion)
import LanguageServer.IdePurescript.Commands (addClauseCmd, addCompletionImportCmd, addModuleImportCmd, buildCmd, caseSplitCmd, cmdName, commands, fixTypoCmd, getAvailableModulesCmd, organiseImportsCmd, replaceAllSuggestionsCmd, replaceSuggestionCmd, restartPscIdeCmd, searchCmd, startPscIdeCmd, stopPscIdeCmd, typedHoleExplicitCmd)
import LanguageServer.IdePurescript.Completion (getCompletions)
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.FoldingRanges (getFoldingRanges)
import LanguageServer.IdePurescript.Formatting (getFormattedDocument)
import LanguageServer.IdePurescript.Imports (addCompletionImport, addModuleImport', getAllModules, organiseImports, organiseImportsDiagnostic)
import LanguageServer.IdePurescript.References (getReferences)
import LanguageServer.IdePurescript.Search (search)
import LanguageServer.IdePurescript.Server (getEnvPursIdeSources, loadAll, retry, startServer')
import LanguageServer.IdePurescript.Symbols (getDefinition, getDocumentSymbols, getWorkspaceSymbols)
import LanguageServer.IdePurescript.Tooltips (getTooltips)
import LanguageServer.IdePurescript.Types (ServerState(..), CommandHandler)
import LanguageServer.Setup (InitParams(..), getConfiguration, initConnection, initDocumentStore)
import LanguageServer.TextDocument (getText, getUri)
import LanguageServer.Types (Diagnostic, DocumentUri(..), FileChangeType(..), FileChangeTypeCode(..), FileEvent(..), Settings, TextDocumentIdentifier(..), intToFileChangeType)
import LanguageServer.Uri (filenameToUri, uriToFilename)
import LanguageServer.Window (showError, showWarningWithActions)
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Node.FS.Sync as FSSync
import Node.Path (resolve)
import Node.Process (argv, cwd, exit)
import PscIde.Command (RebuildError(..))

defaultServerState :: ServerState
defaultServerState = ServerState
  { port: Nothing
  , deactivate: pure unit
  , root: Nothing
  , conn: Nothing
  , modules: initialModulesState
  , modulesFile: Nothing
  , diagnostics: Object.empty
  , clientCapabilities: Nothing
  }

type CmdLineArguments =
  { config :: Maybe String
  , filename :: Maybe String
  }

parseArgs :: Array String -> Maybe CmdLineArguments
parseArgs allArgs = go 0 defaultArgs
  where
  args = Array.drop 2 allArgs

  defaultArgs = { config: Nothing, filename: Nothing }

  go i c =
    case args !! i of
      Just "--config" -> case args !! (i+1) of
        Just conf -> go (i+2) (c { config = Just conf })
        Nothing -> Nothing
      Just "--log" -> case args !! (i+1) of
        Just filename -> go (i+2) (c { filename = Just filename })
        Nothing -> Nothing
      -- stdio etc
      Just _ -> go (i+1) c
      Nothing -> Just c

main :: Effect Unit
main = do
  maybeArgs <- parseArgs <$> argv
  args' <- case maybeArgs of
    Nothing -> do
      Console.error "Error parsing args"
      exit 1
    Just args -> do
      maybe (pure unit) (flip (FSSync.writeTextFile Encoding.UTF8) "Starting logging...\n") args.filename
      let config' = case args.config of
                      Just c -> either (const Nothing) Just $ runExcept $ parseJSON c
                      Nothing -> Nothing
      pure (args { config = config' } )
  launchAff_ $ main' args'

main' ::
  { config :: Maybe Foreign
  , filename :: Maybe String
  } -> Aff Unit
main' { filename: logFile, config: cmdLineConfig } = do
  gotConfig :: AVar Unit <- AVar.empty
  state <- liftEffect $ Ref.new defaultServerState
  config <- liftEffect $ Ref.new (unsafeToForeign {})

  let logError :: Notify
      logError l s = do
        (_.conn <$> unwrap <$> Ref.read state) >>=
          maybe (pure unit) (flip case l of 
              Success -> log
              Info -> info
              Warning -> warn
              Error -> error
            s)
        case logFile of
          Just filename -> FSSync.appendTextFile Encoding.UTF8 filename ("[" <> show l <> "] " <> s <> "\n")
          Nothing -> pure unit
  let launchAffLog = runAff_ (either (logError Error <<< show) (const $ pure unit))
      workspaceRoot = do 
        root <- (_.root <<< unwrap) <$> Ref.read state
        maybe cwd pure root
      resolvePath p = workspaceRoot >>= \root -> resolve [ root ] p

  let stopPscIdeServer :: Aff Unit
      stopPscIdeServer = do
        quit <- liftEffect (_.deactivate <$> unwrap <$> Ref.read state)
        quit
        liftEffect do
          Ref.modify_ (over ServerState $ _ { port = Nothing, deactivate = pure unit }) state
          logError Success "Stopped IDE server"

      startPscIdeServer = do
        liftEffect $ logError Info "Starting IDE server"
        rootPath <- liftEffect workspaceRoot
        settings <- liftEffect $ Ref.read config
        startRes <- startServer' settings rootPath logError logError
        retry logError 6 case startRes of
          { port: Just port, quit } -> do
            loadAll port >>= case _ of
              Left msg -> liftEffect $ logError Info $ "Non-fatal error loading modules: " <> msg
              _ -> pure unit
            liftEffect $ Ref.modify_ (over ServerState $ _ { port = Just port, deactivate = quit }) state
          _ -> pure unit

      restartPscIdeServer = do
        apathize stopPscIdeServer
        startPscIdeServer

  conn <- liftEffect $ initConnection commands $ \({ params: InitParams { rootPath, rootUri, capabilities }, conn }) ->  do
    argv >>= \args -> log conn $ "Starting with args: " <> show args
    root <- case toMaybe rootUri, toMaybe rootPath of
      Just uri, _ -> Just <$> uriToFilename uri
      _, Just path -> pure $ Just path
      Nothing, Nothing -> pure Nothing
    workingRoot <- maybe cwd pure root
    Ref.modify_ (over ServerState $ _ { root = Just workingRoot, clientCapabilities = Just capabilities }) state
    (\(Tuple dir root') -> log conn ("Starting with cwd: " <> dir <> " and using root path: " <> root')) =<< Tuple <$> cwd <*> pure workingRoot
  liftEffect $ Ref.modify_ (over ServerState $ _ { conn = Just conn }) state
  
  documents <- liftEffect $ initDocumentStore conn

  let showModule :: Module -> String
      showModule = unwrap >>> case _ of
         { moduleName, importType, qualifier } -> moduleName <> maybe "" (" as " <> _) qualifier

  let updateModules :: DocumentUri -> Aff (Maybe ServerState)
      updateModules uri = 
        liftEffect (Ref.read state) >>= case _ of 
          ServerState { port: Just port, modulesFile }
            | modulesFile /= Just uri -> do
            text <- liftEffect $ getDocument documents uri >>= getText
            path <- liftEffect $ uriToFilename uri
            modules <- getModulesForFileTemp port path text
            s <- liftEffect $ Ref.modify (over ServerState (_ { modules = modules, modulesFile = Just uri })) state
            pure $ Just s
          _ -> pure Nothing

  let runHandler :: forall a b . String -> (b -> Maybe DocumentUri) -> (Settings -> ServerState -> b -> Aff a) -> b -> Effect (Promise a)
      runHandler handlerName docUri f b =
        fromAff do
          c <- liftEffect $ Ref.read config
          ms <- maybe (pure Nothing) updateModules (docUri b)
          s <- maybe' (\_ -> liftEffect $ Ref.read state) pure ms
          f c s b

  let getTextDocUri :: forall r. { textDocument :: TextDocumentIdentifier | r } -> Maybe DocumentUri
      getTextDocUri = (Just <<< _.uri <<< un TextDocumentIdentifier <<< _.textDocument)

  liftEffect do
    onCompletion conn $ runHandler "onCompletion" getTextDocUri (getCompletions documents)
    onDefinition conn $ runHandler "onDefinition" getTextDocUri (getDefinition documents)
    onDocumentSymbol conn $ runHandler "onDocumentSymbol" getTextDocUri getDocumentSymbols
    onWorkspaceSymbol conn $ runHandler "onWorkspaceSymbol" (const Nothing) getWorkspaceSymbols

    onFoldingRanges conn $ runHandler "onFoldingRanges" getTextDocUri (getFoldingRanges documents)
    onDocumentFormatting conn $ runHandler "onDocumentFormatting" getTextDocUri (getFormattedDocument logError documents)
    onReferences conn $ runHandler "onReferences" getTextDocUri (getReferences documents)
    onHover conn $ runHandler "onHover" getTextDocUri (getTooltips documents)
    onCodeAction conn $ runHandler "onCodeAction" getTextDocUri (getActions documents)
    onShutdown conn $ fromAff stopPscIdeServer

  liftEffect $ onDidChangeWatchedFiles conn $ \{ changes } -> do
    for_ changes \(FileEvent { uri, "type": FileChangeTypeCode n }) -> do
      case intToFileChangeType n of
        Just CreatedChangeType -> log conn $ "Created " <> un DocumentUri uri <> " - full build may be required"
        Just DeletedChangeType -> log conn $ "Deleted " <> un DocumentUri uri <> " - full build may be required"
        _ -> pure unit

  liftEffect $ onDidChangeContent documents $ \_ ->
    Ref.modify_ (over ServerState (_ { modulesFile = Nothing })) state

  liftEffect $ onDidSaveDocument documents \{ document } -> launchAffLog do
    let uri = getUri document
    c <- liftEffect $ Ref.read config
    s <- liftEffect $ Ref.read state
    organiseDiagnostics <- organiseImportsDiagnostic s logError document
    when (Config.fastRebuild c) do 
      liftEffect $ sendDiagnosticsBegin conn
      { pscErrors, diagnostics } <- getDiagnostics uri c s
      filename <- liftEffect $ uriToFilename uri
      let fileDiagnostics = fromMaybe [] $ Object.lookup filename diagnostics
      liftEffect do
        log conn $ "Built with " <> show (length fileDiagnostics) <> "/" <> show (length pscErrors) <> " issues for file: " <> show filename <> ", all diagnostic files: " <> show (Object.keys diagnostics)
        let nonFileDiagnostics = Object.delete filename diagnostics
        when (Object.size nonFileDiagnostics > 0) do
          log conn $ "Unmatched diagnostics: " <> show nonFileDiagnostics
        Ref.write (over ServerState (\s1 -> s1 { 
          diagnostics = Object.insert (un DocumentUri uri) pscErrors (s1.diagnostics)
        , modulesFile = Nothing -- Force reload of modules on next request
        }) s) state
        publishDiagnostics conn { uri, diagnostics: fileDiagnostics <> organiseDiagnostics }
        sendDiagnosticsEnd conn

  let onBuild docs c s arguments = do
        liftEffect $ sendDiagnosticsBegin conn
        fullBuild logError docs c s arguments >>= case _ of
          Right { pscErrors, diagnostics } ->
            liftEffect do
              log conn $ "Built with " <> (show $ length pscErrors) <> " issues"
              pscErrorsMap <- collectByFirst <$> traverse (\e@(RebuildError { filename }) -> do
                projectRoot <- workspaceRoot
                filename' <- traverse (resolve [ projectRoot ]) filename
                uri <- maybe (pure Nothing) (\f -> Just <$> un DocumentUri <$> filenameToUri f) filename'
                pure $ Tuple uri e)
                  pscErrors
              prevErrors <- _.diagnostics <$> un ServerState <$> Ref.read state
              let nonErrorFiles :: Array String
                  nonErrorFiles = Object.keys prevErrors \\ Object.keys pscErrorsMap
              log conn $ "Removing old diagnostics for: " <> show nonErrorFiles
              for_ (map DocumentUri nonErrorFiles) \uri -> publishDiagnostics conn { uri, diagnostics: [] }

              Ref.write (over ServerState (_ { diagnostics = pscErrorsMap }) s) state
              for_ (Object.toUnfoldable diagnostics :: Array (Tuple String (Array Diagnostic))) \(Tuple filename fileDiagnostics) -> do
                uri <- filenameToUri filename
                log conn $ "Publishing diagnostics for: " <> show uri <> " (" <> show filename <> ")"
                publishDiagnostics conn { uri, diagnostics: fileDiagnostics }
          Left err ->
            liftEffect do error conn err
                          showError conn err
        liftEffect $ sendDiagnosticsEnd conn


  let noResult = unsafeToForeign $ toNullable Nothing
  let voidHandler :: forall a. CommandHandler a -> CommandHandler Foreign
      voidHandler h d c s a =
        try (h d c s a) >>= case _ of
          Left err -> do
            liftEffect $ logError Error $ show err
            pure noResult
          Right _ -> pure noResult
      simpleHandler h d c s a = h $> noResult
  let handlers :: Object (CommandHandler Foreign)
      handlers = Object.fromFoldable $ first cmdName <$>
        [ Tuple caseSplitCmd $ voidHandler caseSplit
        , Tuple addClauseCmd $ voidHandler addClause
        , Tuple replaceSuggestionCmd $ voidHandler onReplaceSuggestion
        , Tuple replaceAllSuggestionsCmd $ voidHandler onReplaceAllSuggestions
        , Tuple buildCmd $ voidHandler onBuild
        , Tuple addCompletionImportCmd $ addCompletionImport logError
        , Tuple addModuleImportCmd $ voidHandler $ addModuleImport' logError
        , Tuple organiseImportsCmd $ organiseImports logError
        , Tuple startPscIdeCmd $ simpleHandler startPscIdeServer
        , Tuple stopPscIdeCmd $ simpleHandler stopPscIdeServer
        , Tuple restartPscIdeCmd $ simpleHandler restartPscIdeServer
        , Tuple getAvailableModulesCmd $ getAllModules logError
        , Tuple searchCmd $ search
        , Tuple fixTypoCmd $ fixTypo logError
        , Tuple typedHoleExplicitCmd $ voidHandler $ fillTypedHole logError
        ]

  liftEffect $ onExecuteCommand conn $ \{ command, arguments } -> fromAff do
    c <- liftEffect $ Ref.read config
    s <- liftEffect $ Ref.read state
    case Object.lookup command handlers of 
      Just handler -> handler documents c s arguments
      Nothing -> do
        liftEffect $ error conn $ "Unknown command: " <> command
        pure noResult

  let setConfig :: String -> Foreign -> Aff Unit
      setConfig source newConfig = do
        liftEffect do
          log conn $ "Got new config (" <> source <> ")"
          Ref.write newConfig config
        AVar.tryPut unit gotConfig >>= case _ of
          true -> pure unit
          false -> liftEffect $ log conn "Not starting server, already started"

  liftEffect $ onDidChangeConfiguration conn $ \{settings} ->
    launchAffLog $ setConfig "client push" settings

  -- When config is received, start the server
  _ <- forkAff do
    -- Ensure we only run once
    AVar.read gotConfig
    c <- liftEffect $ Ref.read config
    when (Config.autoStartPscIde c) $ do
      startPscIdeServer
      outputDir <- liftEffect $ resolvePath $ Config.effectiveOutputDirectory c
      hasPackageFile <- or <$> traverse (FS.exists <=< liftEffect  <<< resolvePath) ["bower.json", "psc-package.json", "spago.dhall"]
      envIdeSources <- getEnvPursIdeSources
      when (not hasPackageFile && isNothing envIdeSources) do
        liftEffect $ showError conn "It doesn't look like the workspace root is a PureScript project (has bower.json/psc-package.json/spago.dhall). The PureScript project should be opened as a root workspace folder."
      exists <- FS.exists outputDir
      unless exists $ liftEffect $ launchAffLog do
        let message = "Output directory does not exist at '" <> outputDir <> "'"
        liftEffect $ info conn message
        let buildOption = "Build project"
        action <- showWarningWithActions conn (message <> ". Ensure project is built, or check configuration of output directory and build command.") [ buildOption ]
        when (action == Just buildOption) do
          s <- liftEffect $ Ref.read state
          onBuild documents c s []


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
          liftEffect $ logError Warning "Proceeding with no config receieved"
          void $ AVar.tryPut unit gotConfig

  liftEffect $ log conn "PureScript Language Server started"
