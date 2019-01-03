module LanguageServer.IdePurescript.Main where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Promise (Promise, fromAff)
import Data.Array (length, (!!), (\\))
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (over, un, unwrap)
import Data.Nullable (toMaybe, toNullable)
import Data.Profunctor.Strong (first)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), apathize, delay, runAff_, try)
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
import LanguageServer.Handlers (onCodeAction, onCompletion, onDefinition, onDidChangeConfiguration, onDidChangeWatchedFiles, onDocumentSymbol, onExecuteCommand, onHover, onReferences, onShutdown, onWorkspaceSymbol, publishDiagnostics, sendDiagnosticsBegin, sendDiagnosticsEnd)
import LanguageServer.IdePurescript.Assist (addClause, caseSplit, fillTypedHole, fixTypo)
import LanguageServer.IdePurescript.Build (collectByFirst, fullBuild, getDiagnostics)
import LanguageServer.IdePurescript.CodeActions (getActions, onReplaceAllSuggestions, onReplaceSuggestion)
import LanguageServer.IdePurescript.Commands (addClauseCmd, addCompletionImportCmd, addModuleImportCmd, buildCmd, caseSplitCmd, cmdName, commands, fixTypoCmd, getAvailableModulesCmd, replaceAllSuggestionsCmd, replaceSuggestionCmd, restartPscIdeCmd, searchCmd, startPscIdeCmd, stopPscIdeCmd, typedHoleExplicitCmd)
import LanguageServer.IdePurescript.Completion (getCompletions)
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.Imports (addCompletionImport, addModuleImport', getAllModules)
import LanguageServer.IdePurescript.References (getReferences)
import LanguageServer.IdePurescript.Search (search)
import LanguageServer.IdePurescript.Server (loadAll, retry, startServer')
import LanguageServer.IdePurescript.Symbols (getDefinition, getDocumentSymbols, getWorkspaceSymbols)
import LanguageServer.IdePurescript.Tooltips (getTooltips)
import LanguageServer.IdePurescript.Types (ServerState(..), CommandHandler)
import LanguageServer.Setup (InitParams(..), initConnection, initDocumentStore)
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
  }

parseArgs :: Array String -> Maybe
  { config :: Maybe String
  , filename :: Maybe String
  }
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
  state <- Ref.new defaultServerState
  config <- Ref.new (unsafeToForeign {})
  logFile <- Ref.new (Nothing :: Maybe String)
  gotConfig <- Ref.new false

  maybeArgs <- parseArgs <$> argv
  case maybeArgs of
    Nothing -> do
      Console.error "Error parsing args"
      exit 1
    Just args -> do
      Ref.write args.filename logFile
      maybe (pure unit) (flip (FSSync.writeTextFile Encoding.UTF8) "Starting logging...\n") args.filename
      case args.config of
        Just c -> either (const $ pure unit) (\cc -> do
            Ref.write cc config
            Ref.write true gotConfig
        ) $ runExcept $ parseJSON c
        Nothing -> pure unit

  let logError :: Notify
      logError l s = do
        (_.conn <$> unwrap <$> Ref.read state) >>=
          maybe (pure unit) (flip case l of 
              Success -> log
              Info -> info
              Warning -> warn
              Error -> error
            s)
        liftEffect (Ref.read logFile) >>= case _ of
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
        rootPath <- liftEffect $ (_.root <<< unwrap) <$> Ref.read state
        settings <- liftEffect $ Ref.read config
        startRes <- startServer' settings rootPath logError logError
        retry logError 6 case startRes of
          { port: Just port, quit } -> do
            loadAll port
            liftEffect $ Ref.modify_ (over ServerState $ _ { port = Just port, deactivate = quit }) state
          _ -> pure unit

      restartPscIdeServer = do
        apathize stopPscIdeServer
        startPscIdeServer

  conn <- initConnection commands $ \({ params: InitParams { rootPath, rootUri }, conn }) ->  do
    argv >>= \args -> log conn $ "Starting with args: " <> show args
    root <- case toMaybe rootUri, toMaybe rootPath of
      Just uri, _ -> Just <$> uriToFilename uri
      _, Just path -> pure $ Just path
      Nothing, Nothing -> pure Nothing 
    (\(Tuple dir root) -> log conn ("Starting with cwd: " <> dir <> " and using root path: " <> root)) =<< Tuple <$> cwd <*> workspaceRoot
    Ref.modify_ (over ServerState $ _ { root = root }) state
  Ref.modify_ (over ServerState $ _ { conn = Just conn }) state
  
  documents <- initDocumentStore conn

  let showModule :: Module -> String
      showModule = unwrap >>> case _ of
         { moduleName, importType, qualifier } -> moduleName <> maybe "" (" as " <> _) qualifier

  let updateModules :: DocumentUri -> Aff Unit
      updateModules uri = 
        liftEffect (Ref.read state) >>= case _ of 
          ServerState { port: Just port, modulesFile } 
            | modulesFile /= Just uri -> do
            text <- liftEffect $ getDocument documents uri >>= getText
            path <- liftEffect $ uriToFilename uri
            modules <- getModulesForFileTemp port path text
            liftEffect $ Ref.modify_ (over ServerState (_ { modules = modules, modulesFile = Just uri })) state
          _ -> pure unit

  let runHandler :: forall a b . String -> (b -> Maybe DocumentUri) -> (Settings -> ServerState -> b -> Aff a) -> b -> Effect (Promise a)
      runHandler handlerName docUri f b =
        fromAff do
          c <- liftEffect $ Ref.read config
          s <- liftEffect $ Ref.read state
          maybe (pure unit) updateModules (docUri b)          
          f c s b

  let getTextDocUri :: forall r. { textDocument :: TextDocumentIdentifier | r } -> Maybe DocumentUri
      getTextDocUri = (Just <<< _.uri <<< un TextDocumentIdentifier <<< _.textDocument)

  onCompletion conn $ runHandler "onCompletion" getTextDocUri (getCompletions documents)
  onDefinition conn $ runHandler "onDefinition" getTextDocUri (getDefinition documents)
  onDocumentSymbol conn $ runHandler "onDocumentSymbol" getTextDocUri getDocumentSymbols
  onWorkspaceSymbol conn $ runHandler "onWorkspaceSymbol" (const Nothing) getWorkspaceSymbols

  onReferences conn $ runHandler "onReferences" (const Nothing) (getReferences documents)
  onHover conn $ runHandler "onHover" getTextDocUri (getTooltips documents)
  onCodeAction conn $ runHandler "onCodeAction" getTextDocUri (getActions documents)
  onShutdown conn $ fromAff stopPscIdeServer

  onDidChangeWatchedFiles conn $ \{ changes } -> do
    for_ changes \(FileEvent { uri, "type": FileChangeTypeCode n }) -> do
      case intToFileChangeType n of
        Just CreatedChangeType -> log conn $ "Created " <> un DocumentUri uri <> " - full build may be required"
        Just DeletedChangeType -> log conn $ "Deleted " <> un DocumentUri uri <> " - full build may be required"
        _ -> pure unit

  onDidChangeContent documents $ \_ ->
    liftEffect $ Ref.modify_ (over ServerState (_ { modulesFile = Nothing })) state

  onDidSaveDocument documents \{ document } -> launchAffLog do
    let uri = getUri document
    c <- liftEffect $ Ref.read config
    s <- liftEffect $ Ref.read state

    when (Config.fastRebuild c) do 
      liftEffect $ sendDiagnosticsBegin conn
      { pscErrors, diagnostics } <- getDiagnostics uri c s
      filename <- liftEffect $ uriToFilename uri
      let fileDiagnostics = fromMaybe [] $ Object.lookup filename diagnostics
      liftEffect do
        log conn $ "Built with " <> show (length pscErrors) <> " issues for file: " <> show filename <> ", all diagnostic files: " <> show (Object.keys diagnostics)
        Ref.write (over ServerState (\s1 -> s1 { 
          diagnostics = Object.insert (un DocumentUri uri) pscErrors (s1.diagnostics)
        , modulesFile = Nothing -- Force reload of modules on next request
        }) s) state
        publishDiagnostics conn { uri, diagnostics: fileDiagnostics }
        sendDiagnosticsEnd conn

  let onBuild docs c s arguments = do
        liftEffect $ sendDiagnosticsBegin conn
        fullBuild logError docs c s arguments >>= case _ of
          Right { pscErrors, diagnostics } ->
            liftEffect do
              log conn $ "Built with " <> (show $ length pscErrors) <> " issues"
              pscErrorsMap <- collectByFirst <$> traverse (\(e@RebuildError { filename }) -> do
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
      , Tuple startPscIdeCmd $ simpleHandler startPscIdeServer
      , Tuple stopPscIdeCmd $ simpleHandler stopPscIdeServer
      , Tuple restartPscIdeCmd $ simpleHandler restartPscIdeServer
      , Tuple getAvailableModulesCmd $ getAllModules logError
      , Tuple searchCmd $ search
      , Tuple fixTypoCmd $ fixTypo logError
      , Tuple typedHoleExplicitCmd $ voidHandler $ fillTypedHole logError
      ]

  onExecuteCommand conn $ \{ command, arguments } -> fromAff do
    c <- liftEffect $ Ref.read config
    s <- liftEffect $ Ref.read state
    case Object.lookup command handlers of 
      Just handler -> handler documents c s arguments
      Nothing -> do
        liftEffect $ error conn $ "Unknown command: " <> command
        pure noResult

  let onConfig = launchAffLog do
        liftEffect $ Ref.write true gotConfig
        c <- liftEffect $ Ref.read config
        when (Config.autoStartPscIde c) $ do
          startPscIdeServer
          let outputDir = Config.effectiveOutputDirectory c
          hasPackageFile <- (||) <$> (FS.exists "bower.json") <*> (FS.exists "psc-package.json")
          when (not hasPackageFile) do
            liftEffect $ showError conn "It doesn't look like the workspace root is a PureScript project (has bower.json/psc-package.json). The PureScript project should be opened as a root workspace folder."
          exists <- FS.exists outputDir
          when (not exists) $ liftEffect $ launchAffLog do
            let message = "Output directory does not exist at '" <> outputDir <> "'"
            liftEffect $ info conn message
            let buildOption = "Build project"
            action <- showWarningWithActions conn (message <> ". Ensure project is built, or check configuration of output directory and build command.") [ buildOption ]
            when (action == Just buildOption) do
              s <- liftEffect $ Ref.read state
              onBuild documents c s []

  Ref.read gotConfig >>= (_ `when` onConfig)

  onDidChangeConfiguration conn $ \{settings} -> do 
    log conn "Got updated settings"
    Ref.write settings config 
    Ref.read gotConfig >>= \c -> when (not c) onConfig

  launchAffLog $ do delay (Milliseconds 250.0)
                    got <- liftEffect $ Ref.read gotConfig
                    liftEffect $ when (not got) $ do
                      logError Warning "Proceeding with no config receieved"
                      onConfig

  log conn "PureScript Language Server started"