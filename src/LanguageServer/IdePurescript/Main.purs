module LanguageServer.IdePurescript.Main where

import Prelude

import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (modifyRef, newRef, readRef, writeRef)
import Control.Monad.Except (runExcept)
import Control.Promise (Promise, fromAff)
import Data.Array ((\\), length)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.JSON (parseJSON)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (over, un, unwrap)
import Data.Nullable (toMaybe, toNullable)
import Data.Profunctor.Strong (first)
import Data.StrMap (StrMap, empty, fromFoldable, insert, lookup, toUnfoldable, keys)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import IdePurescript.Modules (Module, getModulesForFileTemp, initialModulesState)
import IdePurescript.PscErrors (PscError(..))
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import LanguageServer.Console (error, info, log, warn)
import LanguageServer.DocumentStore (getDocument, onDidChangeContent, onDidSaveDocument)
import LanguageServer.Handlers (onCodeAction, onCompletion, onDefinition, onDidChangeConfiguration, onDidChangeWatchedFiles, onDocumentSymbol, onExecuteCommand, onHover, onShutdown, onWorkspaceSymbol, publishDiagnostics, sendDiagnosticsBegin, sendDiagnosticsEnd)
import LanguageServer.IdePurescript.Assist (addClause, caseSplit, fixTypo)
import LanguageServer.IdePurescript.Build (collectByFirst, fullBuild, getDiagnostics)
import LanguageServer.IdePurescript.CodeActions (getActions, onReplaceSuggestion)
import LanguageServer.IdePurescript.Commands (addClauseCmd, addCompletionImportCmd, addModuleImportCmd, buildCmd, caseSplitCmd, cmdName, commands, fixTypoCmd, getAvailableModulesCmd, replaceSuggestionCmd, restartPscIdeCmd, searchCmd, startPscIdeCmd, stopPscIdeCmd)
import LanguageServer.IdePurescript.Completion (getCompletions)
import LanguageServer.IdePurescript.Config (fastRebuild)
import LanguageServer.IdePurescript.Imports (addCompletionImport, addModuleImport', getAllModules)
import LanguageServer.IdePurescript.Search (search)
import LanguageServer.IdePurescript.Server (retry, startServer')
import LanguageServer.IdePurescript.Symbols (getDefinition, getDocumentSymbols, getWorkspaceSymbols)
import LanguageServer.IdePurescript.Tooltips (getTooltips)
import LanguageServer.IdePurescript.Types (ServerState(..), MainEff, CommandHandler)
import LanguageServer.Setup (InitParams(..), initConnection, initDocumentStore)
import LanguageServer.TextDocument (getText, getUri)
import LanguageServer.Types (Diagnostic, DocumentUri(..), FileChangeType(..), FileChangeTypeCode(..), FileEvent(..), Settings, TextDocumentIdentifier(..), intToFileChangeType)
import LanguageServer.Uri (filenameToUri, uriToFilename)
import Node.Process (argv, cwd)
import PscIde (load)

defaultServerState :: forall eff. ServerState eff
defaultServerState = ServerState
  { port: Nothing
  , deactivate: pure unit
  , root: Nothing
  , conn: Nothing
  , modules: initialModulesState
  , modulesFile: Nothing
  , diagnostics: empty
  }

main :: forall eff. Eff (MainEff eff) Unit
main = do
  state <- newRef defaultServerState
  config <- newRef (toForeign {})
  gotConfig <- newRef false

  argv >>= case _ of 
    [ _, _, "--stdio", "--config", c ] -> either (const $ pure unit) (\cc -> do
        writeRef config cc
        writeRef gotConfig true
    ) $ runExcept $ parseJSON c
    _ -> pure unit

  let logError :: Notify (MainEff eff)
      logError l s = do
        (_.conn <$> unwrap <$> readRef state) >>=
          maybe (pure unit) (\conn -> case l of 
            Success -> log conn s
            Info -> info conn s
            Warning -> warn conn s
            Error -> error conn s)
  let launchAffLog = void <<< runAff (logError Error <<< show) (const $ pure unit)

  let stopPscIdeServer :: Aff (MainEff eff) Unit
      stopPscIdeServer = do
        quit <- liftEff (_.deactivate <$> unwrap <$> readRef state)
        quit
        liftEff $ modifyRef state (over ServerState $ _ { port = Nothing, deactivate = pure unit })
        liftEff $ logError Success "Stopped IDE server"

      startPscIdeServer = do
        liftEff $ logError Info "Starting IDE server"
        rootPath <- liftEff $ (_.root <<< unwrap) <$> readRef state
        settings <- liftEff $ readRef config
        startRes <- startServer' settings rootPath logError logError
        retry logError 6 case startRes of
          { port: Just port, quit } -> do
            _ <- load port [] []
            liftEff $ modifyRef state (over ServerState $ _ { port = Just port, deactivate = quit })
            liftEff $ logError Success "Started IDE server"
          _ -> pure unit

      restartPscIdeServer = do
        stopPscIdeServer
        startPscIdeServer

  conn <- initConnection commands $ \({ params: InitParams { rootPath }, conn }) ->  do
    cwd >>= \dir -> log conn ("Starting with cwd: " <> dir)
    argv >>= \args -> log conn $ "Starting with args: " <> show args
    modifyRef state (over ServerState $ _ { root = toMaybe rootPath })
  modifyRef state (over ServerState $ _ { conn = Just conn })

  let onConfig = do
        writeRef gotConfig true
        launchAffLog startPscIdeServer

  readRef gotConfig >>= (_ `when` onConfig)

  onDidChangeConfiguration conn $ \{settings} -> do 
    log conn "Got updated settings"
    writeRef config settings
    readRef gotConfig >>= \c -> when (not c) onConfig


  log conn "PureScript Language Server started"
  
  documents <- initDocumentStore conn

  let showModule :: Module -> String
      showModule = unwrap >>> case _ of
         { moduleName, importType, qualifier } -> moduleName <> maybe "" (" as " <> _) qualifier

  let updateModules :: DocumentUri -> Aff (MainEff eff) Unit
      updateModules uri = 
        liftEff (readRef state) >>= case _ of 
          ServerState { port: Just port, modulesFile } 
            | modulesFile /= Just uri -> do
            text <- liftEff $ getDocument documents uri >>= getText
            path <- liftEff $ uriToFilename uri
            modules <- getModulesForFileTemp port path text
            liftEff $ modifyRef state $ over ServerState (_ { modules = modules, modulesFile = Just uri })
            -- liftEff $ info conn $ "Updated modules to: " <> show modules.main <> " / " <> show (showModule <$> modules.modules)
          _ -> pure unit

  let runHandler :: forall a b . String -> (b -> Maybe DocumentUri) -> (Settings -> ServerState (MainEff eff) -> b -> Aff (MainEff eff) a) -> b -> Eff (MainEff eff) (Promise a)
      runHandler handlerName docUri f b =
        fromAff do
          c <- liftEff $ readRef config
          s <- liftEff $ readRef state
          liftEff $ maybe (pure unit) (\con -> log con $ "handler " <> handlerName) (_.conn $ unwrap s)
          maybe (pure unit) updateModules (docUri b)          
          f c s b

  let getTextDocUri :: forall r. { textDocument :: TextDocumentIdentifier | r } -> Maybe DocumentUri
      getTextDocUri = (Just <<< _.uri <<< un TextDocumentIdentifier <<< _.textDocument)

  onCompletion conn $ runHandler "onCompletion" getTextDocUri (getCompletions documents)
  onDefinition conn $ runHandler "onDefinition" getTextDocUri (getDefinition documents)
  onDocumentSymbol conn $ runHandler "onDocumentSymbol" getTextDocUri getDocumentSymbols
  onWorkspaceSymbol conn $ runHandler "onWorkspaceSymbol" (const Nothing) getWorkspaceSymbols
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
    liftEff $ modifyRef state $ over ServerState (_ { modulesFile = Nothing })

  onDidSaveDocument documents \{ document } -> launchAffLog do
    let uri = getUri document
    c <- liftEff $ readRef config
    s <- liftEff $ readRef state

    when (fastRebuild c) do 
      liftEff $ sendDiagnosticsBegin conn
      { pscErrors, diagnostics } <- getDiagnostics uri c s
      filename <- liftEff $ uriToFilename uri
      let fileDiagnostics = fromMaybe [] $ lookup filename diagnostics
      liftEff $ log conn $ "Built with " <> show (length pscErrors) <> " issues for file: " <> show filename <> ", all diagnostic files: " <> show (keys diagnostics)
      liftEff $ writeRef state $ over ServerState (\s1 -> s1 { 
        diagnostics = insert (un DocumentUri uri) pscErrors (s1.diagnostics)
      , modulesFile = Nothing -- Force reload of modules on next request
      }) s
      liftEff $ publishDiagnostics conn { uri, diagnostics: fileDiagnostics }
      liftEff $ sendDiagnosticsEnd conn

  let onBuild docs c s arguments = do
        liftEff $ sendDiagnosticsBegin conn
        { pscErrors, diagnostics } <- fullBuild logError docs c s arguments
        liftEff do
          log conn $ "Built with " <> (show $ length pscErrors) <> " issues"
          pscErrorsMap <- collectByFirst <$> traverse (\(e@PscError { filename }) -> do
            uri <- maybe (pure Nothing) (\f -> Just <$> un DocumentUri <$> filenameToUri f) filename
            pure $ Tuple uri e)
              pscErrors
          prevErrors <- _.diagnostics <$> un ServerState <$> readRef state
          let nonErrorFiles :: Array String
              nonErrorFiles = keys prevErrors \\ keys pscErrorsMap
          writeRef state $ over ServerState (_ { diagnostics = pscErrorsMap }) s
          for_ (toUnfoldable diagnostics :: Array (Tuple String (Array Diagnostic))) \(Tuple filename fileDiagnostics) -> do
            uri <- filenameToUri filename
            publishDiagnostics conn { uri, diagnostics: fileDiagnostics }
          for_ (map DocumentUri nonErrorFiles) \uri -> publishDiagnostics conn { uri, diagnostics: [] }
          sendDiagnosticsEnd conn

  let noResult = toForeign $ toNullable Nothing
  let voidHandler :: forall a. CommandHandler eff a -> CommandHandler eff Foreign
      voidHandler h d c s a = h d c s a $> noResult
      simpleHandler h d c s a = h $> noResult
  let handlers :: StrMap (CommandHandler eff Foreign)
      handlers = fromFoldable $ first cmdName <$>
      [ Tuple caseSplitCmd $ voidHandler caseSplit
      , Tuple addClauseCmd $ voidHandler addClause
      , Tuple replaceSuggestionCmd $ voidHandler onReplaceSuggestion
      , Tuple buildCmd $ voidHandler onBuild
      , Tuple addCompletionImportCmd $ addCompletionImport logError
      , Tuple addModuleImportCmd $ voidHandler $ addModuleImport' logError
      , Tuple startPscIdeCmd $ simpleHandler startPscIdeServer
      , Tuple stopPscIdeCmd $ simpleHandler stopPscIdeServer
      , Tuple restartPscIdeCmd $ simpleHandler restartPscIdeServer
      , Tuple getAvailableModulesCmd $ getAllModules logError
      , Tuple searchCmd $ search
      , Tuple fixTypoCmd $ fixTypo
      ]

  onExecuteCommand conn $ \{ command, arguments } -> fromAff do
    c <- liftEff $ readRef config
    s <- liftEff $ readRef state
    case lookup command handlers of 
      Just handler -> handler documents c s arguments
      Nothing -> do
        liftEff $ error conn $ "Unknown command: " <> command
        pure noResult
