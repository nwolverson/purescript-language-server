module LanguageServer.IdePurescript.WatchedFiles
  ( handleDidChangeWatchedFiles
  )
  where

import Prelude

import Data.Array ((!!), (:))
import Data.Array as Array
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable as Nullable
import Data.String as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign)
import IdePurescript.Tokens (startsWithCapitalLetter)
import LanguageServer.IdePurescript.Assist (lineRange')
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.FileTypes as FileTypes
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (DidChangeWatchedFilesParams, applyEdit)
import LanguageServer.Protocol.Text (makeWorkspaceEdit)
import LanguageServer.Protocol.TextDocument (getTextAtVersion)
import LanguageServer.Protocol.Types (Connection, DocumentStore, DocumentUri(..), FileChangeType(..), FileEvent(..), fromFileChangeTypeCode)

handleDidChangeWatchedFiles ∷
  Ref Foreign ->
  Connection ->
  Ref ServerState -> DocumentStore -> DidChangeWatchedFilesParams -> Aff Unit
handleDidChangeWatchedFiles configRef conn stateRef documents { changes } = do
  for_ changes \(FileEvent { uri, "type": fileChangeTypeCode }) ->
    case FileTypes.uriToRelevantFileType uri of
      FileTypes.PureScriptFile -> case fromFileChangeTypeCode fileChangeTypeCode of
        Just CreatedChangeType -> handleFileCreated configRef conn stateRef documents uri
        _ -> pure unit
      FileTypes.JavaScriptFile -> ifJsFileEvaluatePsFfi stateRef documents uri 
      FileTypes.UnsupportedFile -> pure unit



-- | JS files may in PureScript project may be used for FFI
-- | When a JS file change is passed, check for an associated PS file and queue a fast rebuild
ifJsFileEvaluatePsFfi :: Ref ServerState -> DocumentStore -> DocumentUri -> Aff Unit
ifJsFileEvaluatePsFfi serverStateRef documentStore documentUri =
  case FileTypes.jsUriToMayPsUri documentUri of
    Nothing -> pure unit
    Just psUri -> liftEffect do
      -- try to get from document store
      -- case if found, add to the serverState fast build
      mayDoc <- Nullable.toMaybe <$> getDocument documentStore psUri
      case mayDoc of
        Nothing -> pure unit
        Just doc -> queueForFastBuild serverStateRef psUri doc
    where
      queueForFastBuild ssRef docUri textDocument = (flip Ref.modify_)
        ssRef
        \(ServerState ss) -> ServerState ss{fastRebuildQueue=Map.insert docUri textDocument ss.fastRebuildQueue}


handleFileCreated ∷
  Ref Foreign -> Connection -> Ref ServerState -> DocumentStore -> DocumentUri -> Aff Unit
handleFileCreated configRef connection stateRef documents uri = do
  insertModuleHeader configRef connection stateRef documents uri

-- | Adds the first line module X.Y.Z where to the document based on its path.
-- | Only does so when the file is empty
insertModuleHeader ∷ Ref Foreign -> Connection -> Ref ServerState -> DocumentStore -> DocumentUri -> Aff Unit
insertModuleHeader configRef connection stateRef documents uri = do
  ServerState { clientCapabilities } <- liftEffect $ Ref.read stateRef
  maybeDoc <- liftEffect $ getDocument documents uri
  for_ (Nullable.toMaybe maybeDoc) \doc -> do 
    { text, version } <- liftEffect $ getTextAtVersion doc
    when (text == "") do
      for_ (inferModuleName uri) \inferredModuleName -> do
        config <- liftEffect $ Ref.read configRef
        let
          preludeModule = Config.preludeModule config
          toInsert = "module " <> inferredModuleName <> " where\n\nimport " <> preludeModule <> "\n"
          edit = makeWorkspaceEdit clientCapabilities uri version (lineRange' 0 (String.length toInsert)) toInsert
        applyEdit connection edit

inferModuleName ∷ DocumentUri -> Maybe String
inferModuleName (DocumentUri uri) = ado
  nameWithoutExtension <- case String.stripSuffix (String.Pattern ".purs") uri of
    Nothing -> String.stripSuffix (String.Pattern ".js") uri
    Just x -> Just x
  in moduleNameFromFolderStructure nameWithoutExtension

-- | Guesses the module name from the folder structure
-- | It goes backwards through the uri until it hits a folder that does not
-- | start with a capital letter (such as "src" or "test")
-- | Then glues them together with a dot
-- | Expects a string that has no ".purs" extension
moduleNameFromFolderStructure ∷ String -> String
moduleNameFromFolderStructure path =
  let
    dirs = String.split (String.Pattern "/") path
    parentIndex = fromMaybe (-1) $ Array.findLastIndex (not <<< startsWithCapitalLetter) dirs
    parts = Array.drop (parentIndex + 1) dirs
    parts' = case dirs !! parentIndex of
              Just "test" | parts !! 0 /= Just "Test" -> "Test" : parts
              _ -> parts
  in String.joinWith "." parts'
