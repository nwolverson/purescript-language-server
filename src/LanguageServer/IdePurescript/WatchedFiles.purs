module LanguageServer.IdePurescript.WatchedFiles where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign)
import IdePurescript.Tokens (startsWithCapitalLetter)
import LanguageServer.IdePurescript.Assist (lineRange')
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.Console (log)
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (DidChangeWatchedFilesParams, applyEdit)
import LanguageServer.Protocol.Text (makeWorkspaceEdit)
import LanguageServer.Protocol.TextDocument (getText, getVersion)
import LanguageServer.Protocol.Types (Connection, DocumentStore, DocumentUri(..), FileChangeType(..), FileEvent(..), fromFileChangeTypeCode)

handleDidChangeWatchedFiles ∷
  Ref Foreign ->
  Connection ->
  Ref ServerState -> DocumentStore -> DidChangeWatchedFilesParams -> Aff Unit
handleDidChangeWatchedFiles configRef conn stateRef documents { changes } = do
  for_ changes \(FileEvent { uri, "type": fileChangeTypeCode }) -> do
    case fromFileChangeTypeCode fileChangeTypeCode of
      Just CreatedChangeType -> do
        liftEffect $ log conn $ "CREATED " <> show uri
        handleFileCreated configRef conn stateRef documents uri
        liftEffect $ log conn  "CREATED DONE"
      Just DeletedChangeType -> do
        liftEffect 
          $ log conn 
          $ "Deleted " <> un DocumentUri uri <> " - full build may be required"
      _ -> pure unit

handleFileCreated ∷
  Ref Foreign -> Connection -> Ref ServerState -> DocumentStore -> DocumentUri -> Aff Unit
handleFileCreated configRef connection stateRef documents uri = do
  insertModuleHeader configRef connection stateRef documents uri
  liftEffect
    $ log connection do
        "Created " <> un DocumentUri uri <> " - full build may be required"

-- | Adds the first line module X.Y.Z where to the document based on its path.
-- | Only does so when the file is empty
insertModuleHeader ∷ Ref Foreign -> Connection -> Ref ServerState -> DocumentStore -> DocumentUri -> Aff Unit
insertModuleHeader configRef connection stateRef documents uri = do
  ServerState { clientCapabilities } <- Ref.read stateRef # liftEffect
  doc <- getDocument documents uri # liftEffect
  text <- getText doc # liftEffect
  when (text == "") do
    for_ (inferModuleName uri) \inferredModuleName -> do
      version <- getVersion doc # liftEffect
      config <- Ref.read configRef # liftEffect
      let
        preludeModule = Config.preludeModule config
        toInsert = "module " <> inferredModuleName <> " where\n\nimport " <> preludeModule <> "\n"
        edit = makeWorkspaceEdit clientCapabilities uri version (lineRange' 0 (String.length toInsert)) toInsert
      applyEdit connection edit

inferModuleName ∷ DocumentUri -> Maybe String
inferModuleName (DocumentUri uri) = ado
  nameWithoutExtension <- String.stripSuffix (String.Pattern ".purs") uri
  in moduleNameFromFolderStructure nameWithoutExtension

-- | Guesses the module name from the folder structure
-- | It goes backwards through the uri until it hits a folder that does not
-- | start with a capital letter (such as "src" or "test")
-- | Then glues them together with a dot
-- | Expects a string that has no ".purs" extension
moduleNameFromFolderStructure ∷ String -> String
moduleNameFromFolderStructure =
  String.split (String.Pattern "/")
    >>> Array.reverse
    >>> Array.takeWhile startsWithCapitalLetter
    >>> Array.reverse
    >>> String.joinWith "."
