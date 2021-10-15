module LanguageServer.IdePurescript.Imports where

import Prelude

import Control.Error.Util (hush)
import Control.Monad.Except (runExcept)
import Data.Array (fold, fromFoldable, singleton, (:))
import Data.Either (Either(..))
import Data.Foldable (all, for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un, unwrap)
import Data.Nullable (toNullable)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign, readString, unsafeToForeign)
import IdePurescript.Modules (ImportResult(..), addExplicitImport, addModuleImport, addQualifiedImport, reformatModuleImports)
import IdePurescript.PscIde (getAvailableModules)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import LanguageServer.IdePurescript.Config (autocompleteAddImport, preludeModule)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (applyEdit)
import LanguageServer.Protocol.Text (makeMinimalWorkspaceEdit)
import LanguageServer.Protocol.TextDocument (TextDocument, getText, getVersion)
import LanguageServer.Protocol.Types (DocumentStore, DocumentUri(DocumentUri), Settings, WorkspaceEdit)
import LanguageServer.Protocol.Uri (uriToFilename)
import LanguageServer.Protocol.Window as Window
import PscIde.Command as C

addCompletionImport :: Notify -> DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Foreign
addCompletionImport = addCompletionImport' mempty

addCompletionImport' :: WorkspaceEdit -> Notify -> DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Foreign
addCompletionImport' existingEdit log docs config state@(ServerState { conn }) args = do
  let shouldAddImport = autocompleteAddImport config
  case conn, (runExcept <<< readString) <$> args, shouldAddImport of
    Just conn', [ Right identifier, mod, qual, Right uriRaw, Right ns ], true -> do
      let uri = DocumentUri uriRaw
      doc <- liftEffect $ getDocument docs uri
      version <- liftEffect $ getVersion doc
      text <- liftEffect $ getText doc
      edit <- addCompletionImportEdit log docs config state { identifier, mod: hush mod, qual: hush qual, uri } doc version text (parseNS ns)
      case edit of
        Right edits -> do
          void $ applyEdit conn' (fold $ existingEdit : edits)
          pure $ unsafeToForeign $ toNullable Nothing
        Left res -> do
          void $ applyEdit conn' existingEdit
          pure res
    Just conn', _, _ -> do
      void $ applyEdit conn' existingEdit
      pure $ unsafeToForeign $ toNullable Nothing
    _, _, _ -> pure $ unsafeToForeign $ toNullable Nothing

type CompletionImportArgs
  = { identifier :: String
    , mod :: Maybe String
    , qual :: Maybe String
    , uri :: DocumentUri
    }

parseNS :: String -> Maybe C.Namespace
parseNS "NSValue" = Just C.NSValue
parseNS "NSKind" = Just C.NSKind
parseNS "NSType" = Just C.NSType
parseNS _ = Nothing

showNS :: C.Namespace -> String
showNS C.NSValue = "NSValue"
showNS C.NSKind = "NSKind"
showNS C.NSType = "NSType"

addCompletionImportEdit ::
  Notify ->
  DocumentStore ->
  Settings ->
  ServerState ->
  CompletionImportArgs ->
  TextDocument ->
  Number ->
  String ->
  Maybe C.Namespace ->
  Aff (Either Foreign (Array WorkspaceEdit))
addCompletionImportEdit log _ config (ServerState { port, modules, conn, clientCapabilities }) { identifier, mod, qual, uri } _ version text ns = do
  let prelude = preludeModule config
  case port of
    Just port' -> do
      { result } <-
        case mod, qual of
          Just mod', Just qual'
            | noModule (isSameQualified mod' qual') -> do
              addQualifiedImport modules port' (un DocumentUri uri) text mod' qual'
          Just mod', Nothing
            | mod' == prelude && noModule (isSameUnqualified prelude) -> do
              addModuleImport modules port' (un DocumentUri uri) text mod'
          mod', qual' ->
            addExplicitImport modules port' (un DocumentUri uri) text mod' qual' identifier ns
      case result of
        UpdatedImports newText -> do
          let edit = makeMinimalWorkspaceEdit clientCapabilities uri version text newText
          pure $ Right $ maybe [] singleton edit
        AmbiguousImport imps ->
          liftEffect do
            liftEffect
              $ for_ conn
                  ( _
                      `Window.showError`
                        ("Could not import " <> text <> " because there is more than one option")
                  )
            log Warning "Found ambiguous imports"
            pure $ Left $ unsafeToForeign $ (\(C.TypeInfo { module' }) -> module') <$> imps
        -- UnnecessaryImport is not unusual - e.g. already existing import will hit this case
        -- And so will things that are implicitly imported because they live under Prim
        -- - e.g. Array or String
        UnnecessaryImport -> do
          pure $ Right []
        -- Failed imports are now rare and will display an error
        FailedImport msg -> do
          liftEffect $ for_ conn (_ `Window.showError` ("Failed to import: `" <> identifier <> "`. Error: " <> msg))
          pure $ Right []
    _ -> pure $ Right []

  where

  noModule f = all (not f <<< unwrap) modules.modules
  isSameQualified mod' qual' = case _ of
    { moduleName: mod'', qualifier: Just qual'' } -> mod' == mod'' && qual' == qual''
    _ -> false

  isSameUnqualified mod' = case _ of
    { moduleName, qualifier: Nothing } -> mod' == moduleName
    _ -> false

addModuleImport' :: Notify -> DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Foreign
addModuleImport' log docs config state args = do
  let ServerState { port, modules, conn, clientCapabilities } = state
  case port, (runExcept <<< readString) <$> args of
    Just port', [ Right mod', qual', Right uri ] -> do
      doc <- liftEffect $ getDocument docs (DocumentUri uri)
      version <- liftEffect $ getVersion doc
      text <- liftEffect $ getText doc
      fileName <- liftEffect $ uriToFilename $ DocumentUri uri
      edit <- case qual' of
        Right _ ->
          addCompletionImportEdit log docs config state
            { identifier: ""
            , qual: hush qual'
            , mod: Just mod'
            , uri: DocumentUri uri
            }
            doc
            version
            text
            Nothing
        Left _ -> do
          { result: res } <- addModuleImport modules port' fileName text mod'
          case res of
            UpdatedImports result -> do
              pure $ Right $ fromFoldable $ makeMinimalWorkspaceEdit clientCapabilities (DocumentUri uri) version text result
            _ -> pure $ Right []
      case conn, edit of
        Just conn', Right edits -> do
          void $ applyEdit conn' (fold edits)
        _, _ -> pure unit
      pure successResult

    _, args' -> do
      liftEffect $ log Info $ show args'
      pure successResult

  where
  successResult = unsafeToForeign $ toNullable Nothing

getAllModules :: Notify -> DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Foreign
getAllModules log _ _ state _ =
  case state of
    ServerState { port: Just port } ->
      unsafeToForeign <$> getAvailableModules port
    _ -> do
      liftEffect $ log Error "Fail case"
      pure $ unsafeToForeign []

reformatImports :: Notify -> DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Foreign
reformatImports log docs _ state args = do
  let ServerState { port, modules, conn, clientCapabilities } = state
  case port, (runExcept <<< readString) <$> args of
    Just port', [ Right uri ] -> do
      doc <- liftEffect $ getDocument docs (DocumentUri uri)
      version <- liftEffect $ getVersion doc
      text <- liftEffect $ getText doc
      fileName <- liftEffect $ uriToFilename $ DocumentUri uri
      res <- reformatModuleImports log modules port' fileName text
      case res of
        Just { result } -> do
          let edit = makeMinimalWorkspaceEdit clientCapabilities (DocumentUri uri) version text result
          case conn, edit of
            Just conn', Just edit' -> void $ applyEdit conn' edit'
            _, _ -> pure unit
        _ -> pure unit
      pure successResult

    _, args' -> do
      liftEffect $ log Info $ show args'
      pure successResult

  where
  successResult = unsafeToForeign $ toNullable Nothing
