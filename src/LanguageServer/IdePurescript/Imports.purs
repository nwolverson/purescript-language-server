module LanguageServer.IdePurescript.Imports where

import Prelude

import Control.Error.Util (hush)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Foreign (Foreign, readString, toForeign)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import IdePurescript.Modules (ImportResult(..), addExplicitImport, addModuleImport, addQualifiedImport)
import IdePurescript.PscIde (getAvailableModules)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (applyEdit)
import LanguageServer.IdePurescript.Config (autocompleteAddImport, preludeModule)
import LanguageServer.IdePurescript.Types (MainEff, ServerState(..))
import LanguageServer.Text (makeMinimalWorkspaceEdit)
import LanguageServer.TextDocument (getText, getVersion)
import LanguageServer.Types (DocumentStore, DocumentUri(..), Settings)
import LanguageServer.Uri (uriToFilename)
import PscIde.Command as C

addCompletionImport :: forall eff. Notify (MainEff eff) -> DocumentStore -> Settings -> ServerState (MainEff eff) -> Array Foreign -> Aff (MainEff eff) Foreign
addCompletionImport log docs config state@(ServerState { port, modules, conn }) args = do
  let shouldAddImport = autocompleteAddImport config
      prelude = preludeModule config
  case port, (runExcept <<< readString) <$> args, shouldAddImport of
    Just port, [ Right identifier, mod, qual, Right uri ], true -> do
      doc <- liftEff $ getDocument docs (DocumentUri uri)
      version <- liftEff $ getVersion doc
      text <- liftEff $ getText doc
      { state: modulesState', result } <-
        case hush mod, hush qual of
          Just mod', Just qual' | noModule (isSameQualified mod' qual') ->
            addQualifiedImport modules port uri text mod' qual'
          Just mod', Nothing | mod' == prelude && noModule (isSameUnqualified prelude) ->
            addOpenImport modules port uri text mod'
          mod', qual' ->
            addExplicitImport modules port uri text mod' qual' identifier
      liftEff $ case result of
        UpdatedImports newText -> do
          let edit = makeMinimalWorkspaceEdit (DocumentUri uri) version text newText
          case conn, edit of
            Just conn', Just edit' -> applyEdit conn' edit'
            _, _ -> pure unit
          pure successResult
        AmbiguousImport imps ->  do
          log Warning "Found ambiguous imports"
          pure $ toForeign $ (\(C.TypeInfo { module' }) -> module') <$> imps
        -- Failed import is not unusual - e.g. already existing import will hit this case.
        FailedImport -> pure successResult
    _, args', _ -> do
      when shouldAddImport $ liftEff $ log Info $ show args'
      pure successResult

    where
    successResult = toForeign $ toNullable Nothing

    noModule f = all (not f <<< unwrap) modules.modules
    isSameQualified mod qual = case _ of
      { moduleName: mod', qualifier: Just qual'} -> mod == mod' && qual == qual'
      _ -> false

    isSameUnqualified mod = case _ of
      { moduleName, qualifier: Nothing } -> mod == moduleName
      _ -> false

    -- addModuleImport discards the result data type and wraps it in Maybe. We
    -- need to add it back for the types to unify.
    addOpenImport modules port uri text mod =
      addModuleImport modules port uri text mod <#> case _ of
        Just r -> r { result = UpdatedImports r.result }
        Nothing -> { state: modules, result: FailedImport }


addModuleImport' :: forall eff. Notify (MainEff eff) -> DocumentStore -> Settings -> ServerState (MainEff eff) -> Array Foreign -> Aff (MainEff eff) Foreign
addModuleImport' log docs config state args = do
  let ServerState { port, modules, conn } = state
  case port, (runExcept <<< readString) <$> args of
    Just port', [ Right mod', qual', Right uri ] -> do
      doc <- liftEff $ getDocument docs (DocumentUri uri)
      version <- liftEff $ getVersion doc
      text <- liftEff $ getText doc
      fileName <- liftEff $ uriToFilename $ DocumentUri uri
      res <- addModuleImport modules port' fileName text mod'
      case res of
        Just { result } -> do
          let edit = makeMinimalWorkspaceEdit (DocumentUri uri) version text result
          case conn, edit of
            Just conn', Just edit' -> liftEff $ applyEdit conn' edit'
            _, _ -> pure unit
        _ -> pure unit
      pure successResult

    _, args'-> do
      liftEff $ log Info $ show args'
      pure successResult

    where
    successResult = toForeign $ toNullable Nothing


getAllModules :: forall eff. Notify (MainEff eff) -> DocumentStore -> Settings -> ServerState (MainEff eff) -> Array Foreign -> Aff (MainEff eff) Foreign
getAllModules log docs config state args =
  case state of
    ServerState { port: Just port, modules, conn } ->
      toForeign <$> getAvailableModules port
    _ -> do
      liftEff $ log Error "Fail case"
      pure $ toForeign []
