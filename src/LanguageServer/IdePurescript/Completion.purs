module LanguageServer.IdePurescript.Completion where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (length) as Arr
import Data.Maybe (Maybe(..))
import Data.Newtype (over, un, unwrap)
import Data.Nullable (toNullable)
import Data.String (length)
import IdePurescript.Completion (SuggestionResult(..), SuggestionType(..), getSuggestions)
import IdePurescript.Modules (getAllActiveModules, getQualModule, getUnqualActiveModules)
import IdePurescript.PscIde (getLoadedModules)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (TextDocumentPositionParams)
import LanguageServer.IdePurescript.Commands (addCompletionImport)
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.Types (MainEff, ServerState)
import LanguageServer.TextDocument (getTextAtRange)
import LanguageServer.Types (CompletionItem(..), DocumentStore, Position(..), Range(..), Settings, TextDocumentIdentifier(..), TextEdit(..), completionItem, CompletionItemList(..))
import LanguageServer.Types as LS

getCompletions :: forall eff. DocumentStore -> Settings -> ServerState (MainEff eff) -> TextDocumentPositionParams -> Aff (MainEff eff) CompletionItemList
getCompletions docs settings state ({ textDocument, position }) = do
    let uri = _.uri $ un TextDocumentIdentifier textDocument
    doc <- liftEff $ getDocument docs uri
    line <- liftEff $ getTextAtRange doc (mkRange position)
    let autoCompleteAllModules = Config.autoCompleteAllModules settings
        { port, modules } = unwrap state
        getQualifiedModule = (flip getQualModule) modules

    case port of
        Just port' ->  do
            usedModules <- if autoCompleteAllModules
                then getLoadedModules port'
                else pure $ getUnqualActiveModules modules Nothing
            suggestions <- getSuggestions port' 
                { line
                , moduleInfo: { modules: usedModules, getQualifiedModule, mainModule: modules.main, importedModules: getAllActiveModules modules }
                , maxResults: Config.autocompleteLimit settings
                , groupCompletions: Config.autocompleteGrouped settings
                , preferredModules: Config.importsPreferredModules settings
                }
            pure $ result $ convert uri <$> suggestions
        _ -> pure $ result []

    where
    result arr = CompletionItemList
        { items: arr
        , isIncomplete: Config.autocompleteLimit settings == Just (Arr.length arr)
        }
    mkRange (pos@ Position { line, character }) = Range 
        { start: pos # over Position (_ { character = 0 })
        , end: pos
        }

    convertSuggest = case _ of
      Module -> LS.Module
      Value -> LS.Value
      Function -> LS.Function
      Type -> LS.Class

    convert _ (ModuleSuggestion { text, suggestType, prefix }) = completionItem text (convertSuggest suggestType)
    convert uri (IdentSuggestion { origMod, exportMod, identifier, qualifier, suggestType, prefix, valueType, exportedFrom }) =
        completionItem identifier (convertSuggest suggestType) 
        # over CompletionItem (_
          { detail = toNullable $ Just valueType
          , documentation = toNullable $ Just exportText
          , command = toNullable $ Just $ addCompletionImport identifier (Just exportMod) qualifier uri
        --   , textEdit = toNullable $ Just edit
          })
        where
        exportText = if exportMod == origMod then origMod else exportMod <> " (re-exported from " <> origMod <> ")"

        edit = TextEdit
            { range: Range
                { start: position # over Position (\pos -> pos { character = pos.character - length prefix })
                , end: position
                }
            , newText: identifier
            }
