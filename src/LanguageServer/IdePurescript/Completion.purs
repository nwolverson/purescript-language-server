module LanguageServer.IdePurescript.Completion where

import Prelude

import Data.Array (filter, mapMaybe)
import Data.Array (length, null) as Arr
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (over, un, unwrap)
import Data.Nullable (toNullable)
import Data.String (Pattern(..), Replacement(..), indexOf, joinWith, length, replaceAll, split, toUpper)
import Data.String.Utils (toCharArray)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import IdePurescript.Completion (SuggestionResult(..), SuggestionType(..), getSuggestions)
import IdePurescript.Modules (State, getAllActiveModules, getModuleFromUnknownQualifier, getModuleName, getQualModule, getUnqualActiveModules)
import IdePurescript.Modules as Modules
import IdePurescript.PscIde (getLoadedModules)
import IdePurescript.PscIdeServer (Notify)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (TextDocumentPositionParams)
import LanguageServer.IdePurescript.Commands (addCompletionImport)
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.Imports (showNS)
import LanguageServer.IdePurescript.SuggestionRank (Ranking(..), cmapRanking)
import LanguageServer.IdePurescript.SuggestionRank as SuggestionRank
import LanguageServer.IdePurescript.Types (ServerState)
import LanguageServer.TextDocument (getTextAtRange)
import LanguageServer.Types (CompletionItem(..), DocumentStore, Position(..), Range(..), Settings, TextDocumentIdentifier(..), TextEdit(..), completionItem, CompletionItemList(..), markupContent)
import LanguageServer.Types as LS

getCompletions :: Notify -> DocumentStore -> Settings -> ServerState -> TextDocumentPositionParams -> Aff CompletionItemList
getCompletions notify docs settings state ({ textDocument, position }) = do
    let uri = _.uri $ un TextDocumentIdentifier textDocument
    doc <- liftEffect $ getDocument docs uri
    line <- liftEffect $ getTextAtRange doc (mkRange position)
    let autoCompleteAllModules = Config.autoCompleteAllModules settings
        { port, modules } = unwrap state
        getQualifiedModule = (flip getQualModule) modules

    case port of
        Just port' ->  do
            usedModules <- if autoCompleteAllModules
                then getLoadedModules port'
                else pure $ getUnqualActiveModules modules Nothing
            let qualifiers = mapMaybe (\(Modules.Module { qualifier }) -> qualifier) modules.modules
            { results, isIncomplete } <- getSuggestions notify port'
                { line
                , moduleInfo:
                    { modules: usedModules
                    , openModules: getUnqualActiveModules modules Nothing
                    , candidateModules: getUnqualActiveModules modules <<< Just
                    , getQualifiedModule
                    , mainModule: modules.main
                    , importedModules: getAllActiveModules modules
                    }
                , qualifiers
                , maxResults: Config.autocompleteLimit settings
                , groupCompletions: Config.autocompleteGrouped settings
                , preferredModules: Config.importsPreferredModules settings
                }
            pure $ CompletionItemList { items: convert uri <$> results, isIncomplete }
        _ -> pure $ result []

    where
    result arr = CompletionItemList
        { items: arr
        , isIncomplete: true
        }
    mkRange pos = Range
        { start: pos # over Position (_ { character = 0 })
        , end: pos
        }

    convertSuggest = case _ of
      Module -> LS.Module
      Value -> LS.Value
      Function -> LS.Function
      Type -> LS.Class
      DCtor ->  LS.Enum
      Kind -> LS.Interface

    edit newText prefix = TextEdit
        { range: Range
            { start: position # over Position (\pos -> pos { character = pos.character - length prefix })
            , end: position
            }
        , newText
        }

    convert _ (QualifierSuggestion { text }) =
        completionItem text LS.Module
    convert _ (ModuleSuggestion { text, suggestType, prefix }) =
        completionItem text (convertSuggest suggestType)
        # over CompletionItem (_
          { textEdit = toNullable $ Just $ edit text prefix
          })
    convert uri sugg@(IdentSuggestion { origMod, exportMod, identifier, qualifier, suggestType, prefix, valueType, documentation, namespace }) =
        completionItem identifier (convertSuggest suggestType)
        # over CompletionItem (_
          { detail = toNullable $ Just valueType
          , documentation = toNullable $ Just $ markupContent $ (fromMaybe "" documentation) <> exportText
          , command = toNullable $ Just $ addCompletionImport identifier (Just exportMod) qualifier uri (maybe "" showNS namespace)
          , textEdit = toNullable $ Just $ edit identifier prefix
          , sortText = toNullable $ Just $ rankText <> "." <> identifier
          })
        where
        exportText = "\n*From: " <> (if exportMod == origMod then origMod else exportMod <> " (re-exported from " <> origMod <> ")") <> "*"
        rankText = SuggestionRank.toString $ unwrap rankSuggestion { state: (unwrap state).modules, suggestion: sugg }

rankSuggestion :: Ranking  { state :: State, suggestion :: SuggestionResult }
rankSuggestion = flip cmapRanking rankUnknownQualified case _ of
    { state, suggestion: IdentSuggestion { qualifier: Just qualifier, exportMod } }
        | Arr.null (getQualModule qualifier state) -> Just { state, qualifier, mod: exportMod }
    _ -> Nothing

rankUnknownQualified :: Ranking { state :: State, qualifier :: String, mod :: String }
rankUnknownQualified =
    rankQualifiedWithType
    <> rankQualifiedWithSegment
    <> rankQualifiedWithAbv
    <> rankQualifiedWithConcat

rankQualifiedWithType :: Ranking { state :: State, qualifier :: String, mod :: String }
rankQualifiedWithType = Ranking \opts ->
    case getModuleFromUnknownQualifier opts.qualifier opts.state of
        Just mod | getModuleName mod == opts.mod -> top
        _ -> bottom

rankQualifiedWithSegment :: Ranking { state :: State, qualifier :: String, mod :: String }
rankQualifiedWithSegment = Ranking \opts ->
    let segments = split (Pattern ".") opts.mod
    in segments # foldMapWithIndex (\ix segment -> unwrap rankSegmentPrefix { len: Arr.length segments, ix, segment, prefix: opts.qualifier })

rankSegmentPrefix :: Ranking { len :: Int, ix :: Int, segment :: String, prefix :: String }
rankSegmentPrefix = Ranking \{ len, ix, segment, prefix } ->
    case indexOf (Pattern prefix) segment of
        Just 0 -> SuggestionRank.fromInt $ (1 + ix) * (1 + (length segment - length prefix)) + (len - ix)
        _ -> bottom

rankQualifiedWithAbv :: Ranking { state :: State, qualifier :: String, mod :: String }
rankQualifiedWithAbv = flip cmapRanking rankModuleAbv \opts ->
    if toUpper opts.qualifier == opts.qualifier
        then Just { abv: opts.qualifier, mod: opts.mod }
        else Nothing

rankModuleAbv :: Ranking { abv :: String, mod :: String }
rankModuleAbv = flip cmapRanking rankSub \{ abv, mod } ->
    let
        modAbv = mod
            # replaceAll (Pattern ".") (Replacement "")
            # toCharArray
            # filter (\ch -> toUpper ch == ch)
            # joinWith ""
    in
        Just { sub: abv, str: modAbv }

rankQualifiedWithConcat :: Ranking { state :: State, qualifier :: String, mod :: String }
rankQualifiedWithConcat = flip cmapRanking rankSub \opts ->
    Just { sub: opts.qualifier, str: replaceAll (Pattern ".") (Replacement "") opts.mod }

rankSub :: Ranking { sub :: String, str :: String }
rankSub = Ranking \{ sub, str } ->
    case indexOf (Pattern sub) str of
        Just ix -> SuggestionRank.fromInt ((1 + ix) * (1 + length sub - (length str + ix)))
        Nothing -> bottom
