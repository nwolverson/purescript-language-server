module IdePurescript.Completion where

import Prelude

import Control.Alt ((<|>))
import Data.Array (concatMap, filter, head, intersect, sortBy, (:))
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, indexOf, length)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Utils (startsWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import IdePurescript.PscIde (getAvailableModules, getCompletion')
import IdePurescript.Regex (match', test')
import IdePurescript.Tokens (identPart, modulePart, moduleRegex)
import PscIde.Command (CompletionOptions(..), TypeInfo(..))
import PscIde.Command as C

type ModuleInfo =
  { modules :: Array String
  , getQualifiedModule :: String -> Array String
  , mainModule :: Maybe String
  , importedModules :: Array String
  }

data SuggestionType = Module | Type | DCtor | Function | Value | Kind

instance showSuggestionType :: Show SuggestionType where
  show Module = "Module"
  show Type = "Type"
  show DCtor = "DCtor"
  show Function = "Function"
  show Value = "Value"
  show Kind = "Kind"

parseSuggestionType :: String -> Maybe SuggestionType
parseSuggestionType = case _ of
  "Module" -> Just Module
  "Type" -> Just Type
  "DCtor" -> Just DCtor
  "Function" -> Just Function
  "Value" -> Just Value
  _ -> Nothing

explicitImportRegex :: Either String Regex
explicitImportRegex = regex ("""^import\s+""" <> modulePart <> """\s+\([^)]*?""" <> identPart <> "$") noFlags

getModuleSuggestions :: Int -> String -> Aff (Array String)
getModuleSuggestions port prefix = do
  list <- getAvailableModules port
  pure $ filter (\m -> indexOf (Pattern prefix) m == Just 0) list

data SuggestionResult =
  ModuleSuggestion { text :: String, suggestType :: SuggestionType, prefix :: String }
  | IdentSuggestion { origMod :: String, exportMod :: String, exportedFrom :: Array String, identifier :: String, qualifier :: Maybe String, valueType :: String, suggestType :: SuggestionType, namespace :: Maybe C.Namespace, prefix :: String, documentation :: Maybe String }
  | QualifierSuggestion { text :: String }


getSuggestions :: Int -> {
    line :: String,
    moduleInfo :: ModuleInfo,
    qualifiers :: Array String,
    groupCompletions :: Boolean,
    maxResults :: Maybe Int,
    preferredModules :: Array String
  } -> Aff (Array SuggestionResult)
getSuggestions port
    { line
    , moduleInfo: { modules, getQualifiedModule, mainModule, importedModules }
    , qualifiers
    , maxResults
    , groupCompletions
    , preferredModules
    } =
  if moduleExplicit then
    case match' explicitImportRegex line of
      Just [ Just _, Just mod, Just token ] -> do
        let cc ns = Tuple ns <$> getCompletion' Nothing [C.PrefixFilter token, C.NamespaceFilter [ ns ] ] port mainModule Nothing [ mod ] getQualifiedModule opts
        completions <- traverse cc [ C.NSValue, C.NSType, C.NSKind ]
        pure $ concatMap (\(Tuple n cs) -> result Nothing token (Just n) <$> cs) completions
      _ -> pure []
  else
    case parsed of
      Just { mod, token } ->
        if moduleCompletion then do
          let prefix = getModuleName (fromMaybe "" mod) token
          completions <- getModuleSuggestions port prefix
          pure $ map (modResult prefix) completions
        else do
          let cc ns = Tuple ns <$> getCompletion' Nothing [C.PrefixFilter token, C.NamespaceFilter [ ns ] ] port mainModule mod ("Prim":modules) getQualifiedModule opts
          completions <- traverse cc [ C.NSValue, C.NSType, C.NSKind ]
          pure $ 
            matchingQualifiers token <> 
            concatMap (\(Tuple n cs) -> result mod token (Just n) <$> cs) completions
      Nothing -> pure []
    where
    opts = CompletionOptions { maxResults, groupReexports: groupCompletions }

    matchingQualifiers token = convQ <$> Array.filter (\q -> indexOf (Pattern token) q == Just 0) qualifiers
      where
      convQ text = QualifierSuggestion { text }

    getModuleName "" token  = token
    getModuleName mod token = mod <> "." <> token

    isImport = indexOf (Pattern "import") line == Just 0
    hasBracket = indexOf (Pattern "(") line /= Nothing
    moduleCompletion = isImport && not hasBracket
    moduleExplicit = isImport && hasBracket

    parsed = case match' moduleRegex line of
        Just [ Just _, mod, tok ] | mod /= Nothing || tok /= Nothing ->
          Just { mod, token: fromMaybe "" tok}
        _ -> Nothing

    modResult prefix moduleName = ModuleSuggestion { text: moduleName, suggestType: Module, prefix }
    result qualifier prefix ns (TypeInfo {type', identifier, module': origMod, exportedFrom, documentation }) =
      IdentSuggestion { origMod, exportMod, identifier, qualifier, suggestType, prefix, valueType: type', namespace: ns, exportedFrom, documentation }
      where
        suggestType = 
          case ns of
            Just C.NSKind -> Kind
            Just C.NSType -> Type
            Just C.NSValue | test' (regex "^[A-Z]" noFlags) identifier -> DCtor
                           | contains (Pattern "->") type' || contains (Pattern "→") type' -> Function
            Just C.NSValue -> Value
            Nothing -> Value
          -- if ns == Just C.NSKind then Kind
          -- else Value
          -- else if 
          -- if type' == "Type" then Type
          -- else if type' == "kind" then Kind
          -- else if test' (regex "(->|→) Type$" noFlags) type' then Type -- type constructor
          -- -- else if identifier == "Baz" then Module
          -- else if contains (Pattern "Type") type' then Type
          -- else if test' (regex "^[A-Z]" noFlags) identifier then DCtor
          -- else if contains (Pattern "->") type' || contains (Pattern "→") type' then Function
          -- else Value

        -- Strategies for picking the re-export to choose
        -- 1. User configuration of preferred modules (ordered list)
        -- 2. Existing imports
        -- 3. Re-export from a prefix named module (e.g. Foo.Bar.Baz reexported from Foo.Bar) shortest first
        -- 4. Original module (if none of the previous rules apply, there are no re-exports, or either grouping
        --    is disabled or compiler version does not support it)
        exportMod = fromMaybe origMod (preferredModule <|> existingModule <|> prefixModule)
        existingModule = head $ intersect importedModules exportedFrom
        preferredModule = head $ intersect preferredModules exportedFrom
        prefixModule = head $
          sortBy (\a b -> length a `compare` length b) $
          filter (\m -> startsWith (m <> ".") origMod) exportedFrom
