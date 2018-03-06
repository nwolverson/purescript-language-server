module IdePurescript.Completion where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Data.Array (filter, head, intersect, sortBy, (:))
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, indexOf, length)
import Data.String.Utils (startsWith)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (noFlags)
import IdePurescript.PscIde (eitherToErr, getCompletion)
import IdePurescript.Regex (match', test')
import IdePurescript.Tokens (identPart, modulePart, moduleRegex)
import PscIde (NET, listAvailableModules)
import PscIde.Command (CompletionOptions(..), ModuleList(..), TypeInfo(..))

type ModuleInfo =
  { modules :: Array String
  , getQualifiedModule :: String -> Array String
  , mainModule :: Maybe String
  , importedModules :: Array String
  }

data SuggestionType = Module | Type | Function | Value

explicitImportRegex :: Either String Regex
explicitImportRegex = regex ("""^import\s+""" <> modulePart <> """\s+\([^)]*?""" <> identPart <> "$") noFlags

getModuleSuggestions :: forall eff. Int -> String -> Aff (net :: NET | eff) (Array String)
getModuleSuggestions port prefix = do
  list <- eitherToErr $ listAvailableModules port
  pure $ case list of
    (ModuleList lst) -> filter (\m -> indexOf (Pattern prefix) m == Just 0) lst

data SuggestionResult =
  ModuleSuggestion { text :: String, suggestType :: SuggestionType, prefix :: String }
  | IdentSuggestion { origMod :: String, exportMod :: String, exportedFrom :: Array String, identifier :: String, qualifier :: Maybe String, valueType :: String, suggestType :: SuggestionType, prefix :: String, documentation :: Maybe String }

getSuggestions :: forall eff. Int -> {
    line :: String,
    moduleInfo :: ModuleInfo,
    groupCompletions :: Boolean,
    maxResults :: Maybe Int,
    preferredModules :: Array String
  } -> Aff (net :: NET | eff) (Array SuggestionResult)
getSuggestions port
    { line
    , moduleInfo: { modules, getQualifiedModule, mainModule, importedModules }
    , maxResults
    , groupCompletions
    , preferredModules
    } =
  if moduleExplicit then
    case match' explicitImportRegex line of
      Just [ Just _, Just mod, Just token ] -> do
        completions <- getCompletion port token mainModule Nothing [ mod ] getQualifiedModule opts
        pure $ map (result Nothing token) completions
      _ -> pure []
  else
    case parsed of
      Just { mod, token } ->
        if moduleCompletion then do
          let prefix = getModuleName (fromMaybe "" mod) token
          completions <- getModuleSuggestions port prefix
          pure $ map (modResult prefix) completions
        else do
          completions <- getCompletion port token mainModule mod ("Prim":modules) getQualifiedModule opts
          pure $ map (result mod token) completions
      Nothing -> pure []
    where
    opts = CompletionOptions { maxResults, groupReexports: groupCompletions }

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
    result qualifier prefix (TypeInfo {type', identifier, module': origMod, exportedFrom, documentation }) =
      IdentSuggestion { origMod, exportMod, identifier, qualifier, suggestType, prefix, valueType: type', exportedFrom, documentation }
      where
        suggestType =
          if contains (Pattern "->") type' then Function
          else if test' (regex "^[A-Z]" noFlags) identifier then Type
          else Value

        -- Strategies for picking the re-export to choose
        -- 1. Existing imports
        -- 2. User configuration of preferred modules (ordered list)
        -- 3. Re-export from a prefix named module (e.g. Foo.Bar.Baz reexported from Foo.Bar) shortest first
        -- 4. Original module (if none of the previous rules apply, there are no re-exports, or either grouping
        --    is disabled or compiler version does not support it)
        exportMod = fromMaybe origMod (existingModule <|> preferredModule <|> prefixModule)
        existingModule = head $ intersect importedModules exportedFrom
        preferredModule = head $ intersect preferredModules exportedFrom
        prefixModule = head $
          sortBy (\a b -> length a `compare` length b) $
          filter (\m -> startsWith (m <> ".") origMod) exportedFrom
