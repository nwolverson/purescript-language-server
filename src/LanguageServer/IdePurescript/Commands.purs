module LanguageServer.IdePurescript.Commands where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Foreign (Foreign, unsafeToForeign)
import LanguageServer.Types (Command(..), DocumentUri, Range)
import PscIde.Command (TypeInfo)

cmdName :: CommandInfo -> String
cmdName (CommandInfo _ command) = "purescript." <> command

c :: CommandInfo -> Maybe (Array Foreign) -> Command
c cmd@(CommandInfo title command) args = Command { title, command: cmdName cmd, arguments: toNullable args }

data CommandInfo = CommandInfo String String

caseSplitCmd :: CommandInfo
caseSplitCmd = CommandInfo "Case split (explicit position)" "caseSplit-explicit"

addClauseCmd :: CommandInfo
addClauseCmd = CommandInfo "Add clause (explicit position/cmd)" "addClause-explicit"

addCompletionImportCmd :: CommandInfo
addCompletionImportCmd = CommandInfo "Add completion import" "addCompletionImport"

addCompletionImport :: String -> Maybe String -> Maybe String -> DocumentUri -> Command
addCompletionImport ident mod qual uri = c addCompletionImportCmd $
  Just [ unsafeToForeign ident, unsafeToForeign $ toNullable mod, unsafeToForeign $ toNullable qual, unsafeToForeign uri ]

addModuleImportCmd :: CommandInfo
addModuleImportCmd = CommandInfo "Add module import" "addModuleImport"

replaceSuggestionCmd :: CommandInfo
replaceSuggestionCmd = CommandInfo "Apply Suggestion" "replaceSuggestion"

replaceSuggestion :: String -> DocumentUri -> String -> Range -> Command
replaceSuggestion title uri replacement fixRange = c (CommandInfo title "replaceSuggestion") $ 
  Just $ [ unsafeToForeign uri, unsafeToForeign replacement, unsafeToForeign fixRange ]

replaceAllSuggestionsCmd :: CommandInfo
replaceAllSuggestionsCmd = CommandInfo "Replace all suggestions" "replaceAllSuggestions"

type Replacement = { replacement:: String, range :: Range }

replaceAllSuggestions :: String -> DocumentUri -> Array Replacement -> Command
replaceAllSuggestions text uri replacements = c (CommandInfo text "replaceAllSuggestions") $ 
  Just $ [ unsafeToForeign uri, unsafeToForeign replacements ]

buildCmd :: CommandInfo
buildCmd = CommandInfo "Build" "build"

build :: Command
build = c buildCmd Nothing

typedHoleCmd :: CommandInfo
typedHoleCmd = CommandInfo "Insert typed hole suggestion" "typedHole"

typedHole :: String -> DocumentUri -> Range -> Array TypeInfo -> Command
typedHole name url range options = c typedHoleCmd (Just $ unsafeToForeign name : unsafeToForeign url : unsafeToForeign range : (unsafeToForeign <$> options) )

typedHoleExplicitCmd :: CommandInfo
typedHoleExplicitCmd = CommandInfo "Insert typed hole suggestion" "typedHole-explicit"

startPscIdeCmd :: CommandInfo
startPscIdeCmd = CommandInfo "Start Psc-Ide-Server" "startPscIde"

stopPscIdeCmd :: CommandInfo
stopPscIdeCmd = CommandInfo "Stop Psc-Ide-Server" "stopPscIde"

restartPscIdeCmd :: CommandInfo
restartPscIdeCmd = CommandInfo "Restart Psc-Ide-Server" "restartPscIde"

getAvailableModulesCmd :: CommandInfo
getAvailableModulesCmd = CommandInfo "Get available modules" "getAvailableModules"

searchCmd :: CommandInfo
searchCmd = CommandInfo "Search identifiers" "search"

fixTypoCmd :: CommandInfo
fixTypoCmd = CommandInfo "Fix typo/add import" "fixTypo"

fixTypo :: DocumentUri -> Int -> Int -> Command
fixTypo uri row char = c fixTypoCmd $ Just $ [ unsafeToForeign uri, unsafeToForeign row, unsafeToForeign char ] 

fixTypo' :: String -> DocumentUri -> Int -> Int -> Foreign -> Command
fixTypo' x uri row char tinfo = c (CommandInfo x "fixTypo") $ Just $ [ unsafeToForeign uri, unsafeToForeign row, unsafeToForeign char, tinfo ] 


commands :: Array String
commands = cmdName <$> 
  [ addCompletionImportCmd 
  , caseSplitCmd
  , addClauseCmd
  , replaceSuggestionCmd
  , buildCmd
  , startPscIdeCmd
  , stopPscIdeCmd
  , restartPscIdeCmd
  , typedHoleExplicitCmd
  , replaceAllSuggestionsCmd
  , fixTypoCmd
  ]

