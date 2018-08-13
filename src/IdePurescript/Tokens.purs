module IdePurescript.Tokens where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.String (length, take, drop)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)

type WordRange = { left :: Int, right :: Int }

modulePart :: String
modulePart = """((?:[A-Z][A-Za-z0-9]*\.)*(?:[A-Z][A-Za-z0-9]*))"""

identPart :: String
identPart = "((?:[a-zA-Z_][a-zA-Z0-9_']*)|[:!#$%&*+\\./<=>?@\\^|~-]+)"

modulePrefix :: String
modulePrefix = "(?:^|[^A-Za-z_.])(?:" <> modulePart <> """\.)"""

moduleRegex :: Either String Regex
moduleRegex = regex (modulePrefix <> "?" <> identPart <> "?$") noFlags

identifierAtPoint :: String -> Int -> Maybe { word :: String, range :: WordRange, qualifier :: Maybe String }
identifierAtPoint line column =
  let beforeRegex = regex "[a-zA-Z_0-9':!#$%&*+/<=>?@^|~-]*$" noFlags
      afterRegex = regex "^[a-zA-Z_0-9':!#$%&*+/<=>?@^|~-]*" noFlags
      moduleEndRegex = regex (modulePrefix <> "$") noFlags
      textBefore = take column line
      textAfter = drop column line
      wordRange left right = { left: column - left, right: column + right }
      match' r t = either (const Nothing) (\r' -> match r' t) r
  in
  case match' beforeRegex textBefore, match' afterRegex textAfter of
    Just ss, Just ss'
      | Just s <- NEA.head ss
      , Just s' <- NEA.head ss'  ->
        let qualifier = case match' moduleEndRegex (take (length textBefore - length s) textBefore) of
                            Just arr | [ _, mm ] <- NEA.toArray arr -> mm
                            _ -> Nothing
        in
          Just { word : s<>s', range : wordRange (length s) (length s'), qualifier }
    _, _ -> Nothing
