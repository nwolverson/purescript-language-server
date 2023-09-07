module IdePurescript.Tokens where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String (Pattern(..), contains)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (noFlags)
import IdePurescript.Regex (test')
import PureScript.CST.Lexer as CST.Lexer
import PureScript.CST.TokenStream as TokenStream
import PureScript.CST.Types (ModuleName(..), Token(..))

type WordRange = { left :: Int, right :: Int }

-- Regexes still used by Completion
modulePart :: String
modulePart = """((?:[A-Z][A-Za-z0-9]*\.)*(?:[A-Z][A-Za-z0-9]*))"""

identPart :: String
identPart = """((?:[a-zA-Z_][a-zA-Z0-9_']*)|[:!#$%&*+./<=>?@\^|~\\-]+)"""

moduleRegex :: Either String Regex
moduleRegex = regex (modulePrefix <> "?" <> identPart <> "?$") noFlags
  where
  modulePrefix :: String
  modulePrefix = "(?:^|[^A-Za-z_.])(?:" <> modulePart <> """\.)"""

type TokenInfo = { word :: String, range :: WordRange, qualifier :: Maybe String }

-- | Finds identifier token at given position (column number) of the text
-- | string.
identifierAtPoint :: String -> Int -> Maybe TokenInfo
identifierAtPoint line column =
  go $ TokenStream.step $ CST.Lexer.lex line
  where
  go
    ( TokenStream.TokenCons
        { range: { start: { column: startCol }, end: { column: endCol } }
        , value
        }
        _
        str
        _
    ) =
    if column < startCol then
      Nothing
    else if column >= endCol then
      go $ TokenStream.step str
    else
      let
        range = { left: startCol, right: endCol } -- ish?
        res mn word = Just { range, word, qualifier: un ModuleName <$> mn }
      in
        case value of
          TokLowerName mn word -> res mn word
          TokUpperName mn word -> res mn word
          TokOperator mn word -> res mn word
          TokSymbolName mn word -> res mn word
          _ -> Nothing
  go _ = Nothing

startsWithCapitalLetter :: String -> Boolean
startsWithCapitalLetter = test' (regex "^[A-Z]" noFlags)

containsArrow :: String -> Boolean
containsArrow type' =
  contains (Pattern "->") type' || contains (Pattern "→") type'

-- What happens for ->> or "->!"? Can we be more precise to identify functions?
