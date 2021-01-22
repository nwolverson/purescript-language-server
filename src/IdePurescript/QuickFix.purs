module IdePurescript.QuickFix where

import Prelude

import Data.Foldable (elem)
import Data.String (null, trim)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (noFlags)
import IdePurescript.Regex (replace', test')
import Data.String.Regex.Flags (global, noFlags)

-- | Modify suggestion replacement text, removing extraneous newlines
getReplacement :: String -> String -> String
getReplacement replacement' extraText =
  (trim $ replace' (regex "\\s+\n" global) "\n" replacement')
  <> if addNewline then "\n" else ""
  where
  trailingNewline = test' (regex "\n\\s+$" noFlags) replacement'
  addNewline = trailingNewline && (not $ null extraText)

-- | Get a title which explains what applying a compiler suggestion will do
getTitle :: String -> String
getTitle code = case code of
  "UnusedImport"                -> "Remove import"
  "RedundantEmptyHidingImport"  -> "Remove import"
  "DuplicateImport"             -> "Remove import"
  "RedundantUnqualifiedImport"  -> "Remove import"
  "DeprecatedQualifiedSyntax"   -> "Remove qualified keyword"
  "ImplicitImport"              -> "Make import explicit"
  "UnusedExplicitImport"        -> "Remove unused references"
  _                             -> "Apply suggestion"

-- | Determine whether an error code represents an unknown token (unknown identifier or missing import)
isUnknownToken :: String -> Boolean
isUnknownToken = flip elem
  [ "UnknownValue"
  , "UnknownType"
  , "UnknownDataConstructor"
  , "UnknownTypeConstructor"
    -- In later compiler versions UnknownName covers all of the above
  , "UnknownName" ]

isImport :: String -> Boolean
isImport = flip elem
  [ "UnusedImport" 
  , "DuplicateImport"
  , "HidingImport"
  , "ImplicitImport"
  , "ImplicitQualifiedImport"
  , "UnusedDctorExplicitImport"
  , "UnusedDctorImport"
  , "UnusedExplicitImport"
  ]