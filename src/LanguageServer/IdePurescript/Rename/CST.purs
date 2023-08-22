module LanguageServer.IdePurescript.Rename.CST where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import LanguageServer.IdePurescript.Util.CST (sourceRangeToRange)
import LanguageServer.Protocol.Types (Range)
import PureScript.CST.Types (DataMembers(..), Declaration(..), Delimited, DelimitedNonEmpty, Export(..), Ident(..), Import(..), ImportDecl(..), Labeled(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName, Name(..), Operator(..), Proper(..), Separated(..), SourceRange, Wrapped(..))

delimitedToArray :: forall a. Delimited a -> Array a
delimitedToArray (Wrapped { value: Just (Separated { head, tail }) }) =
  Array.cons head (snd <$> tail)
delimitedToArray _ = []

delimitedToArray' :: forall a. DelimitedNonEmpty a -> Array a
delimitedToArray' (Wrapped { value: (Separated { head, tail }) }) =
  Array.cons head (snd <$> tail)

getExportedRanges :: forall a. Module a -> Boolean -> String -> Array Range
getExportedRanges (Module { header: ModuleHeader { exports } }) isType name =
  sourceRangeToRange
    <$> Array.foldMap go (maybe [] delimitedToArray' exports)
  where
  go = case _ of
    ExportValue (Name { token, name: Ident ident })
      | ident == name -> pure token.range

    ExportType (Name { token, name: Proper proper }) _
      | isType && proper == name -> pure token.range

    ExportType _ (Just (DataEnumerated ctors))
      | not isType -> maybe [] pure (findCtor name ctors)

    ExportOp (Name { token, name: Operator op })
      | not isType && op == name -> pure token.range

    ExportTypeOp _ (Name { token, name: Operator op })
      | isType && op == name -> pure token.range

    ExportClass _ (Name { token, name: Proper proper })
      | isType && name == proper -> pure token.range

    _ -> []

shiftRange :: Int -> SourceRange -> SourceRange
shiftRange num org@{ start, end } =
  if num == 0 then
    org
  else
    { start: { column: start.column + num, line: start.line }
    , end: { column: end.column + num, line: end.line }
    }

findCtor :: String -> Delimited (Name Proper) -> Maybe SourceRange
findCtor name ctors =
  Array.findMap
    ( \c ->
        if unwrap c.name == name then Just c.token.range
        else Nothing
    )
    (unwrap <$> delimitedToArray ctors)

getImportedRanges :: forall a. Module a -> Boolean -> String -> ModuleName -> Array Range
getImportedRanges (Module { header: ModuleHeader { imports } }) isType name moduleName =
  sourceRangeToRange <$> Array.foldMap go imports
  where
  go (ImportDecl { names, module: (Name { name: mn }) })
    | mn == moduleName = maybe [] goName names
    | otherwise = []
  goName (_ /\ delim) = Array.foldMap
    case _ of
      ImportValue (Name { token, name: Ident ident })
        | not isType && ident == name -> pure token.range

      ImportType (Name { token, name: Proper proper }) _
        | isType && proper == name -> pure token.range

      ImportType _ (Just (DataEnumerated ctors))
        | not isType -> maybe [] pure (findCtor name ctors)

      ImportOp (Name { token, name: Operator op })
        | not isType && op == name -> pure token.range

      ImportTypeOp _ (Name { token, name: Operator op })
        | isType && op == name -> pure token.range

      ImportClass _ (Name { token, name: Proper proper })
        | isType && name == proper -> pure token.range

      _ ->
        []
    (delimitedToArray' delim)

getDeclSignatureName :: forall a. Module a -> Boolean -> String -> Maybe Range
getDeclSignatureName (Module { body: ModuleBody { decls } }) isType name =
  sourceRangeToRange
    <$> Array.findMap
      case _ of
        DeclSignature (Labeled { label: Name { token, name: Ident ident } })
          | (ident == name) -> Just token.range

        DeclKindSignature _ (Labeled ({ label: Name { token, name: Proper proper } }))
          | (isType && proper == name) -> Just token.range
        _ ->
          Nothing
      decls
