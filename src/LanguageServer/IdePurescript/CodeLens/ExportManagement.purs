module LanguageServer.IdePurescript.CodeLens.ExportManagement
  ( exportManagementCodeLenses
  , printExports
  )
where

import Prelude

import Data.Array (intercalate, (:))
import Data.Array as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (class Newtype, un)
import Data.Nullable as Nullable
import Data.String (Pattern(..), stripSuffix)
import Data.String.CodeUnits as String
import Data.String.Utils as StringUtils
import Data.Tuple (snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import LanguageServer.IdePurescript.Commands (replaceSuggestion)
import LanguageServer.IdePurescript.Formatting (mkTextEdit)
import LanguageServer.IdePurescript.Util.CST (sourcePosToPosition, sourceRangeToRange)
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (CodeLensResult)
import LanguageServer.Protocol.TextDocument (getText)
import LanguageServer.Protocol.Types (Command, DocumentStore, DocumentUri)
import LanguageServer.Protocol.Types as LSP
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types as CST

exportManagementCodeLenses ∷
  Maybe LSP.Connection ->
  DocumentStore ->
  DocumentUri -> Aff (Array CodeLensResult)
exportManagementCodeLenses maybeConnection documentStore uri = case maybeConnection of
  Nothing -> pure []
  Just connection -> ado
    addExportManagement <- addExportManagementCodeLenses connection documentStore uri
    in addExportManagement

exportToName ∷ ∀ err. CST.Export err -> Maybe String
exportToName = case _ of
  CST.ExportValue (CST.Name { name: CST.Ident name }) -> Just name
  _ -> Nothing

exportsToArray ∷ ∀ err. CST.Separated (CST.Export err) -> Array (CST.Export err)
exportsToArray (CST.Separated { head, tail }) = head : (snd <$> tail)

addExportManagementCodeLenses ∷ LSP.Connection -> DocumentStore -> DocumentUri -> Aff (Array CodeLensResult)
addExportManagementCodeLenses connection documentStore uri = do
  textDocument <- getDocument documentStore uri # liftEffect
  text <- getText textDocument # liftEffect
  case parseModule text of
    ParseSucceeded
      ( parsedModule@( CST.Module
          { header:
          CST.ModuleHeader
          { name: CST.Name { token: { range: { end: moduleNameEnd } } }
        , exports
        , where: { range: { start: whereClauseStart } }
        }
        }
      )
    ) -> pure $ mkCodeLenses uri parsedModule moduleNameEnd whereClauseStart exports
    ParseSucceededWithErrors
      ( parsedModule@( CST.Module
          { header:
          CST.ModuleHeader
          { name: CST.Name { token: { range: { end: moduleNameEnd } } }
        , exports
        , where: { range: { start: whereClauseStart } }
        }
        }
      )
    )
      _ -> pure $ mkCodeLenses uri parsedModule moduleNameEnd whereClauseStart exports
    _ -> pure []

printExports ∷ Array String -> String
printExports exports = "\n  ( " <> (intercalate "\n  , " (Array.sort exports)) <> "\n  )\n  "

printExport ∷ ∀ e. CST.Export e -> Maybe String
printExport = case _ of
  CST.ExportOp (CST.Name { name: (CST.Operator name) }) -> Just $ "(" <> name <> ")"
  CST.ExportType (CST.Name { name: (CST.Proper name) }) _ -> Just $ name
  CST.ExportTypeOp _ (CST.Name { name: (CST.Operator name) }) -> Just $ "type (" <> name <> ")"
  CST.ExportClass _ (CST.Name { name: (CST.Proper name) }) -> Just $ "class " <> name
  CST.ExportKind _ (CST.Name { name: (CST.Proper name) }) -> Just $ name
  CST.ExportModule _ (CST.Name { name: CST.ModuleName name }) -> Just $ "module " <> name
  CST.ExportValue (CST.Name { name: CST.Ident name }) -> Just $ name
  CST.ExportError e -> Nothing

newtype SemigroupRange = SemigroupRange LSP.Range

instance semigroupSemigroupRange ∷ Semigroup SemigroupRange where
  append sr1 _ = sr1

derive instance newtypeSemigroupRange ∷ Newtype SemigroupRange _

getSemigroupRanges ∷ ∀ a. CST.Module a -> Object SemigroupRange
getSemigroupRanges =
  foldMapModule
    $ defaultMonoidalVisitor
        { onDecl =
          case _ of
            CST.DeclSignature (CST.Labeled { label: (CST.Name { token, name: CST.Ident name }) }) -> Object.singleton name (SemigroupRange (sourceRangeToRange token.range))
            CST.DeclData { name: (CST.Name { token, name: CST.Proper name }) } _ -> Object.singleton name (SemigroupRange (sourceRangeToRange token.range))
            CST.DeclNewtype { name: (CST.Name { token, name: CST.Proper name }) } _ _ _ -> Object.singleton name (SemigroupRange (sourceRangeToRange token.range))
            CST.DeclType { name: (CST.Name { token, name: CST.Proper name }) } _ _ -> Object.singleton name (SemigroupRange (sourceRangeToRange token.range))
            CST.DeclClass { name: (CST.Name { token, name: CST.Proper name }) } _ -> Object.singleton ("class " <> name) (SemigroupRange (sourceRangeToRange token.range))
            CST.DeclFixity { operator: CST.FixityValue _ _ (CST.Name { token, name: CST.Operator name }) } -> Object.singleton ("(" <> name <> ")") (SemigroupRange (sourceRangeToRange token.range))
            CST.DeclFixity { operator: CST.FixityType _ _ _ (CST.Name { token, name: CST.Operator name }) } -> Object.singleton ("type (" <> name <> ")") (SemigroupRange (sourceRangeToRange token.range))
            CST.DeclForeign _ _ (CST.ForeignValue (CST.Labeled { label: (CST.Name { token, name: CST.Ident name }) })) -> Object.singleton name (SemigroupRange (sourceRangeToRange token.range))
            CST.DeclForeign _ _ (CST.ForeignData _ (CST.Labeled { label: (CST.Name { token, name: CST.Proper name }) })) -> Object.singleton name (SemigroupRange (sourceRangeToRange token.range))
            CST.DeclForeign _ _ (CST.ForeignKind _ (CST.Name { token, name: CST.Proper name })) -> Object.singleton name (SemigroupRange (sourceRangeToRange token.range))
            -- CST.DeclClass (CST.Labeled { label: (CST.Name { token, name: CST.Ident name }) }) -> Object.singleton name (SemigroupRange (sourceRangeToRange token.range))
            -- [TODO] Add type classes, type aliases, etc.
            _ -> Object.empty
        }

mkCodeLenses ∷
  ∀ err.
  DocumentUri ->
  CST.Module err ->
  CST.SourcePos ->
  CST.SourcePos ->
  Maybe (CST.Wrapped (CST.Separated (CST.Export err))) ->
  (Array CodeLensResult)
mkCodeLenses uri parsedModule moduleNameEnd whereClauseStart exports =
  let
    start = sourcePosToPosition moduleNameEnd
    end = sourcePosToPosition whereClauseStart
    exportsRange ∷ LSP.Range
    exportsRange = LSP.Range { start, end }
    noExplicitExports = isNothing exports
    exportArray = case exports of
      Nothing -> []
      Just
        ( CST.Wrapped
          { value: separatedExports
        }
      ) -> exportsToArray separatedExports
    exportNames = Array.mapMaybe printExport exportArray
    decls = getSemigroupRanges parsedModule <#> un SemigroupRange
    declToCodeLens name range = do
      let codeLensRange = range -- shiftRangeUp 1 range
      let replace title exps = Array.singleton $ mkCodeLensResult codeLensRange $ replaceSuggestion title uri (printExports exps) exportsRange
      let exportedWithConstructors x = Array.elem (withConstructors x) exportNames
      let exportedWithoutConstructors x = StringUtils.endsWith "(..)" x && Array.elem (String.dropRight 4 x <> "(..)") exportNames
      -- [TODO] This isn't quite right yet
      if noExplicitExports then
        replace ("exported (export only " <> name <> ")") [name] <>
          replace ("(export everything but " <> name <> ")") (decls # Object.keys # Array.delete name)
      else
        if exportedWithConstructors name then
          replace ("exported as " <> withConstructors name <> " (export only type)") (exportNames # Array.delete (withConstructors name) # Array.cons name)
        else
          if exportedWithoutConstructors name then
            replace ("exported as " <> withoutConstructors name <> " (export with constructors)") (exportNames # Array.delete (withoutConstructors name) # Array.cons name)
          else
            if Array.elem name exportNames then
              replace ("exported (remove " <> name <> " from exports)") (exportNames # Array.delete name)
            else
              replace ("private (add " <> name <> " to exports)") (Array.snoc exportNames name)

    privatePublicCodeLenses ∷ Array CodeLensResult
    privatePublicCodeLenses = decls # Object.mapWithKey declToCodeLens # fold
  in
    privatePublicCodeLenses
  where
  mkCodeLensResult ∷ LSP.Range -> Command -> CodeLensResult
  mkCodeLensResult codeLensRange replaceCommand = do
    { range: codeLensRange
    , command: Nullable.notNull replaceCommand
    , data: Nullable.null # unsafeToForeign
    }

withConstructors ∷ String -> String
withConstructors s = s <> "(..)"

withoutConstructors ∷ String -> String
withoutConstructors s = fromMaybe s $ stripSuffix (Pattern "(..)") s
