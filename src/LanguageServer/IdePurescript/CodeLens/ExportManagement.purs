module LanguageServer.IdePurescript.CodeLens.ExportManagement
  ( exportManagementCodeLenses
  , printExports
  ) where

import Prelude
import Data.Array (intercalate, (:))
import Data.Array as Array
import Data.Foldable (fold, foldMap)
import Data.Generic.Rep (NoConstructors)
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Monoid (guard)
import Data.Newtype (class Newtype, un)
import Data.Nullable as Nullable
import Data.String (Pattern(..), stripSuffix)
import Data.String.CodeUnits as String
import Data.String.Utils as StringUtils
import Data.Tuple (snd)
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import LanguageServer.IdePurescript.Commands (replaceSuggestion)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.IdePurescript.Util.CST (sourcePosToPosition, sourceRangeToRange)
import LanguageServer.Protocol.Handlers (CodeLensResult)
import LanguageServer.Protocol.Types (Command, DocumentStore, DocumentUri, Settings)
import LanguageServer.Protocol.Types as LSP
import PureScript.CST (RecoveredParserResult(..))
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types as CST

exportManagementCodeLenses ∷ DocumentStore -> Settings -> ServerState -> DocumentUri -> Aff (Array CodeLensResult)
exportManagementCodeLenses _documentStore _settings (ServerState { parsedModules }) uri = do
  case _.parsed <$> Map.lookup uri parsedModules of
    Just (ParseSucceeded parsedModule) -> pure $ mkCodeLenses uri parsedModule
    Just (ParseSucceededWithErrors parsedModule _) -> pure $ mkCodeLenses uri parsedModule
    _ -> pure []

exportsToArray ∷ ∀ err. CST.Separated (CST.Export err) -> Array (CST.Export err)
exportsToArray (CST.Separated { head, tail }) = head : (snd <$> tail)

printExports ∷ Array String -> String
printExports exports = "\n  ( " <> (intercalate "\n  , " (Array.sort exports)) <> "\n  )\n  "

printExport ∷ ∀ e. CST.Export e -> Maybe String
printExport = case _ of
  CST.ExportOp (CST.Name { name: (CST.Operator name) }) -> Just $ "(" <> name <> ")"
  CST.ExportType (CST.Name { name: (CST.Proper name) }) members -> Just $ name <> guard (isJust members) "(..)"
  CST.ExportTypeOp _ (CST.Name { name: (CST.Operator name) }) -> Just $ "type (" <> name <> ")"
  CST.ExportClass _ (CST.Name { name: (CST.Proper name) }) -> Just $ "class " <> name
  CST.ExportKind _ (CST.Name { name: (CST.Proper name) }) -> Just $ name
  CST.ExportModule _ (CST.Name { name: CST.ModuleName name }) -> Just $ "module " <> name
  CST.ExportValue (CST.Name { name: CST.Ident name }) -> Just $ name
  CST.ExportError _ -> Nothing

data DeclConstructorInfo
  = WithConstructors
  | NoConstructors
derive instance Eq DeclConstructorInfo

newtype DeclNameInfo
  = DeclNameInfo { range :: LSP.Range, ctors :: DeclConstructorInfo, name :: String }

formatDeclName :: DeclNameInfo -> String
formatDeclName (DeclNameInfo { name, ctors }) = case ctors of
  NoConstructors -> name
  WithConstructors -> name <> "(..)"

instance Semigroup DeclNameInfo where
  append sr1 _ = sr1

derive instance newtypeSemigroupRange ∷ Newtype DeclNameInfo _

getDeclNameInfo ∷ ∀ a. CST.Module a -> SemigroupMap String DeclNameInfo
getDeclNameInfo =
  foldMapModule
    $ defaultMonoidalVisitor
        { onDecl =
          case _ of
            CST.DeclSignature (CST.Labeled { label: (CST.Name { token, name: CST.Ident name }) }) -> entry name token.range NoConstructors
            CST.DeclData { name: (CST.Name { token, name: CST.Proper name }) } _ -> entry name token.range WithConstructors
            CST.DeclNewtype { name: (CST.Name { token, name: CST.Proper name }) } _ _ _ -> entry name token.range WithConstructors
            CST.DeclType { name: (CST.Name { token, name: CST.Proper name }) } _ _ -> entry name token.range NoConstructors
            CST.DeclClass { name: (CST.Name { token, name: CST.Proper name }) } _ -> entry ("class " <> name) token.range NoConstructors --  Object.singleton  (SemigroupRange (sourceRangeToRange token.range))
            CST.DeclFixity { operator: CST.FixityValue _ _ (CST.Name { token, name: CST.Operator name }) } -> entry ("(" <> name <> ")") token.range NoConstructors -- Object.singleton ("(" <> name <> ")") (SemigroupRange (sourceRangeToRange token.range))
            CST.DeclFixity { operator: CST.FixityType _ _ _ (CST.Name { token, name: CST.Operator name }) } -> entry ("type (" <> name <> ")") token.range NoConstructors
            CST.DeclForeign _ _ (CST.ForeignValue (CST.Labeled { label: (CST.Name { token, name: CST.Ident name }) })) -> entry name token.range NoConstructors
            CST.DeclForeign _ _ (CST.ForeignData _ (CST.Labeled { label: (CST.Name { token, name: CST.Proper name }) })) -> entry name token.range NoConstructors
            CST.DeclForeign _ _ (CST.ForeignKind _ (CST.Name { token, name: CST.Proper name })) -> entry name token.range NoConstructors
            -- [TODO] Add type classes, type aliases, etc.
            _ -> SemigroupMap $ Map.empty
        }
  where
  entry name range ctors = SemigroupMap $ Map.singleton name $ DeclNameInfo { range: sourceRangeToRange range, ctors, name }

mkCodeLenses ∷ ∀ err. DocumentUri -> CST.Module err -> Array CodeLensResult
mkCodeLenses uri
  parsedModule@
    ( CST.Module
        { header:
            CST.ModuleHeader
              { name: CST.Name { token: { range: { end: moduleNameEnd } } }
              , exports
              , where: { range: { start: whereClauseStart } }
              , keyword
              }
        }
    ) =
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
    (SemigroupMap decls) = getDeclNameInfo parsedModule

    declToCodeLens info@(DeclNameInfo { name, ctors, range }) = do
      let
        replace title exps = Array.singleton $ mkCodeLensResult range $ replaceSuggestion title uri (printExports exps) exportsRange
        exported = Array.elem name exportNames
      if noExplicitExports then
        replace ("exported (export only this)") [ name ]
          <> replace ("(export everything else)")
              (Array.delete name $ Array.fromFoldable $ formatDeclName <$> Map.values decls)
      else if ctors == WithConstructors && Array.elem (withConstructors name) exportNames then
        let
          otherNames = exportNames # Array.delete (withConstructors name)
        in
          replace ("exported with constructors (export only type)") (name : otherNames)
            <> replace ("(remove from exports)") otherNames
      else if ctors == WithConstructors && exported then
        let
          otherNames = exportNames # Array.delete name
        in
          replace ("exported without constructors (export constructors)") (withConstructors name : otherNames)
            <> replace ("(remove from exports)") otherNames
      else if exported then
        replace ("exported (remove from exports)") (Array.delete name exportNames)
      else
        replace ("private (add to exports)") (exportNames `Array.snoc` formatDeclName info)

    moduleLens :: Maybe CodeLensResult
    moduleLens =
      if noExplicitExports then
        Just
          $ mkCodeLensResult (sourceRangeToRange keyword.range)
          $ replaceSuggestion "implicit module exports - make all explicit" uri
              (printExports $ Array.fromFoldable $ formatDeclName <$> Map.values decls) exportsRange
      else
        Nothing
    privatePublicCodeLenses ∷ Array CodeLensResult
    privatePublicCodeLenses = foldMap declToCodeLens $ Map.values decls
  in
    Array.fromFoldable moduleLens <> privatePublicCodeLenses
  where
  mkCodeLensResult ∷ LSP.Range -> Command -> CodeLensResult
  mkCodeLensResult codeLensRange replaceCommand = do
    { range: codeLensRange
    , command: Nullable.notNull replaceCommand
    , data: Nullable.null # unsafeToForeign
    }

withConstructors ∷ String -> String
withConstructors s = s <> "(..)"
