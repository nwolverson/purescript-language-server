module IdePurescript.Modules
  ( Module(..)
  , initialModulesState
  , State
  , getMainModule
  , getModuleName
  , getModulesForFile
  , getModulesForFileTemp
  , getUnqualActiveModules
  , getAllActiveModules
  , getQualModule
  , getModuleFromUnknownQualifier
  , findImportInsertPos
  , addModuleImport
  , addExplicitImport
  , addQualifiedImport
  , ImportResult(..)
  , reformatModuleImports
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Array (concatMap, filter, findLastIndex, intercalate, singleton, (:))
import Data.Array as Array
import Data.Either (either, Either(..))
import Data.Foldable (all, any, notElem, elem)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.String (Pattern(Pattern), split)
import Data.String as String
import Data.String.Regex (regex) as R
import Data.String.Regex.Flags (global, noFlags, multiline) as R
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import IdePurescript.Regex (replace', match', test')
import IdePurescript.Tokens (startsWithCapitalLetter)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (sep)
import PscIde as P
import PscIde.Command (ImportType(..))
import PscIde.Command as C

newtype Module
  = Module
  { moduleName :: String
  , importType :: C.ImportType
  , qualifier :: Maybe String
  }

derive instance moduleNewtype :: Newtype Module _

instance moduleEq :: Eq Module where
  eq (Module m1) (Module m2) =
    m1.moduleName
      == m2.moduleName
      && m1.qualifier
      == m2.qualifier
      && m1.importType `eqImportType` m2.importType

eqImportType :: C.ImportType -> C.ImportType -> Boolean
eqImportType Implicit Implicit = true
eqImportType (Explicit idents) (Explicit idents') = idents == idents'
eqImportType (Hiding idents) (Hiding idents') = idents == idents'
eqImportType _ _ = false

getModuleName :: Module -> String
getModuleName (Module { moduleName }) = moduleName

type State
  = { main :: Maybe String
    , modules :: Array Module
    , identifiers :: Array String
    , identToModule :: Object.Object Module
    }

type Path
  = String

getMainModule :: String -> Maybe String
getMainModule text =
  case match' regex text of
    Just [ _, Just m ] -> Just m
    _ -> Nothing
  where
  regex = R.regex """module\s+([\w.]+)""" $ R.multiline

getModulesForFile :: Int -> Path -> String -> Aff State
getModulesForFile port file fullText = do
  C.ImportList { moduleName, imports } <- either (const default) identity <$> P.listImports port file
  let
    modules = map mod imports
    main = maybe (getMainModule fullText) Just moduleName
    identToModule = Object.fromFoldable $ concatMap idents modules
    identifiers = Object.keys identToModule
  pure { main, modules, identifiers, identToModule }
  where
  default = C.ImportList { moduleName: Nothing, imports: [] }
  mod (C.Import imp) = Module imp
  idents m@(Module { importType: Explicit ids }) = flip Tuple m <$> ids
  idents _ = []

getModulesForFileTemp :: Int -> Path -> String -> Aff State
getModulesForFileTemp port file fullText = do
  tmpFile <- makeTempFile file fullText
  res <- getModulesForFile port tmpFile fullText
  _ <- attempt $ FS.unlink tmpFile
  pure res

mkImplicit :: String -> Module
mkImplicit m = Module { qualifier: Nothing, importType: Implicit, moduleName: m }

getUnqualActiveModules :: State -> Maybe String -> Array String
getUnqualActiveModules state ident = getModules include state
  where
  include (Module { qualifier: Just _ }) = false
  include (Module { importType: Explicit idents }) =
    maybe false
      ( \x -> x `elem` idents
          || ("(" <> x <> ")") `elem` idents
          || -- Include there's a constructor import like `Maybe(..)`
            ( startsWithCapitalLetter x
                -- Unfortunately PSC-IDE strips the (..) part in the "identifier"
                -- so for now we have to include any module with an identifier
                -- that starts with a capital letter, ideally we only included
                -- constructor imports of the form `Whatever(..)`
                && any startsWithCapitalLetter idents
            )
      )
      ident
  include (Module { importType: Implicit }) = true
  include (Module { importType: Hiding idents }) = maybe true (_ `notElem` idents) ident

getAllActiveModules :: State -> Array String
getAllActiveModules = getModules (const true)

getModules :: (Module -> Boolean) -> State -> Array String
getModules include { modules, main } =
  ([ "Prim" ] <> _) $ map getModuleName $ maybe [] (singleton <<< mkImplicit) main <> filter include modules

getQualModule :: String -> State -> Array String
getQualModule qualifier { modules } =
  map getModuleName $ filter (qual qualifier) modules
  where
  qual q (Module { qualifier: Just q' }) = q == q'
  qual _ _ = false

getModuleFromUnknownQualifier :: String -> State -> Maybe Module
getModuleFromUnknownQualifier qual { identToModule } =
  Object.lookup qual identToModule <|> Object.lookup ("class " <> qual) identToModule

initialModulesState :: State
initialModulesState = { main: Nothing, modules: [], identifiers: [], identToModule: Object.empty }

findImportInsertPos :: String -> Int
findImportInsertPos text =
  let
    regex = R.regex """^(module|import) [A-Z][^(]*($|\([^()]*\))""" R.noFlags
    lines = split (Pattern "\n") text
    res = fromMaybe 0 $ findLastIndex (test' regex) lines
  in
    res + 1

foreign import tmpDir :: Effect String

data ImportResult
  = UpdatedImports String
  | AmbiguousImport (Array C.TypeInfo)
  | UnnecessaryImport
  | FailedImport String

makeTempFile :: Path -> String -> Aff Path
makeTempFile fileName text = do
  dir <- liftEffect tmpDir
  uuid <- liftEffect genUUID
  let
    name = replace' (R.regex "[\\/\\\\:]" R.global) "-" fileName
    tmpFile = dir <> sep <> "ide-purescript-" <> show uuid <> "-" <> name
  FS.writeTextFile UTF8 tmpFile text
  pure tmpFile

withTempFile ::
  String ->
  String ->
  (String -> Aff (Either String C.ImportResult)) ->
  Aff ImportResult
withTempFile fileName text action = do
  tmpFile <- makeTempFile fileName text
  res <- action tmpFile
  answer <- case res of
    Right (C.SuccessFile _) -> UpdatedImports <$> FS.readTextFile UTF8 tmpFile
    Right (C.MultipleResults a) -> pure $ AmbiguousImport a
    Right (C.SuccessText st) -> pure (FailedImport (intercalate "\n" st))
    Left err -> pure (FailedImport err)
  _ <- attempt $ FS.unlink tmpFile
  pure answer

addModuleImport :: State -> Int -> String -> String -> String -> Aff { state :: State, result :: ImportResult }
addModuleImport state port fileName text moduleName =
  case shouldAdd of
    false -> pure { state, result: UnnecessaryImport }
    true -> do
      result <- withTempFile fileName text addImport
      pure { state, result }
  where
  addImport tmpFile = P.implicitImport port tmpFile (Just tmpFile) [] moduleName
  shouldAdd =
    state.main /= Just moduleName && (mkImplicit moduleName `notElem` state.modules)

addExplicitImport ::
  State ->
  Int ->
  String ->
  String ->
  Maybe String ->
  Maybe String ->
  String ->
  Maybe C.Namespace ->
  Aff { state :: State, result :: ImportResult }
addExplicitImport state port fileName text moduleName qualifier identifier ns =
  case shouldAdd of
    false -> pure { state, result: UnnecessaryImport }
    true -> do
      result <- withTempFile fileName text addImport
      let
        state' = case result of
          UpdatedImports _ -> state { identifiers = identifier : state.identifiers }
          _ -> state
      pure { result, state: state' }
  where
  addImport tmpFile = P.explicitImport port tmpFile (Just tmpFile) (filters <> namespaceFilters) identifier qualifier
  filters = maybe [] (\m -> [ C.ModuleFilter [ m ] ]) moduleName
  namespaceFilters = maybe [] (\n -> [ C.NamespaceFilter [ n ] ]) ns
  isThisModule = case moduleName of
    Just _ -> moduleName == state.main
    _ -> false

  isOpenPrim = moduleName == Just "Prim" && not (any isExplicitPrim state.modules)
  isExplicitPrim (Module { moduleName: "Prim", importType }) = case importType of
    Implicit -> false
    _ -> true
  isExplicitPrim _ = false

  shouldAdd =
    not isThisModule
      && not isOpenPrim
      -- This check did not validate identifier namespace - so always let purs ide decide
      -- && not (identifier `elem` state.identifiers)
      && maybe true (\mn -> all (shouldAddMatch mn) state.modules) moduleName

  shouldAddMatch mn (Module { moduleName: moduleName', qualifier: Nothing, importType: Implicit })
    | moduleName' == mn = false
  shouldAddMatch mn (Module { moduleName: moduleName', qualifier: Nothing, importType: Hiding idents })
    | moduleName' == mn = identifier `elem` idents
  shouldAddMatch _ _ = true

addQualifiedImport ::
  State ->
  Int ->
  String ->
  String ->
  String ->
  String ->
  Aff { state :: State, result :: ImportResult }
addQualifiedImport state port fileName text moduleName qualifier =
  if not isThisModule then
    { state, result: _ } <$> withTempFile fileName text addImport
  else
    pure { state, result: UnnecessaryImport }
  where
  addImport tmpFile = P.qualifiedImport port tmpFile (Just tmpFile) moduleName qualifier
  isThisModule = Just moduleName == state.main

reformatModuleImports ::
  Notify ->
  State ->
  Int ->
  String ->
  String ->
  Aff (Maybe { state :: State, result :: String })
reformatModuleImports log state port fileName text = do
  res <- withTempFile fileName text addBogusImport
  case res of
    UpdatedImports result -> do
      let
        result' =
          intercalate "\n"
            $ Array.filter (not <<< String.contains (Pattern qualifier))
            $ lines result
      liftEffect $ log Info (show $ lines result')
      pure $ Just { state, result: result' }
    _ -> pure Nothing
  where
  qualifier = "__IDE_IMPORT_HACK"
  addBogusImport tmpFile = P.qualifiedImport port tmpFile (Just tmpFile) "Prim" qualifier
