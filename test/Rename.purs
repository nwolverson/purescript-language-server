module Test.Rename (main) where

import Prelude

import Control.Apply (lift2)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign (unsafeToForeign)
import IdePurescript.Modules as Modules
import IdePurescript.PscIdeServer (Notify, Port)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.IdePurescript.Rename as R
import LanguageServer.IdePurescript.Server as PursIde
import LanguageServer.Protocol.Types (Position(..), Range(..))
import LanguageServer.Protocol.Uri (filenameToUri)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Process as Process
import PscIde as PscIde
import PscIde.Command (TypePosition(..))
import PureScript.CST (parseModule)
import Test.Spec (it, itOnly)
import Test.Spec as S
import Test.Spec.Assertions as A
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

emptyNotify :: Notify
emptyNotify _ _ =
  pure unit

newtype ModuleName = ModuleName String
newtype ModulePath = ModulePath String
newtype ModuleText = ModuleText String

derive instance Newtype ModuleName _
derive instance Eq ModuleName
derive instance Ord ModuleName

derive instance Newtype ModulePath _
derive instance Newtype ModuleText _

newtype LinePattern = LinePattern String
newtype IdentPattern = IdentPattern String

newtype Ident = Ident String
newtype LineIdx = LineIdx Int

{-
Text position types involved in tests:

LS.Protocol.Types:
  starts from 0
  Position = Position { line :: Int, character :: Int }
  Range = Range { start :: Position, end :: Position }

PscIde:
  Position = { line :: Int, column :: Int } -- starts from 1
  TypePosition =
    TypePosition { name :: String, start :: Position, end :: Position }
                  ^ name is module file path

IdePurescript.Tokens:
  WordRange = { left :: Int, right :: Int }
-}

getTypePos ::
  ModulePath -> ModuleText -> LinePattern -> Pattern -> Maybe TypePosition
getTypePos modulePath (ModuleText text) (LinePattern linePtn) identPtn = do
  line <- Array.findIndex (String.contains (Pattern linePtn)) lines
  lineText <- Array.index lines line
  character <-
    -- First search inside the line pattern then in the line text.
    case String.indexOf identPtn linePtn, String.indexOf (Pattern linePtn) lineText of
      Just idx, Just linePtnIdx -> Just $ idx + linePtnIdx
      _, _ -> String.indexOf identPtn lineText
  pure $ TypePosition
    { name: unwrap modulePath
    , start: { column: character + 1, line: line + 1 }
    -- The end column position is a position of the last character.
    , end: { column: character + identLen + 1, line: line + 1 }
    }
  where
  lines = String.split (Pattern "\n") text
  identLen = String.length $ unwrap identPtn

getIdentTypePos ::
  ModulePath ->
  ModuleText ->
  LinePattern ->
  IdentPattern ->
  Maybe { typePos :: TypePosition, ident :: Ident, qualifier :: Maybe String }
getIdentTypePos
  (ModulePath modulePath)
  (ModuleText text)
  (LinePattern linePtn)
  (IdentPattern ident) = do

  lineIdx <- Array.findIndex (String.contains (Pattern linePtn)) lines
  lineText <- Array.index lines lineIdx
  linePtnIdx <- String.indexOf (Pattern linePtn) lineText
  -- First search inside the line pattern then in the line text.
  character <- case String.indexOf (Pattern ident) linePtn of
    Just idx -> Just (linePtnIdx + idx)
    _ -> String.indexOf (Pattern ident) lineText

  let position = Position { character, line: lineIdx }
  { word, qualifier, range } <-
    identifierAtPoint lineText character
  pure
    { typePos: mkTypePosition modulePath $ mkWordRange position range Nothing
    , ident: Ident word
    , qualifier: qualifier
    }
  where
  lines = String.split (Pattern "\n") text

  posToTypePos (Position { character, line }) =
    { column: character + 1, line: line + 1 } -- Differs by 1.
  mkWordRange pos { left, right } qualifier =
    Range
      { start: pos # over Position mkStart
      , end: pos # over Position (\c -> c { character = right })
      }
    where
    mkStart = _
      { character = left
          -- Adjust start if qualified.
          + (maybe 0 (\s -> String.length s + 1) qualifier)
      }
  mkTypePosition filePath (Range { start, end }) =
    TypePosition
      { name: filePath
      , start: posToTypePos start
      , end: posToTypePos end
      }

rangeFromTp :: TypePosition -> Range
rangeFromTp (TypePosition { start, end }) =
  (Range { start: toR start, end: toR end })
  where
  toR { column, line } = Position
    { character: column - 1
    , line: line - 1
    }

splitLines :: String -> Array String
splitLines =
  String.split (Pattern "\n")

fileToModuleName :: String -> String
fileToModuleName filePath = fromMaybe filePath do
  name <- Array.last $ String.split (Pattern "test/") filePath
  pure $ name
    # String.replace (Pattern ".purs") (Replacement "")
    # String.replaceAll (Pattern ("/")) (Replacement ".")
    # String.replaceAll (Pattern ("\"")) (Replacement "")

renderRange :: Range -> String
renderRange (Range { start, end }) =
  renderPos start <> "-" <> renderPos end
  where
  renderPos (Position { character, line }) =
    show (line + 1) <> ":" <> show (character + 1)

editsToCompare :: R.TextEditsMap -> Array (String /\ Array (String /\ String))
editsToCompare edits =
  Map.toUnfoldable edits <#>
    \((uri /\ _) /\ ranges) ->
      (fileToModuleName $ show uri)
        /\ (Array.sort $ ranges <#> map renderRange <<< lift2 (/\) _.newText _.range)

-- | Coverts found type position to ["Module.Name" /\ ["1:10-1:15"]]
tpsToCompare ::
  Array (String /\ TypePosition) -> Array (String /\ Array (String /\ String))
tpsToCompare positions =
  Map.toUnfoldable $ gather
    $ positions <#> \(searchText /\ tp@(TypePosition { name })) ->
        fileToModuleName name /\ (searchText /\ (renderRange $ rangeFromTp tp))
  where
  addRange range = pure <<< maybe [ range ] (Array.sort <<< flip (<>) [ range ])
  gather = Array.foldl
    (\m (key /\ range) -> Map.alter (addRange range) key m)
    Map.empty

type ModulePrep =
  { state :: Modules.State
  , name :: ModuleName
  , path :: ModulePath
  , text :: ModuleText
  }

type PrepareResult =
  { port :: Port
  , modules :: Map ModuleName ModulePrep
  , docsToEdit :: Map String R.DocToEdit
  }

-- | Prepare tests. Starts purs ide server, rebuilds test modules.
prepare :: Aff PrepareResult
prepare = do
  cwdDir <- liftEffect $ Process.cwd
  let rootPath = cwdDir <> "/test"

  let settings = unsafeToForeign {} --{ purescript: {"pscIdelogLevel": "debug"} }
  startRes <- PursIde.startServer' settings rootPath notify notify

  case startRes of
    { port: Just port } -> do
      rebuild moduleA
      rebuild moduleB

      modules <- prepareModules
      docsToEdit <- getDocsToEdit

      pure { port, modules, docsToEdit }

      where
      moduleA = ModuleName "Test.Fixture.RenameA"
      moduleB = ModuleName "Test.Fixture.RenameB"

      allModules = [ moduleA, moduleB ]

      prepareModules = Map.fromFoldable <$>
        traverse
          ( \mn -> do
              prepared <- prepareModule mn
              pure $ mn /\ prepared
          )
          allModules

      prepareModule mName = do
        text <- readModuleText mName
        state <- getModuleState mName text
        pure { text, state, name: mName, path: toPath mName }

      getDocsToEdit = Map.fromFoldable <$>
        traverse
          ( \path -> do
              uri <- liftEffect $ filenameToUri path
              docText <- FS.readTextFile (UTF8) path
              let docLines = splitLines docText
              pure $ path /\
                { uri
                , docTextAtRange:
                    \range ->
                      R.getTextAtRangeInLines docLines range
                , version: Nothing
                , parsed: parseModule docText
                }
          )
          (toPathStr <$> allModules)

      readModuleText moduleN =
        ModuleText <$> FS.readTextFile UTF8 (unwrap $ toPath moduleN)

      getModuleState moduleN (ModuleText moduleText) =
        Modules.getModulesForFileTemp port mPath moduleText
        where
        (ModulePath mPath) = toPath moduleN

      rebuild moduleN = do
        buildRes <-
          PscIde.rebuild port mPath (Just mPath) Nothing
        case buildRes of
          Left err ->
            A.fail $ "Module rebuild error: " <> err
          Right _ ->
            pure unit
        where
        (ModulePath mPath) = toPath moduleN

      toPathStr = unwrap <<< toPath

      toPath (ModuleName m) =
        ModulePath $
          rootPath <> "/" <>
            (String.replaceAll (Pattern ".") (Replacement "/") m)
            # String.replace (Pattern "Test/") (Replacement "")
            # flip (<>) ".purs"

    _ ->
      liftEffect $ throw "Could not start ide server"
  where
  --notify _ str = Console.log str
  notify = emptyNotify


-- | Test Rename.getTextEdits function.
renameSpec :: PrepareResult -> S.Spec Unit
renameSpec prep = do
  S.before (pure prep) $
    --S.beforeAll prepare $
    S.describe "Find usages for rename refactor" do

      let
        expectedFunc =
          [ moduleA /\ "func1 ::" /\ "func1" -- def
          , moduleA /\ "func1 int" /\ "func1"
          , moduleA /\ "( func1" /\ "func1" -- export
          , moduleA /\ "= func1 10" /\ "func1"
          , moduleB /\ "import" /\ "func1" -- import
          , moduleB /\ "nc1, func1" /\ "func1" -- duplicate import
          , moduleB /\ "= A.func1 0" /\ "func1"
          , moduleB /\ "func1 v" /\ "func1" -- inside instance
          ]

      testRename it "value ident - in def"
        (moduleA /\ "func1 int" /\ "func1")
        expectedFunc

      testRename it "value ident - in signature"
        (moduleA /\ "func1 :" /\ "func1")
        expectedFunc

      testRename it "value ident - in usage"
        (moduleA /\ "func1 10" /\ "func1")
        expectedFunc

      testRename itOnly "name with quote"
        (moduleA /\ "func' =" /\ "func'")
        [ moduleA /\ "func' ::" /\ "func'"]

      let
        expectedTypeSyn =
          [ moduleA /\ "type TypeSyn =" /\ "TypeSyn" -- def
          , moduleA /\ "type TypeSyn ::" /\ "TypeSyn" -- kind def
          , moduleA /\ ", TypeSyn" /\ "TypeSyn" --export
          , moduleA /\ "func1 ::" /\ "TypeSyn"
          , moduleB /\ "import" /\ "TypeSyn"
          , moduleB /\ "TypeB TypeSyn" /\ "TypeSyn"
          , moduleB /\ "a -> TypeSyn" /\ "TypeSyn"
          ]

      testRename it "type synonym - in type def"
        (moduleA /\ "type TypeSyn =" /\ "TypeSyn")
        expectedTypeSyn

      testRename it "type synonym - in kind signature"
        (moduleA /\ "type TypeSyn :" /\ "TypeSyn")
        expectedTypeSyn

      testRename it "type synonym - in usage"
        (moduleA /\ "Int -> TypeSyn" /\ "TypeSyn")
        expectedTypeSyn

      testRename it "type synonym - in type def"
        (moduleA /\ "type TypeSyn =" /\ "TypeSyn")
        expectedTypeSyn

      testRename it "data type"
        (moduleA /\ "data DataType :" /\ "DataType")
        [ moduleA /\ "data DataType :" /\ "DataType" -- def kind
        , moduleA /\ "data DataType =" /\ "DataType" -- def
        , moduleA /\ "DataType(" /\ "DataType" -- export
        , moduleB /\ "import" /\ "DataType"
        , moduleB /\ "dt :: A.DataType" /\ "DataType"
        , moduleB /\ "dt2 :: DataType" /\ "DataType"
        ]

      testRename it "data constructor"
        (moduleB /\ "= ACons 0" /\ "ACons")
        [ moduleA /\ "= ACons Int" /\ "ACons" -- def
        , moduleA /\ "DataType(ACons" /\ "ACons" -- export
        , moduleB /\ "import" /\ "ACons"
        , moduleB /\ "dt = A.ACons 0" /\ "ACons"
        , moduleB /\ "dt2 = ACons 0" /\ "ACons"
        ]

      let
        expectedNewt =
          [ moduleA /\ "newtype Newt ::" /\ "Newt" -- def kind
          , moduleA /\ "newtype Newt =" /\ "Newt" -- def
          , moduleA /\ "Newt(Newt" /\ "Newt" -- export
          , moduleA /\ "newT :: Newt" /\ "Newt"
          , moduleA /\ "fNewT :: Newt" /\ "Newt" -- foreign import
          , moduleB /\ "Newt(Newt" /\ "Newt" --import
          , moduleB /\ "newT ::" /\ "Newt"
          ]

      testRename it "newtype type - in def"
        (moduleA /\ "newtype Newt =" /\ "Newt")
        expectedNewt

      testRename it "newtype type - in kind signature"
        (moduleA /\ "newtype Newt :" /\ "Newt")
        expectedNewt

      testRename it "newtype type - in usage"
        (moduleB /\ "newT :" /\ "Newt")
        expectedNewt

      let
        expectedConsNewt =
          [ moduleA /\ "= Newt Int" /\ "Newt" -- def
          , moduleA /\ "Newt 10" /\ "Newt"
          , moduleA /\ "ewt(Newt" /\ "Newt" -- export
          , moduleB /\ "ewt(Newt" /\ "Newt" -- import
          , moduleB /\ "newT =" /\ "Newt" -- import
          ]

      testRename it "newtype constructor - in def"
        (moduleA /\ "= Newt Int" /\ "Newt")
        expectedConsNewt

      testRename it "newtype constructor - in usage"
        (moduleA /\ "= Newt 10" /\ "Newt")
        expectedConsNewt

      let
        expectedTc =
          [ moduleA /\ "class ClsA a" /\ "ClsA" --def
          , moduleA /\ ", class ClsA" /\ "ClsA" -- export
          , moduleB /\ "class ClsA" /\ "ClsA" -- import
          , moduleB /\ "instance ClsA" /\ "ClsA"
          , moduleB /\ "ClsA a =>" /\ "ClsA"
          ]

      testRename it "type class - in def"
        (moduleA /\ "class ClsA" /\ "ClsA")
        expectedTc

      testRename it "type class - in usage"
        (moduleB /\ "ClsA a =>" /\ "ClsA")
        expectedTc

      -- Note: renaming of members in type class instances is not implemented,
      -- as we can not get their locations from purs ide (because to declare
      -- instances one doesn't need to import them explicitly, it would need
      -- ad-hoc logic for this case).
      --
      -- But it would rename usages in expressions. And manual renaming in
      -- instances will be required.
      --
      -- In future it is possible to implement it on LSP side using CST parsing:
      -- will need to find usages of the type class, filter its usages in
      -- instances, then find target member declarations in instances.
      testRename it "type class member"
        (moduleA /\ "toInt ::" /\ "toInt")
        [ moduleA /\ "toInt ::" /\ "toInt" --def
        , moduleA /\ ", toInt" /\ "toInt" -- export
        , moduleB /\ "import" /\ "toInt" -- import
        , moduleB /\ "= toInt" /\ "toInt" -- usage
        -- finding instance member declaration - doesn't work
        -- , moduleB /\ "toInt (TypeB v)" /\ "toInt"
        ]

      -- TODO: tests for value fixity, type fixity (value/ctor), value op, type op, kind

      let
        expectedValueOp =
          [ moduleA /\ "5 Tup as" /\ "/\\" --def
          , moduleA /\ ", (/\\)" /\ "/\\" -- export
          , moduleB /\ "import" /\ "/\\" -- import
          , moduleB /\ "tup =" /\ "/\\" -- usage
          ]

      testRename it "value operator"
        (moduleA /\ "5 Tup as" /\ "/\\")
        expectedValueOp

      testRename it "value operator (in other module)"
        (moduleB /\ "tup = 1" /\ "/\\")
        expectedValueOp

      let
        expectedTypeOp =
          [ moduleA /\ "5 type Tup as" /\ "/\\" --def
          , moduleA /\ ", type (/\\)" /\ "/\\" -- export
          , moduleB /\ "type (/\\)" /\ "/\\" -- import
          , moduleB /\ "tup ::" /\ "/\\" -- usage
          ]

      testRename it "type operator"
        (moduleA /\ "5 type Tup as" /\ "/\\")
        expectedTypeOp

      testRename it "type operator"
        (moduleA /\ "5 type Tup as" /\ "/\\")
        expectedTypeOp

  -- TODO: support renaming names in imports/exports

  -- TODO: there is case when ident exists in import but not used in the code
  -- (when purs ide provides imports)

  where
  moduleA = ModuleName "Test.Fixture.RenameA"
  moduleB = ModuleName "Test.Fixture.RenameB"
  testRename fn title (mn /\ linePtn /\ identPtn) expected =
    fn testName \{ port, modules, docsToEdit } ->
      let
        res = do
          module_ <- Map.lookup mn modules
          pos <- getIdentTypePos module_.path module_.text
            (LinePattern linePtn)
            (IdentPattern identPtn)
          pure $ module_ /\ pos
      in
        case res of
          Just (modA /\ { typePos, ident: (Ident ident), qualifier }) -> do
            mbUsage <-
              R.getTypeInfoWithUsages port ident typePos qualifier modA.state
            case mbUsage of
              -- Returns usages: array of type positions.
              Just (typeInfo /\ usages) -> do
                let
                  edits =
                    R.getTextEdits typeInfo usages docsToEdit (newName ident) identPtn

                case makeTps modules ident of
                  Right tps ->
                    editsToCompare edits `A.shouldEqual` tpsToCompare tps

                  Left ((ModuleName mn') /\ lPtn /\ iPtn) ->
                    A.fail $ "Could not find expected usage " <> show (mn' /\ lPtn /\ iPtn)
              _ ->
                A.fail "No usages found"
            pure unit

          _ ->
            A.fail $ "Could not find identifier " <> identPtn
    where
    testName = ("Finds replacements for " <> title <> " (" <> identPtn <> ")")
    newName ident = ident <> "X"

    makeTps modules ident = flip traverse expected
      \input@(mn' /\ lPtn /\ iPtn) -> note input do
        { text, path } <- Map.lookup mn' modules
        (/\) (replaceNewName ident iPtn)
          <$> (getTypePos path text (LinePattern lPtn) (Pattern ident))

    replaceNewName ident iPtn =

      (String.replace (Pattern ident) (Replacement (newName ident)) iPtn)

main :: Effect Unit
main = launchAff_ do
  prepare >>=  runSpec [ consoleReporter ] <<< renameSpec
