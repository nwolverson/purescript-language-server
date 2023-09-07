module LanguageServer.Protocol.Types where

import Prelude

import Data.Array (concat, groupBy, sortWith, (:))
import Data.Array.NonEmpty (toNonEmpty)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (class Newtype, over)
import Data.NonEmpty ((:|))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Semigroup.Foldable (foldl1)
import Data.Tuple (Tuple(..))
import Foreign (F, Foreign, readInt)
import Foreign.Index ((!))
import Foreign.Object (Object)
import Foreign.Object as Object
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (UndefinedOr)

foreign import data Connection :: Type
foreign import data DocumentStore :: Type

type MarkedString = { language :: String, value :: String }

markedString :: String -> MarkedString
markedString s = { language: "purescript", value: s }

type MarkupContent = { kind :: String, value :: String }

markupContent :: String -> MarkupContent
markupContent s = { kind: "markdown", value: s }

derive instance newtypeDocumentUri :: Newtype DocumentUri _

newtype DocumentUri = DocumentUri String

instance showDocumentUri :: Show DocumentUri where
  show (DocumentUri uri) = "DocumentUri " <> show uri

derive newtype instance eqDocumentUri :: Eq DocumentUri
derive newtype instance Ord DocumentUri

-- Line and character indexes, start from 0
newtype Position = Position { line :: Int, character :: Int }

instance eqPosition :: Eq Position where
  eq
    (Position { line, character })
    (Position { line: line', character: character' }) = line == line' &&
    character == character'

instance positionOrd :: Ord Position where
  compare
    (Position { line: line, character: character })
    (Position { line: line', character: character' })
    | line < line' = LT
    | line == line' && character < character' = LT
    | line == line' && character == character' = EQ
    | otherwise = GT

derive instance newtypePosition :: Newtype Position _

instance showPosition :: Show Position where
  show (Position { line, character }) = "Position(" <> show line <> ","
    <> show character
    <> ")"

newtype Range = Range { start :: Position, end :: Position }

instance eqRange :: Eq Range where
  eq (Range { start, end }) (Range { start: start', end: end' }) =
    start == start' && end == end'

instance ordRange :: Ord Range where
  compare (Range { start, end }) (Range { start: start', end: end' }) =
    compare start start' <> compare end end'

instance showRange :: Show Range where
  show (Range { start, end }) = "Range(" <> show start <> "," <> show end <> ")"

derive instance newtypeRange :: Newtype Range _

readRange :: Foreign -> F Range
readRange r = do
  start <- r ! "start" >>= readPosition
  end <- r ! "end" >>= readPosition
  pure $ Range { start, end }
  where
  readPosition p = do
    line <- p ! "line" >>= readInt
    character <- p ! "character" >>= readInt
    pure $ Position { line, character }

newtype Location = Location { uri :: DocumentUri, range :: Range }

derive instance newtypeLocation :: Newtype Location _

newtype LocationLink = LocationLink
  { originSelectionRange :: Nullable Range
  , targetUri :: DocumentUri
  , targetRange :: Range
  , targetSelectionRange :: Range
  }

foreign import data GotoDefinitionResult :: Type

gotoDefinitionResult :: Either Location LocationLink -> GotoDefinitionResult
gotoDefinitionResult = either unsafeCoerce unsafeCoerce

newtype Diagnostic = Diagnostic
  { range :: Range
  , severity :: Nullable Int -- 1 (Error) - 4 (Hint)
  , code :: Nullable String -- String | Int
  , source :: Nullable String
  , message :: String
  }

derive instance newtypeDiagnostic :: Newtype Diagnostic _
derive newtype instance showDiagnostic :: Show Diagnostic

newtype CompletionItemLabelDetails = CompletionItemLabelDetails
  { detail :: Nullable String
  , description :: Nullable String
  }

newtype CompletionItem = CompletionItem
  { label :: String
  , kind :: Nullable Int
  , detail :: Nullable String
  , labelDetails :: Nullable CompletionItemLabelDetails
  , documentation :: Nullable MarkupContent
  , sortText :: Nullable String
  , filterText :: Nullable String
  , insertText :: Nullable String
  , textEdit :: Nullable TextEdit
  , additionalTextEdits :: Nullable (Array TextEdit)
  , command :: Nullable Command
  }

derive instance newtypeCompletionItem :: Newtype CompletionItem _

data CompletionItemKind
  = Text
  | Method
  | Function
  | Constructor
  | Field
  | Variable
  | Class
  | Interface
  | Module
  | Property
  | Unit
  | Value
  | Enum
  | Keyword
  | Snippet
  | Color
  | File
  | Reference

defaultCompletionItem :: String -> CompletionItem
defaultCompletionItem label =
  CompletionItem
    { label
    , kind: toNullable Nothing
    , detail: toNullable Nothing
    , labelDetails: toNullable Nothing
    , documentation: toNullable Nothing
    , sortText: toNullable Nothing
    , filterText: toNullable Nothing
    , insertText: toNullable $ Just label
    , textEdit: toNullable Nothing
    , additionalTextEdits: toNullable Nothing
    , command: toNullable Nothing
    }

completionItem :: String -> CompletionItemKind -> CompletionItem
completionItem label k = defaultCompletionItem label # over CompletionItem
  (_ { kind = toNullable $ Just $ completionItemKindToInt k })

completionItemKindToInt :: CompletionItemKind -> Int
completionItemKindToInt = case _ of
  Text -> 1
  Method -> 2
  Function -> 3
  Constructor -> 4
  Field -> 5
  Variable -> 6
  Class -> 7
  Interface -> 8
  Module -> 9
  Property -> 10
  Unit -> 11
  Value -> 12
  Enum -> 13
  Keyword -> 14
  Snippet -> 15
  Color -> 16
  File -> 17
  Reference -> 18

newtype CompletionItemList = CompletionItemList
  { isIncomplete :: Boolean
  , items :: Array CompletionItem
  }

derive instance newtypeCompletionList :: Newtype CompletionItemList _

newtype SymbolInformation = SymbolInformation
  { name :: String
  , kind :: Int
  , location :: Location
  , containerName :: Nullable String
  }

data SymbolKind
  = FileSymbolKind
  | ModuleSymbolKind
  | NamespaceSymbolKind
  | PackageSymbolKind
  | ClassSymbolKind
  | MethodSymbolKind
  | PropertySymbolKind
  | FieldSymbolKind
  | ConstructorSymbolKind
  | EnumSymbolKind
  | InterfaceSymbolKind
  | FunctionSymbolKind
  | VariableSymbolKind
  | ConstantSymbolKind
  | StringSymbolKind
  | NumberSymbolKind
  | BooleanSymbolKind
  | ArraySymbolKind

symbolKindToInt :: SymbolKind -> Int
symbolKindToInt = case _ of
  FileSymbolKind -> 1
  ModuleSymbolKind -> 2
  NamespaceSymbolKind -> 3
  PackageSymbolKind -> 4
  ClassSymbolKind -> 5
  MethodSymbolKind -> 6
  PropertySymbolKind -> 7
  FieldSymbolKind -> 8
  ConstructorSymbolKind -> 9
  EnumSymbolKind -> 10
  InterfaceSymbolKind -> 11
  FunctionSymbolKind -> 12
  VariableSymbolKind -> 13
  ConstantSymbolKind -> 14
  StringSymbolKind -> 15
  NumberSymbolKind -> 16
  BooleanSymbolKind -> 17
  ArraySymbolKind -> 18

newtype Hover = Hover
  { contents :: MarkupContent
  , range :: UndefinedOr Range
  }

newtype Command = Command
  { title :: String, command :: String, arguments :: Nullable (Array Foreign) }

derive instance newtypeCommand :: Newtype Command _

newtype CodeAction = CodeAction
  { title :: String
  , kind :: CodeActionKind
  , isPreferred :: Boolean
  , edit :: Nullable WorkspaceEdit
  , command :: Nullable Command
  }

foreign import data CodeActionResult :: Type

codeActionResult :: Either CodeAction Command -> CodeActionResult
codeActionResult = either unsafeCoerce unsafeCoerce

newtype TextEdit = TextEdit { range :: Range, newText :: String }

derive instance newtypeTextEdit :: Newtype TextEdit _

instance eqTextEdit :: Eq TextEdit where
  eq
    (TextEdit { range, newText })
    (TextEdit { range: range', newText: newText' }) = range == range' && newText
    == newText'

instance showTextEdit :: Show TextEdit where
  show (TextEdit { range, newText }) =
    ("TextEdit(" <> show range <> ", " <> show newText <> ")")

newtype WorkspaceEdit = WorkspaceEdit
  { documentChanges :: Nullable (Array TextDocumentEdit)
  , changes :: Nullable (Object (Array TextEdit))
  }

instance semigroupWorkspaceEdit :: Semigroup WorkspaceEdit where
  append
    (WorkspaceEdit { documentChanges, changes })
    (WorkspaceEdit { documentChanges: documentChanges', changes: changes' }) =
    WorkspaceEdit
      { documentChanges:
          toNullable
            $
              case
                isNothing (toMaybe documentChanges),
                isNothing (toMaybe documentChanges')
                of
                true, true -> Nothing
                _, _ ->
                  Just
                    $ map (foldl1 combine)
                    $ map toNonEmpty
                    $ groupBy (\d1 d2 -> docId d1 == docId d2)
                        ( fromNullableArray documentChanges <> fromNullableArray
                            documentChanges'
                        )
      , changes:
          toNullable
            $ case isNothing (toMaybe changes), isNothing (toMaybe changes') of
                true, true -> Nothing
                _, _ ->
                  Just $ Object.fromFoldableWith (<>)
                    (goStrMap changes <> goStrMap changes')
      }
    where
    combine
      (TextDocumentEdit { textDocument, edits })
      (TextDocumentEdit { edits: edits' }) =
      TextDocumentEdit { textDocument, edits: edits <> edits' }
    docId (TextDocumentEdit { textDocument }) = textDocument

    fromNullableArray :: forall a. Nullable (Array a) -> Array a
    fromNullableArray a = fromMaybe [] $ toMaybe a

    goStrMap :: forall a. Nullable (Object a) -> Array (Tuple String a)
    goStrMap a = Object.toUnfoldable $ fromMaybe Object.empty $ toMaybe a

instance monoidWorkspaceEdit :: Monoid WorkspaceEdit where
  mempty = WorkspaceEdit
    { documentChanges: toNullable Nothing, changes: toNullable Nothing }

-- | Create workspace edit, supporting both documentChanges and older changes property for v2 clients
workspaceEdit ::
  Maybe ClientCapabilities -> Array TextDocumentEdit -> WorkspaceEdit
workspaceEdit capabilities edits =
  WorkspaceEdit
    { documentChanges:
        toNullable
          $ if useDocumentChanges then Just edits else Nothing
    , changes:
        toNullable
          $
            if useDocumentChanges then
              Nothing
            else
              Just
                $ Object.fromFoldable
                $ map
                    (\(h :| t) -> Tuple (uri h) (concat $ edit h : map edit t))
                $ map toNonEmpty
                $ groupBy (\a b -> uri a == uri b)
                $ sortWith uri edits
    }
  where
  useDocumentChanges = supportsDocumentChanges capabilities
  uri
    ( TextDocumentEdit
        { textDocument: OptionalVersionedTextDocumentIdentifier
            { uri: DocumentUri uri' }
        }
    ) = uri'
  edit (TextDocumentEdit { edits: edits' }) = edits'

supportsDocumentChanges :: Maybe ClientCapabilities -> Boolean
supportsDocumentChanges Nothing = false
supportsDocumentChanges (Just { workspace }) = fromMaybe false $
  toMaybe workspace >>= (_.workspaceEdit >>> toMaybe) >>=
    (_.documentChanges >>> toMaybe)

newtype TextDocumentEdit = TextDocumentEdit
  { textDocument :: OptionalVersionedTextDocumentIdentifier
  , edits :: Array TextEdit
  }

newtype TextDocumentIdentifier = TextDocumentIdentifier { uri :: DocumentUri }

derive instance Newtype TextDocumentIdentifier _
derive instance Eq TextDocumentIdentifier

newtype OptionalVersionedTextDocumentIdentifier =
  OptionalVersionedTextDocumentIdentifier
    { uri :: DocumentUri, version :: Nullable Number }

derive instance Newtype OptionalVersionedTextDocumentIdentifier _
derive instance Eq OptionalVersionedTextDocumentIdentifier

type Settings = Foreign

newtype FileChangeTypeCode = FileChangeTypeCode Int

data FileChangeType
  = CreatedChangeType
  | ChangedChangeType
  | DeletedChangeType

fileChangeTypeToInt :: FileChangeType -> Int
fileChangeTypeToInt = case _ of
  CreatedChangeType -> 1
  ChangedChangeType -> 2
  DeletedChangeType -> 3

intToFileChangeType :: Int -> Maybe FileChangeType
intToFileChangeType = case _ of
  1 -> Just CreatedChangeType
  2 -> Just ChangedChangeType
  3 -> Just DeletedChangeType
  _ -> Nothing

fromFileChangeTypeCode :: FileChangeTypeCode -> Maybe FileChangeType
fromFileChangeTypeCode = case _ of
  FileChangeTypeCode 1 -> Just CreatedChangeType
  FileChangeTypeCode 2 -> Just ChangedChangeType
  FileChangeTypeCode 3 -> Just DeletedChangeType
  _ -> Nothing

newtype FileEvent = FileEvent { uri :: DocumentUri, type :: FileChangeTypeCode }

newtype FoldingRange = FoldingRange
  { startLine :: Int
  , startCharacter :: Nullable Int
  , endLine :: Int
  , endCharacter :: Nullable Int
  , kind :: Nullable String -- | comment, imports, region
  }

type ClientCapabilities =
  { workspace :: Nullable WorkspaceClientCapabilities
  , textDocument :: Nullable TextDocumentClientCapabilities
  }

type WorkspaceClientCapabilities =
  { applyEdit :: Nullable Boolean
  , workspaceEdit :: Nullable WorkspaceEditClientCapabilities
  , codeLens :: Nullable CodeLensWorkspaceClientCapabilities
  }

type TextDocumentClientCapabilities =
  { codeAction :: Nullable CodeActionClientCapabilities
  , definition :: Nullable DefinitionClientCapabilities
  , codeLens :: Nullable CodeLensClientCapabilities
  }

type DefinitionClientCapabilities = { linkSupport :: Nullable Boolean }

type CodeActionClientCapabilities =
  { codeActionLiteralSupport ::
      Nullable { codeActionKind :: { valueSet :: Array CodeActionKind } }
  , isPreferredSupport :: Nullable Boolean
  }

type CodeLensClientCapabilities = { dynamicRegistration :: Nullable Boolean }

type CodeLensWorkspaceClientCapabilities =
  { refreshSupport :: Nullable Boolean }

newtype CodeActionKind = CodeActionKind String

instance showCodeActionKind :: Show CodeActionKind where
  show (CodeActionKind s) = "CodeActionKind " <> s

codeActionEmpty :: CodeActionKind
codeActionEmpty = CodeActionKind ""

codeActionQuickFix :: CodeActionKind
codeActionQuickFix = CodeActionKind "quickfix"

codeActionRefactor :: CodeActionKind
codeActionRefactor = CodeActionKind "refactor"

codeActionRefactorExtract :: CodeActionKind
codeActionRefactorExtract = CodeActionKind "refactor.extract"

codeActionRefactorInline :: CodeActionKind
codeActionRefactorInline = CodeActionKind "refactor.inline"

codeActionRefactorRewrite :: CodeActionKind
codeActionRefactorRewrite = CodeActionKind "refactor.rewrite"

codeActionSource :: CodeActionKind
codeActionSource = CodeActionKind "source"

-- | source.sortImports added in VSCode 1.57 for JS/TS
codeActionSourceSortImports :: CodeActionKind
codeActionSourceSortImports = CodeActionKind "source.sortImports"

codeActionSourceOrganizeImports :: CodeActionKind
codeActionSourceOrganizeImports = CodeActionKind "source.organizeImports"

-- https://microsoft.github.io/language-server-protocol/specifications/specification-current/#workspaceEditClientCapabilities
type WorkspaceEditClientCapabilities = { documentChanges :: Nullable Boolean }
