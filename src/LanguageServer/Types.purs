module LanguageServer.Types where

import Prelude
import Control.Monad.Eff (kind Effect)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Nullable (Nullable, toNullable)

foreign import data CONN :: Effect
foreign import data Connection :: Type
foreign import data DocumentStore :: Type

type MarkedString = { language :: String, value :: String }

markedString :: String -> MarkedString
markedString s = { language: "purescript", value: s }

derive instance newtypeDocumentUri :: Newtype DocumentUri _

newtype DocumentUri = DocumentUri String

derive newtype instance eqDocumentUri :: Eq DocumentUri

newtype Position = Position { line :: Int, character :: Int }

instance eqPosition :: Eq Position where
  eq (Position { line, character }) (Position { line: line', character: character' }) =
    line == line' && character == character'

instance positionOrd :: Ord Position where
  compare (Position { line: line, character: character }) (Position { line: line', character: character' })
    | line < line' = LT
    | line == line' && character < character' = LT
    | line == line' && character == character' = EQ
    | otherwise = GT

derive instance newtypePosition :: Newtype Position _

instance showPosition :: Show Position where
  show (Position { line, character }) = "Position(" <> show line <> "," <> show character <> ")"

newtype Range = Range { start :: Position, end :: Position }

instance showRange :: Show Range where
  show (Range { start, end }) = "Range(" <> show start <> "," <> show end <> ")"

derive instance newtypeRange :: Newtype Range _

newtype Location = Location { uri :: DocumentUri, range :: Range }

derive instance newtypeLocation :: Newtype Location _

newtype Diagnostic = Diagnostic
    { range :: Range
    , severity :: Nullable Int -- 1 (Error) - 4 (Hint)
    , code :: Nullable String -- String | Int
    , source :: Nullable String
    , message :: String
    }
derive instance newtypeDiagnostic :: Newtype Diagnostic _

newtype CompletionItem = CompletionItem
    { label :: String
    , kind :: Nullable Int
    , detail :: Nullable String
    , documentation :: Nullable String
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
defaultCompletionItem label = CompletionItem
    { label
    , kind: toNullable Nothing
    , detail: toNullable Nothing
    , documentation: toNullable Nothing
    , sortText: toNullable Nothing
    , filterText: toNullable Nothing
    , insertText: toNullable Nothing
    , textEdit: toNullable Nothing
    , additionalTextEdits: toNullable Nothing
    , command: toNullable Nothing
    }

completionItem :: String -> CompletionItemKind -> CompletionItem
completionItem label k = defaultCompletionItem label # over CompletionItem ( _ { kind = toNullable $ Just $ completionItemKindToInt k } )

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
 
newtype Hover = Hover { contents :: MarkedString, range :: Nullable Range }

newtype Command = Command { title :: String, command :: String, arguments :: Nullable (Array Foreign) }

derive instance newtypeCommand :: Newtype Command _

newtype TextEdit = TextEdit { range :: Range, newText :: String }

newtype WorkspaceEdit = WorkspaceEdit { documentChanges :: Nullable (Array TextDocumentEdit) }

workspaceEdit :: Array TextDocumentEdit -> WorkspaceEdit
workspaceEdit edits = WorkspaceEdit { documentChanges: toNullable $ Just edits }

newtype TextDocumentEdit = TextDocumentEdit { textDocument :: TextDocumentIdentifier, edits :: Array TextEdit }

newtype TextDocumentIdentifier = TextDocumentIdentifier { uri :: DocumentUri, version :: Number }

derive instance newtypeTextDocumentIdentifier :: Newtype TextDocumentIdentifier _

type Settings = Foreign
newtype FileChangeTypeCode = FileChangeTypeCode Int

data FileChangeType = CreatedChangeType | ChangedChangeType | DeletedChangeType

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
  
newtype FileEvent = FileEvent { uri :: DocumentUri, type :: FileChangeTypeCode }