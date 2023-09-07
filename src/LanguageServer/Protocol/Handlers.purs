module LanguageServer.Protocol.Handlers where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import LanguageServer.Protocol.Types (CodeActionKind(..), CodeActionResult, Command, CompletionItemList, Connection, Diagnostic, DocumentUri, FileEvent, FoldingRange, GotoDefinitionResult, Hover, Location, Position, Range, SymbolInformation, TextDocumentIdentifier, TextEdit, WorkspaceEdit)

type TextDocumentPositionParams =
  { textDocument :: TextDocumentIdentifier, position :: Position }

type DocumentSymbolParams = { textDocument :: TextDocumentIdentifier }

type ReferenceParams = TextDocumentPositionParams

type WorkspaceSymbolParams = { query :: String }

type CodeActionParams =
  { textDocument :: TextDocumentIdentifier
  , range :: Range
  , context :: CodeActionContext
  }

type CodeActionContext =
  { diagnostics :: Array Diagnostic, only :: Nullable (Array CodeActionKind) }

type CodeLensParams = { textDocument :: TextDocumentIdentifier }

type CodeLensResult =
  { range :: Range
  , command :: Nullable Command
  , data :: Foreign
  }

type FoldingRangesParams = { textDocument :: TextDocumentIdentifier }

type DocumentFormattingParams =
  { textDocument :: TextDocumentIdentifier
  }

type PrepareRenameParams =
  { textDocument :: TextDocumentIdentifier
  , position :: Position
  }

type RenameParams =
  { textDocument :: TextDocumentIdentifier
  , position :: Position
  , newName :: String
  }

type DidChangeConfigurationParams = { settings :: Foreign }

type PublishDiagnosticParams =
  { uri :: DocumentUri, diagnostics :: Array Diagnostic }

type ExecuteCommandParams = { command :: String, arguments :: Array Foreign }

type DidChangeWatchedFilesParams = { changes :: Array FileEvent }

type Res a = Effect (Promise a)

foreign import onDefinition ::
  Connection ->
  (TextDocumentPositionParams -> Res (Nullable GotoDefinitionResult)) ->
  Effect Unit

foreign import onCompletion ::
  Connection ->
  (TextDocumentPositionParams -> Res CompletionItemList) ->
  Effect Unit

foreign import onHover ::
  Connection ->
  (TextDocumentPositionParams -> Res (Nullable Hover)) ->
  Effect Unit

foreign import onDocumentSymbol ::
  Connection ->
  (DocumentSymbolParams -> Res (Array SymbolInformation)) ->
  Effect Unit

foreign import onWorkspaceSymbol ::
  Connection ->
  (WorkspaceSymbolParams -> Res (Array SymbolInformation)) ->
  Effect Unit

foreign import onReferences ::
  Connection -> (ReferenceParams -> Res (Array Location)) -> Effect Unit

foreign import onCodeAction ::
  Connection ->
  (CodeActionParams -> Res (Array CodeActionResult)) ->
  Effect Unit

foreign import onCodeLens ::
  Connection -> (CodeLensParams -> Res (Array CodeLensResult)) -> Effect Unit

foreign import onFoldingRanges ::
  Connection -> (FoldingRangesParams -> Res (Array FoldingRange)) -> Effect Unit

foreign import onDocumentFormatting ::
  Connection ->
  (DocumentFormattingParams -> Res (Array TextEdit)) ->
  Effect Unit

foreign import onPrepareRename ::
  Connection -> (PrepareRenameParams -> Res (Nullable Range)) -> Effect Unit

foreign import onRenameRequest ::
  Connection -> (RenameParams -> Res WorkspaceEdit) -> Effect Unit

foreign import onDidChangeConfiguration ::
  Connection -> (DidChangeConfigurationParams -> Effect Unit) -> Effect Unit

foreign import onDidChangeWatchedFiles ::
  Connection -> (DidChangeWatchedFilesParams -> Effect Unit) -> Effect Unit

foreign import onExecuteCommand ::
  Connection ->
  (ExecuteCommandParams -> Effect (Promise Foreign)) ->
  Effect Unit

foreign import publishDiagnostics ::
  Connection -> PublishDiagnosticParams -> Effect Unit

foreign import applyEditImpl :: Connection -> WorkspaceEdit -> Res Boolean

applyEdit :: Connection -> WorkspaceEdit -> Aff Boolean
applyEdit conn edit = Promise.toAffE $ applyEditImpl conn edit

foreign import sendDiagnosticsBegin :: Connection -> Effect Unit

foreign import sendDiagnosticsEnd :: Connection -> Effect Unit

foreign import sendCleanBegin :: Connection -> Effect Unit

foreign import sendCleanEnd :: Connection -> Effect Unit

foreign import onExit :: Connection -> (Effect Unit) -> Effect Unit

foreign import onShutdown :: Connection -> (Res Unit) -> Effect Unit
