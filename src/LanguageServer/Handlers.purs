module LanguageServer.Handlers where
  
import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import LanguageServer.Types (Command, CompletionItemList, Connection, Diagnostic, DocumentUri, FileEvent, Hover, Location, Position, Range, SymbolInformation, TextDocumentIdentifier, WorkspaceEdit)

type TextDocumentPositionParams = { textDocument :: TextDocumentIdentifier, position :: Position }

type DocumentSymbolParams = { textDocument :: TextDocumentIdentifier }
type ReferenceParams = TextDocumentPositionParams
type WorkspaceSymbolParams = { query :: String }

type CodeActionParams = { textDocument :: TextDocumentIdentifier, range :: Range, context :: CodeActionContext }
type CodeActionContext = { diagnostics :: Array Diagnostic }

type DidChangeConfigurationParams = { settings :: Foreign }

type PublishDiagnosticParams = { uri :: DocumentUri, diagnostics :: Array Diagnostic }
type ExecuteCommandParams = { command :: String, arguments :: Array Foreign }

type DidChangeWatchedFilesParams = { changes :: Array FileEvent }

type Res a = Effect (Promise a)

foreign import onDefinition :: Connection -> (TextDocumentPositionParams -> Res (Nullable Location)) -> Effect Unit

foreign import onCompletion :: Connection -> (TextDocumentPositionParams -> Res CompletionItemList) -> Effect Unit

foreign import onHover :: Connection -> (TextDocumentPositionParams -> Res (Nullable Hover)) -> Effect Unit

foreign import onDocumentSymbol :: Connection -> (DocumentSymbolParams -> Res (Array SymbolInformation)) -> Effect Unit

foreign import onWorkspaceSymbol :: Connection -> (WorkspaceSymbolParams -> Res (Array SymbolInformation)) -> Effect Unit

foreign import onReferences :: Connection -> (ReferenceParams -> Res (Array Location)) -> Effect Unit

foreign import onCodeAction :: Connection -> (CodeActionParams -> Res (Array Command)) -> Effect Unit

foreign import onDidChangeConfiguration :: Connection -> (DidChangeConfigurationParams -> Effect Unit) -> Effect Unit

foreign import onDidChangeWatchedFiles ::  Connection -> (DidChangeWatchedFilesParams -> Effect Unit) -> Effect Unit

foreign import onExecuteCommand :: Connection -> (ExecuteCommandParams -> Effect (Promise Foreign)) -> Effect Unit

foreign import publishDiagnostics :: Connection -> PublishDiagnosticParams -> Effect Unit

foreign import applyEditImpl :: Connection -> WorkspaceEdit -> Res Boolean

applyEdit :: Connection -> WorkspaceEdit -> Aff Boolean
applyEdit conn edit = Promise.toAffE $ applyEditImpl conn edit

foreign import sendDiagnosticsBegin :: Connection -> Effect Unit

foreign import sendDiagnosticsEnd :: Connection -> Effect Unit

foreign import onExit :: Connection -> (Effect Unit) -> Effect Unit

foreign import onShutdown :: Connection -> (Res Unit) -> Effect Unit
