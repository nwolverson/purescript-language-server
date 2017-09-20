module LanguageServer.Handlers where
  
import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Control.Promise (Promise)
import Data.Foreign (Foreign)
import Data.Nullable (Nullable)
import LanguageServer.Types (CONN, Command, CompletionItemList, Connection, Diagnostic, DocumentUri, FileEvent, Hover, Location, Position, Range, SymbolInformation, TextDocumentIdentifier, WorkspaceEdit)

type TextDocumentPositionParams = { textDocument :: TextDocumentIdentifier, position :: Position }

type DocumentSymbolParams = { textDocument :: TextDocumentIdentifier }
type WorkspaceSymbolParams = { query :: String }

type CodeActionParams = { textDocument :: TextDocumentIdentifier, range :: Range, context :: CodeActionContext }
type CodeActionContext = { diagnostics :: Array Diagnostic }

type DidChangeConfigurationParams = { settings :: Foreign }

type PublishDiagnosticParams = { uri :: DocumentUri, diagnostics :: Array Diagnostic }
type ExecuteCommandParams = { command :: String, arguments :: Array Foreign }

type DidChangeWatchedFilesParams = { changes :: Array FileEvent }

type Res eff a = Eff (conn :: CONN | eff) (Promise a)

foreign import onDefinition :: forall eff. Connection -> (TextDocumentPositionParams -> Res eff (Nullable Location)) -> Eff (conn :: CONN | eff) Unit

foreign import onCompletion :: forall eff. Connection -> (TextDocumentPositionParams -> Res eff CompletionItemList) -> Eff (conn :: CONN | eff) Unit

foreign import onHover :: forall eff. Connection -> (TextDocumentPositionParams -> Res eff (Nullable Hover)) -> Eff (conn :: CONN | eff) Unit

foreign import onDocumentSymbol :: forall eff. Connection -> (DocumentSymbolParams -> Res eff (Array SymbolInformation)) -> Eff (conn :: CONN | eff) Unit

foreign import onWorkspaceSymbol :: forall eff. Connection -> (WorkspaceSymbolParams -> Res eff (Array SymbolInformation)) -> Eff (conn :: CONN | eff) Unit

foreign import onCodeAction :: forall eff. Connection -> (CodeActionParams -> Res eff (Array Command)) -> Eff (conn :: CONN | eff) Unit

foreign import onDidChangeConfiguration :: forall eff. Connection -> (DidChangeConfigurationParams -> Eff (conn :: CONN | eff) Unit) -> Eff (conn :: CONN | eff) Unit

foreign import onDidChangeWatchedFiles ::  forall eff. Connection -> (DidChangeWatchedFilesParams -> Eff (conn :: CONN | eff) Unit) -> Eff (conn :: CONN | eff) Unit

foreign import onExecuteCommand :: forall eff. Connection -> (ExecuteCommandParams -> Eff (conn :: CONN | eff) (Promise Foreign)) -> Eff (conn :: CONN | eff) Unit

foreign import publishDiagnostics :: forall eff. Connection -> PublishDiagnosticParams -> Eff (conn :: CONN | eff) Unit

foreign import applyEdit :: forall eff. Connection -> WorkspaceEdit -> Eff (conn :: CONN | eff) Unit

foreign import sendDiagnosticsBegin :: forall eff. Connection -> Eff (conn :: CONN | eff) Unit

foreign import sendDiagnosticsEnd :: forall eff. Connection -> Eff (conn :: CONN | eff) Unit

foreign import onExit :: forall eff. Connection -> (Eff (conn :: CONN | eff) Unit) -> Eff (conn :: CONN | eff) Unit

foreign import onShutdown :: forall eff. Connection -> (Res eff Unit) -> Eff (conn :: CONN | eff) Unit
