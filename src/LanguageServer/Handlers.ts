import { RequestHandler, RequestHandler0, NotificationHandler, NotificationHandler0, IConnection, createConnection, IPCMessageReader, IPCMessageWriter, TextDocuments,Location,  DefinitionRequest, TextDocumentPositionParams, CompletionItem, Hover, DocumentSymbolParams, PublishDiagnosticsParams, WorkspaceEdit } from 'vscode-languageserver';
import { NotificationType0 } from 'vscode-jsonrpc';

let registerHandler = <T1,T2>(registerF: (handler: RequestHandler<T1, T2, void>) => void) =>
    (f: (args: T1) => () => T2) => () => registerF(x => f(x)());

let registerHandler0 = <T>(registerF: (handler: RequestHandler0<T, void>) => void) =>
    (f: () => T) => () => registerF(f);

let registerNotificationHandler = <T>(registerF: (handler: NotificationHandler<T>) => void) =>
    (f: (args: T) => () => void) => () => registerF(x => f(x)());

let registerNotificationHandler0 = <T>(registerF: (handler: NotificationHandler0) => void) =>
    (f: () => void) => () => registerF(f);

export const onDefinition = (conn: IConnection) => registerHandler(conn.onDefinition);

export const onCompletion = (conn: IConnection) => registerHandler(conn.onCompletion);

export const onHover = (conn: IConnection) => registerHandler(conn.onHover);

export const onDocumentSymbol = (conn: IConnection) => registerHandler(conn.onDocumentSymbol);

export const onWorkspaceSymbol = (conn: IConnection) => registerHandler(conn.onWorkspaceSymbol);

export const onCodeAction = (conn: IConnection) => registerHandler(conn.onCodeAction);

export const onDidChangeConfiguration = (conn: IConnection) => registerNotificationHandler(conn.onDidChangeConfiguration);

export const publishDiagnostics = (conn: IConnection) => (params: PublishDiagnosticsParams) => () => conn.sendDiagnostics(params);

export const applyEdit = (conn: IConnection) => (edit: WorkspaceEdit) => () => conn.workspace.applyEdit(edit);

export const sendDiagnosticsBegin = (conn: IConnection) => () => conn.sendNotification(new NotificationType0('textDocument/diagnosticsBegin'));

export const sendDiagnosticsEnd = (conn: IConnection) => () => conn.sendNotification(new NotificationType0('textDocument/diagnosticsEnd'));

export const onExecuteCommand = (conn: IConnection) => registerHandler(conn.onExecuteCommand);

export const onDidChangeWatchedFiles = (conn: IConnection) => registerNotificationHandler(conn.onDidChangeWatchedFiles);

export const onExit = (conn: IConnection) => registerNotificationHandler0(conn.onExit);

export const onShutdown = (conn: IConnection) => registerHandler0(conn.onShutdown);

