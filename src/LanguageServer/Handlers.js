"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.onShutdown = exports.onExit = exports.onDidChangeWatchedFiles = exports.onExecuteCommand = exports.sendCleanEnd = exports.sendCleanBegin = exports.sendDiagnosticsEnd = exports.sendDiagnosticsBegin = exports.applyEditImpl = exports.publishDiagnostics = exports.onDidChangeConfiguration = exports.onDocumentFormatting = exports.onFoldingRanges = exports.onCodeAction = exports.onReferences = exports.onWorkspaceSymbol = exports.onDocumentSymbol = exports.onHover = exports.onCompletion = exports.onDefinition = exports.registerHandler0 = void 0;
var vscode_jsonrpc_1 = require("vscode-jsonrpc");
var registerHandler = function (registerF) {
    return function (f) { return function () { return registerF(function (x) { return f(x)(); }); }; };
};
// For some reason this is getting deleted by DCE even though it is used and the same form as the others...
exports.registerHandler0 = function (registerF) {
    return function (f) { return function () { return registerF(f); }; };
};
var registerNotificationHandler = function (registerF) {
    return function (f) { return function () { return registerF(function (x) { return f(x)(); }); }; };
};
var registerNotificationHandler0 = function (registerF) {
    return function (f) { return function () { return registerF(f); }; };
};
exports.onDefinition = function (conn) { return registerHandler(conn.onDefinition); };
exports.onCompletion = function (conn) { return registerHandler(conn.onCompletion); };
exports.onHover = function (conn) { return registerHandler(conn.onHover); };
exports.onDocumentSymbol = function (conn) { return registerHandler(conn.onDocumentSymbol); };
exports.onWorkspaceSymbol = function (conn) { return registerHandler(conn.onWorkspaceSymbol); };
exports.onReferences = function (conn) { return registerHandler(conn.onReferences); };
exports.onCodeAction = function (conn) { return registerHandler(conn.onCodeAction); };
exports.onFoldingRanges = function (conn) { return registerHandler(conn.onFoldingRanges); };
exports.onDocumentFormatting = function (conn) { return registerHandler(conn.onDocumentFormatting); };
exports.onDidChangeConfiguration = function (conn) { return registerNotificationHandler(conn.onDidChangeConfiguration); };
exports.publishDiagnostics = function (conn) { return function (params) { return function () { return conn.sendDiagnostics(params); }; }; };
exports.applyEditImpl = function (conn) { return function (edit) { return function () { return conn.workspace.applyEdit(edit).then(function (x) { return x.applied; }); }; }; };
exports.sendDiagnosticsBegin = function (conn) { return function () { return conn.sendNotification(new vscode_jsonrpc_1.NotificationType0('textDocument/diagnosticsBegin')); }; };
exports.sendDiagnosticsEnd = function (conn) { return function () { return conn.sendNotification(new vscode_jsonrpc_1.NotificationType0('textDocument/diagnosticsEnd')); }; };
exports.sendCleanBegin = function (conn) { return function () { return conn.sendNotification(new vscode_jsonrpc_1.NotificationType0('textDocument/cleanBegin')); }; };
exports.sendCleanEnd = function (conn) { return function () { return conn.sendNotification(new vscode_jsonrpc_1.NotificationType0('textDocument/cleanEnd')); }; };
exports.onExecuteCommand = function (conn) { return registerHandler(conn.onExecuteCommand); };
exports.onDidChangeWatchedFiles = function (conn) { return registerNotificationHandler(conn.onDidChangeWatchedFiles); };
exports.onExit = function (conn) { return registerNotificationHandler0(conn.onExit); };
exports.onShutdown = function (conn) { return exports.registerHandler0(conn.onShutdown); };
