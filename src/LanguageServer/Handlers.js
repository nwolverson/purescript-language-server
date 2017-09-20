"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var vscode_jsonrpc_1 = require("vscode-jsonrpc");
var registerHandler = function (registerF) {
    return function (f) { return function () { return registerF(function (x) { return f(x)(); }); }; };
};
var registerHandler0 = function (registerF) {
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
exports.onCodeAction = function (conn) { return registerHandler(conn.onCodeAction); };
exports.onDidChangeConfiguration = function (conn) { return registerNotificationHandler(conn.onDidChangeConfiguration); };
exports.publishDiagnostics = function (conn) { return function (params) { return function () { return conn.sendDiagnostics(params); }; }; };
exports.applyEdit = function (conn) { return function (edit) { return function () { return conn.workspace.applyEdit(edit); }; }; };
exports.sendDiagnosticsBegin = function (conn) { return function () { return conn.sendNotification(new vscode_jsonrpc_1.NotificationType0('textDocument/diagnosticsBegin')); }; };
exports.sendDiagnosticsEnd = function (conn) { return function () { return conn.sendNotification(new vscode_jsonrpc_1.NotificationType0('textDocument/diagnosticsEnd')); }; };
exports.onExecuteCommand = function (conn) { return registerHandler(conn.onExecuteCommand); };
exports.onDidChangeWatchedFiles = function (conn) { return registerNotificationHandler(conn.onDidChangeWatchedFiles); };
exports.onExit = function (conn) { return registerNotificationHandler0(conn.onExit); };
exports.onShutdown = function (conn) { return registerHandler0(conn.onShutdown); };
