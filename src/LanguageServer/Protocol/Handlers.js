"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.onShutdown = exports.onExit = exports.onDidChangeWatchedFiles = exports.onExecuteCommand = exports.sendCleanEnd = exports.sendCleanBegin = exports.sendDiagnosticsEnd = exports.sendDiagnosticsBegin = exports.applyEditImpl = exports.publishDiagnostics = exports.onDidChangeConfiguration = exports.onDocumentFormatting = exports.onFoldingRanges = exports.onCodeLens = exports.onCodeAction = exports.onReferences = exports.onWorkspaceSymbol = exports.onDocumentSymbol = exports.onHover = exports.onCompletion = exports.onDefinition = exports.registerHandler0 = void 0;
var vscode_jsonrpc_1 = require("vscode-jsonrpc");
var registerHandler = function (registerF) {
    return function (f) {
        return function () {
            return registerF(function (x) { return f(x)(); });
        };
    };
};
// For some reason this is getting deleted by DCE even though it is used and the same form as the others...
var registerHandler0 = function (registerF) {
    return function (f) {
        return function () {
            return registerF(f);
        };
    };
};
exports.registerHandler0 = registerHandler0;
var registerNotificationHandler = function (registerF) {
    return function (f) {
        return function () {
            return registerF(function (x) { return f(x)(); });
        };
    };
};
var registerNotificationHandler0 = function (registerF) {
    return function (f) {
        return function () {
            return registerF(f);
        };
    };
};
var onDefinition = function (conn) {
    return registerHandler(conn.onDefinition);
};
exports.onDefinition = onDefinition;
var onCompletion = function (conn) {
    return registerHandler(conn.onCompletion);
};
exports.onCompletion = onCompletion;
var onHover = function (conn) { return registerHandler(conn.onHover); };
exports.onHover = onHover;
var onDocumentSymbol = function (conn) {
    return registerHandler(conn.onDocumentSymbol);
};
exports.onDocumentSymbol = onDocumentSymbol;
var onWorkspaceSymbol = function (conn) {
    return registerHandler(conn.onWorkspaceSymbol);
};
exports.onWorkspaceSymbol = onWorkspaceSymbol;
var onReferences = function (conn) {
    return registerHandler(conn.onReferences);
};
exports.onReferences = onReferences;
var onCodeAction = function (conn) {
    return registerHandler(conn.onCodeAction);
};
exports.onCodeAction = onCodeAction;
var onCodeLens = function (conn) {
    return registerHandler(conn.onCodeLens);
};
exports.onCodeLens = onCodeLens;
var onFoldingRanges = function (conn) {
    return registerHandler(conn.onFoldingRanges);
};
exports.onFoldingRanges = onFoldingRanges;
var onDocumentFormatting = function (conn) {
    return registerHandler(conn.onDocumentFormatting);
};
exports.onDocumentFormatting = onDocumentFormatting;
var onDidChangeConfiguration = function (conn) {
    return registerNotificationHandler(conn.onDidChangeConfiguration);
};
exports.onDidChangeConfiguration = onDidChangeConfiguration;
var publishDiagnostics = function (conn) { return function (params) { return function () {
    return conn.sendDiagnostics(params);
}; }; };
exports.publishDiagnostics = publishDiagnostics;
var applyEditImpl = function (conn) { return function (edit) { return function () {
    return conn.workspace.applyEdit(edit).then(function (x) { return x.applied; });
}; }; };
exports.applyEditImpl = applyEditImpl;
var sendDiagnosticsBegin = function (conn) { return function () {
    return conn.sendNotification(new vscode_jsonrpc_1.NotificationType0("textDocument/diagnosticsBegin"));
}; };
exports.sendDiagnosticsBegin = sendDiagnosticsBegin;
var sendDiagnosticsEnd = function (conn) { return function () {
    return conn.sendNotification(new vscode_jsonrpc_1.NotificationType0("textDocument/diagnosticsEnd"));
}; };
exports.sendDiagnosticsEnd = sendDiagnosticsEnd;
var sendCleanBegin = function (conn) { return function () {
    return conn.sendNotification(new vscode_jsonrpc_1.NotificationType0("textDocument/cleanBegin"));
}; };
exports.sendCleanBegin = sendCleanBegin;
var sendCleanEnd = function (conn) { return function () {
    return conn.sendNotification(new vscode_jsonrpc_1.NotificationType0("textDocument/cleanEnd"));
}; };
exports.sendCleanEnd = sendCleanEnd;
var onExecuteCommand = function (conn) {
    return registerHandler(conn.onExecuteCommand);
};
exports.onExecuteCommand = onExecuteCommand;
var onDidChangeWatchedFiles = function (conn) {
    return registerNotificationHandler(conn.onDidChangeWatchedFiles);
};
exports.onDidChangeWatchedFiles = onDidChangeWatchedFiles;
var onExit = function (conn) {
    return registerNotificationHandler0(conn.onExit);
};
exports.onExit = onExit;
var onShutdown = function (conn) {
    return (0, exports.registerHandler0)(conn.onShutdown);
};
exports.onShutdown = onShutdown;
