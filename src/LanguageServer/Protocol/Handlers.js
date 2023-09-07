import { NotificationType0 } from "vscode-jsonrpc";
var registerHandler = function (registerF) {
    return function (f) {
        return function () {
            return registerF(function (x) { return f(x)(); });
        };
    };
};
var registerHandler0 = function (registerF) {
    return function (f) {
        return function () {
            return registerF(f);
        };
    };
};
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
export var onDefinition = function (conn) {
    return registerHandler(conn.onDefinition);
};
export var onCompletion = function (conn) {
    return registerHandler(conn.onCompletion);
};
export var onHover = function (conn) { return registerHandler(conn.onHover); };
export var onDocumentSymbol = function (conn) {
    return registerHandler(conn.onDocumentSymbol);
};
export var onWorkspaceSymbol = function (conn) {
    return registerHandler(conn.onWorkspaceSymbol);
};
export var onReferences = function (conn) {
    return registerHandler(conn.onReferences);
};
export var onCodeAction = function (conn) {
    return registerHandler(conn.onCodeAction);
};
export var onCodeLens = function (conn) {
    return registerHandler(conn.onCodeLens);
};
export var onFoldingRanges = function (conn) {
    return registerHandler(conn.onFoldingRanges);
};
export var onDocumentFormatting = function (conn) {
    return registerHandler(conn.onDocumentFormatting);
};
export var onPrepareRename = function (conn) {
    return registerHandler(conn.onPrepareRename);
};
export var onRenameRequest = function (conn) {
    return registerHandler(conn.onRenameRequest);
};
export var onDidChangeConfiguration = function (conn) {
    return registerNotificationHandler(conn.onDidChangeConfiguration);
};
export var publishDiagnostics = function (conn) { return function (params) { return function () {
    return conn.sendDiagnostics(params);
}; }; };
export var applyEditImpl = function (conn) { return function (edit) { return function () {
    return conn.workspace.applyEdit(edit).then(function (x) { return x.applied; });
}; }; };
export var sendDiagnosticsBegin = function (conn) { return function () {
    return conn.sendNotification(new NotificationType0("textDocument/diagnosticsBegin"));
}; };
export var sendDiagnosticsEnd = function (conn) { return function () {
    return conn.sendNotification(new NotificationType0("textDocument/diagnosticsEnd"));
}; };
export var sendCleanBegin = function (conn) { return function () {
    return conn.sendNotification(new NotificationType0("textDocument/cleanBegin"));
}; };
export var sendCleanEnd = function (conn) { return function () {
    return conn.sendNotification(new NotificationType0("textDocument/cleanEnd"));
}; };
export var onExecuteCommand = function (conn) {
    return registerHandler(conn.onExecuteCommand);
};
export var onDidChangeWatchedFiles = function (conn) {
    return registerNotificationHandler(conn.onDidChangeWatchedFiles);
};
export var onExit = function (conn) {
    return registerNotificationHandler0(conn.onExit);
};
export var onShutdown = function (conn) {
    return registerHandler0(conn.onShutdown);
};
