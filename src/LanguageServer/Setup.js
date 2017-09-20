"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var vscode_languageserver_1 = require("vscode-languageserver");
exports.initConnection = function (commands) { return function (cb) { return function () {
    var conn = vscode_languageserver_1.createConnection();
    conn.listen();
    conn.onInitialize(function (params) {
        conn.console.info(JSON.stringify(params));
        cb({
            params: params,
            conn: conn
        })();
        return {
            capabilities: {
                // Tell the client that the server works in FULL text document sync mode
                textDocumentSync: vscode_languageserver_1.TextDocumentSyncKind.Full,
                // Tell the client that the server support code complete
                completionProvider: {
                    resolveProvider: false,
                    triggerCharacters: []
                },
                hoverProvider: true,
                definitionProvider: true,
                workspaceSymbolProvider: true,
                documentSymbolProvider: true,
                codeActionProvider: true,
                executeCommandProvider: {
                    commands: commands
                }
            }
        };
    });
    return conn;
}; }; };
exports.initDocumentStore = function (conn) { return function () {
    var documents = new vscode_languageserver_1.TextDocuments();
    documents.listen(conn);
    return documents;
}; };
