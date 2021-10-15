"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getConfigurationImpl = exports.initDocumentStore = exports.initConnection = void 0;
var node_1 = require("vscode-languageserver/node");
var vscode_languageserver_textdocument_1 = require("vscode-languageserver-textdocument");
exports.initConnection = function (commands) { return function (cb) { return function () {
    var conn = node_1.createConnection();
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
                textDocumentSync: node_1.TextDocumentSyncKind.Full,
                // Tell the client that the server support code complete
                completionProvider: {
                    resolveProvider: false,
                    triggerCharacters: ["."]
                },
                codeLensProvider: {
                    resolveProvider: false
                },
                hoverProvider: true,
                definitionProvider: true,
                workspaceSymbolProvider: true,
                documentSymbolProvider: true,
                codeActionProvider: { codeActionKinds: [node_1.CodeActionKind.Empty, node_1.CodeActionKind.SourceOrganizeImports, "source.sortImports", node_1.CodeActionKind.SourceFixAll, node_1.CodeActionKind.Source] },
                executeCommandProvider: (params.initializationOptions || {}).executeCommandProvider === false
                    ? undefined : {
                    commands: commands
                },
                referencesProvider: true,
                foldingRangeProvider: true,
                documentFormattingProvider: true
            }
        };
    });
    return conn;
}; }; };
exports.initDocumentStore = function (conn) { return function () {
    var documents = new node_1.TextDocuments(vscode_languageserver_textdocument_1.TextDocument);
    documents.listen(conn);
    return documents;
}; };
exports.getConfigurationImpl = function (conn) { return function () {
    return conn.workspace.getConfiguration("purescript").then(function (config) {
        return { purescript: config };
    });
}; };
