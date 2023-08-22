import { createConnection, TextDocuments, CodeActionKind, TextDocumentSyncKind, } from "vscode-languageserver/node.js";
import { TextDocument } from "vscode-languageserver-textdocument";
export var initConnection = function (commands) {
    return function (cb) {
        return function () {
            var conn = createConnection();
            conn.listen();
            conn.onInitialize(function (params) {
                // conn.console.info(JSON.stringify(params));
                cb({
                    params: params,
                    conn: conn,
                })();
                return {
                    capabilities: {
                        // Tell the client that the server works in FULL text document sync mode
                        textDocumentSync: {
                            save: { includeText: false },
                            change: TextDocumentSyncKind.Incremental,
                            openClose: true,
                        },
                        // Tell the client that the server support code complete
                        completionProvider: {
                            resolveProvider: false,
                            triggerCharacters: ["."],
                        },
                        codeLensProvider: {
                            resolveProvider: false,
                        },
                        hoverProvider: true,
                        definitionProvider: true,
                        workspaceSymbolProvider: true,
                        documentSymbolProvider: true,
                        codeActionProvider: {
                            codeActionKinds: [
                                CodeActionKind.Empty,
                                CodeActionKind.SourceOrganizeImports,
                                "source.sortImports",
                                CodeActionKind.SourceFixAll,
                                CodeActionKind.Source,
                            ],
                        },
                        renameProvider: {
                            prepareProvider: true,
                            workDoneProgress: true
                        },
                        executeCommandProvider: (params.initializationOptions || {})
                            .executeCommandProvider === false
                            ? undefined
                            : {
                                commands: commands,
                            },
                        referencesProvider: true,
                        foldingRangeProvider: true,
                        documentFormattingProvider: true,
                    },
                };
            });
            return conn;
        };
    };
};
export var initDocumentStore = function (conn) { return function () {
    var documents = new TextDocuments(TextDocument);
    documents.listen(conn);
    return documents;
}; };
export var getConfigurationImpl = function (conn) { return function () {
    return conn.workspace.getConfiguration("purescript").then(function (config) {
        return { purescript: config };
    });
}; };
