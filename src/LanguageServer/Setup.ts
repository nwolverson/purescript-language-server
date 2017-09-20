import { IConnection, createConnection,InitializeParams, IPCMessageReader, IPCMessageWriter, TextDocuments, Location, Hover, TextDocumentSyncKind } from 'vscode-languageserver';

exports.initConnection = (commands: string[]) => (cb: (arg: {params: InitializeParams, conn: IConnection}) => () => void) => (): IConnection => {
    const conn = createConnection();
    conn.listen();
    
    conn.onInitialize((params) => {
        conn.console.info(JSON.stringify(params));
        cb({
            params,
            conn
        })();
        return {
            capabilities: {
                // Tell the client that the server works in FULL text document sync mode
                textDocumentSync: TextDocumentSyncKind.Full,
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
                    commands
                }
            }
        };
    });
    return conn;
}

exports.initDocumentStore = (conn : IConnection) => () => {
    const documents: TextDocuments = new TextDocuments();
    documents.listen(conn);
    return documents;
}
