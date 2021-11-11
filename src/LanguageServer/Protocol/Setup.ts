import {
  Connection,
  createConnection,
  InitializeParams,
  TextDocuments,
  TextDocumentSyncKind,
  CodeActionKind,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";
export const initConnection =
  (commands: string[]) =>
  (cb: (arg: { params: InitializeParams; conn: Connection }) => () => void) =>
  (): Connection => {
    const conn = createConnection();
    conn.listen();

    conn.onInitialize((params) => {
      conn.console.info(JSON.stringify(params));
      cb({
        params,
        conn,
      })();

      return {
        capabilities: {
          // Tell the client that the server works in FULL text document sync mode
          textDocumentSync: TextDocumentSyncKind.Full,
          // Tell the client that the server support code complete
          completionProvider: {
            resolveProvider: false,
            triggerCharacters: ["."],
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
          executeCommandProvider:
            (params.initializationOptions || {}).executeCommandProvider ===
            false
              ? undefined
              : {
                  commands,
                },
          referencesProvider: true,
          foldingRangeProvider: true,
          documentFormattingProvider: true,
        },
      };
    });
    return conn;
  };

export const initDocumentStore = (conn: Connection) => () => {
  const documents: TextDocuments<TextDocument> = new TextDocuments(
    TextDocument
  );
  documents.listen(conn);
  return documents;
};

export const getConfigurationImpl = (conn: Connection) => () =>
  conn.workspace.getConfiguration("purescript").then((config) => {
    return { purescript: config };
  });