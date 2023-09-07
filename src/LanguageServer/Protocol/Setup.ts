import {
  Connection,
  createConnection,
  InitializeParams,
  TextDocuments,
  CodeActionKind,
  LSPObject,
  TextDocumentSyncKind,
} from "vscode-languageserver/node.js";
import { TextDocument } from "vscode-languageserver-textdocument";

export const initConnection =
  (commands: string[]) =>
  (cb: (arg: { params: InitializeParams; conn: Connection }) => () => void) =>
  (): Connection => {
    const conn = createConnection();
    conn.listen();

    conn.onInitialize((params) => {
      // conn.console.info(JSON.stringify(params));
      cb({
        params,
        conn,
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
          executeCommandProvider:
            ((params.initializationOptions as LSPObject) || {})
              .executeCommandProvider === false
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
