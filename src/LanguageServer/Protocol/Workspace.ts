import {
  CodeLensRefreshRequest,
  Connection,
  MessageActionItem,
} from "vscode-languageserver/node.js";

export const codeLensRefresh = (conn: Connection) => () =>
  conn.sendRequest(CodeLensRefreshRequest.type);
