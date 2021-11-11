import { CodeLensRefreshRequest, Connection, MessageActionItem } from "vscode-languageserver/node";

export const codeLensRefresh = (conn: Connection) => () => conn.sendRequest(CodeLensRefreshRequest.type);