import { CodeLensRefreshRequest, } from "vscode-languageserver/node.js";
export var codeLensRefresh = function (conn) { return function () {
    return conn.sendRequest(CodeLensRefreshRequest.type);
}; };
