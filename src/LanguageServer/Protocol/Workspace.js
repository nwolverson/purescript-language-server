import { CodeLensRefreshRequest, } from "vscode-languageserver/node";
export var codeLensRefresh = function (conn) { return function () {
    return conn.sendRequest(CodeLensRefreshRequest.type);
}; };
