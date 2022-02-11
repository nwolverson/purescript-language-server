import { URI } from "vscode-uri";
export var uriToFilename = function (uri) { return function () { return URI.parse(uri).fsPath; }; };
export var filenameToUri = function (filename) { return function () {
    return URI.file(filename).toString();
}; };
