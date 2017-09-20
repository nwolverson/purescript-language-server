"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var vscode_uri_1 = require("vscode-uri");
exports.uriToFilename = function (uri) { return function () { return vscode_uri_1.default.parse(uri).fsPath; }; };
exports.filenameToUri = function (filename) { return function () { return vscode_uri_1.default.file(filename).toString(); }; };
