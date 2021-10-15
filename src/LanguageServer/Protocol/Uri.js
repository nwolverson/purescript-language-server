"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.filenameToUri = exports.uriToFilename = void 0;
var vscode_uri_1 = require("vscode-uri");
exports.uriToFilename = function (uri) { return function () { return vscode_uri_1.URI.parse(uri).fsPath; }; };
exports.filenameToUri = function (filename) { return function () { return vscode_uri_1.URI.file(filename).toString(); }; };
