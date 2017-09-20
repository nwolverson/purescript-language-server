"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getDocuments = function (documents) { return function () { return documents.all(); }; };
exports.getDocument = function (documents) { return function (uri) { return function () { return documents.get(uri); }; }; };
exports.onDidSaveDocument = function (documents) { return function (f) { return function () { return documents.onDidSave(function (p) { return f(p)(); }); }; }; };
exports.onDidOpenDocument = function (documents) { return function (f) { return function () { return documents.onDidOpen(function (p) { return f(p)(); }); }; }; };
exports.onDidCloseDocument = function (documents) { return function (f) { return function () { return documents.onDidClose(function (p) { return f(p)(); }); }; }; };
exports.onDidChangeContent = function (documents) { return function (f) { return function () { return documents.onDidChangeContent(function (p) { return f(p)(); }); }; }; };
