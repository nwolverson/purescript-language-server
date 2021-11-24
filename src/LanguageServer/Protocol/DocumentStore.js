"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.onDidChangeContent = exports.onDidCloseDocument = exports.onDidOpenDocument = exports.onDidSaveDocument = exports.getDocument = exports.getDocuments = void 0;
var getDocuments = function (documents) { return function () {
    return documents.all();
}; };
exports.getDocuments = getDocuments;
var getDocument = function (documents) { return function (uri) { return function () {
    return documents.get(uri);
}; }; };
exports.getDocument = getDocument;
var onDidSaveDocument = function (documents) {
    return function (f) {
        return function () {
            return documents.onDidSave(function (p) { return f(p)(); });
        };
    };
};
exports.onDidSaveDocument = onDidSaveDocument;
var onDidOpenDocument = function (documents) {
    return function (f) {
        return function () {
            return documents.onDidOpen(function (p) { return f(p)(); });
        };
    };
};
exports.onDidOpenDocument = onDidOpenDocument;
var onDidCloseDocument = function (documents) {
    return function (f) {
        return function () {
            return documents.onDidClose(function (p) { return f(p)(); });
        };
    };
};
exports.onDidCloseDocument = onDidCloseDocument;
var onDidChangeContent = function (documents) {
    return function (f) {
        return function () {
            return documents.onDidChangeContent(function (p) { return f(p)(); });
        };
    };
};
exports.onDidChangeContent = onDidChangeContent;
