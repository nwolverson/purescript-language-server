"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.positionAtOffset = exports.offsetAtPosition = exports.getLineCount = exports.getVersion = exports.getLanguageId = exports.getUri = exports.getTextAtRange = exports.getText = void 0;
exports.getText = function (document) { return function () { return document.getText(); }; };
exports.getTextAtRange = function (document) { return function (range) { return function () {
    return document.getText(range);
}; }; };
exports.getUri = function (document) { return document.uri; };
exports.getLanguageId = function (document) { return document.languageId; };
exports.getVersion = function (document) { return function () { return document.version; }; };
exports.getLineCount = function (document) { return function () {
    return document.lineCount;
}; };
exports.offsetAtPosition = function (document) { return function (pos) { return function () {
    return document.offsetAt(pos);
}; }; };
exports.positionAtOffset = function (document) { return function (offset) { return function () {
    return document.positionAt(offset);
}; }; };
