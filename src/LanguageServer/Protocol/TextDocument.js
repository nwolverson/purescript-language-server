"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.positionAtOffset = exports.offsetAtPosition = exports.getLineCount = exports.getVersion = exports.getLanguageId = exports.getUri = exports.getTextAtRange = exports.getText = void 0;
var getText = function (document) { return function () { return document.getText(); }; };
exports.getText = getText;
var getTextAtRange = function (document) { return function (range) { return function () {
    return document.getText(range);
}; }; };
exports.getTextAtRange = getTextAtRange;
var getUri = function (document) { return document.uri; };
exports.getUri = getUri;
var getLanguageId = function (document) { return document.languageId; };
exports.getLanguageId = getLanguageId;
var getVersion = function (document) { return function () { return document.version; }; };
exports.getVersion = getVersion;
var getLineCount = function (document) { return function () {
    return document.lineCount;
}; };
exports.getLineCount = getLineCount;
var offsetAtPosition = function (document) { return function (pos) { return function () {
    return document.offsetAt(pos);
}; }; };
exports.offsetAtPosition = offsetAtPosition;
var positionAtOffset = function (document) { return function (offset) { return function () {
    return document.positionAt(offset);
}; }; };
exports.positionAtOffset = positionAtOffset;
