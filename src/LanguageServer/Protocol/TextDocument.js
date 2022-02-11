export var getText = function (document) { return function () { return document.getText(); }; };
export var getTextAtRange = function (document) { return function (range) { return function () {
    return document.getText(range);
}; }; };
export var getUri = function (document) { return document.uri; };
export var getLanguageId = function (document) { return document.languageId; };
export var getVersion = function (document) { return function () { return document.version; }; };
export var getLineCount = function (document) { return function () {
    return document.lineCount;
}; };
export var offsetAtPosition = function (document) { return function (pos) { return function () {
    return document.offsetAt(pos);
}; }; };
export var positionAtOffset = function (document) { return function (offset) { return function () {
    return document.positionAt(offset);
}; }; };
