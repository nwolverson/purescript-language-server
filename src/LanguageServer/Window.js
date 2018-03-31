"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.showError = function (conn) { return function (s) { return function () { return conn.window.showErrorMessage(s); }; }; };
exports.showErrorWithActionsImpl = function (conn) { return function (s) { return function (actions) { return function () {
    return (_a = conn.window).showErrorMessage.apply(_a, [s].concat(actions));
    var _a;
}; }; }; };
exports.showWarning = function (conn) { return function (s) { return function () { return conn.window.showWarningMessage(s); }; }; };
exports.showWarningWithActionsImpl = function (conn) { return function (s) { return function (actions) { return function () {
    return (_a = conn.window).showWarningMessage.apply(_a, [s].concat(actions));
    var _a;
}; }; }; };
exports.showInformation = function (conn) { return function (s) { return function () { return conn.window.showInformationMessage(s); }; }; };
exports.showInformationWithActionsImpl = function (conn) { return function (s) { return function (actions) { return function () {
    return (_a = conn.window).showInformationMessage.apply(_a, [s].concat(actions));
    var _a;
}; }; }; };
