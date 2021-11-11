"use strict";
var __spreadArrays = (this && this.__spreadArrays) || function () {
    for (var s = 0, i = 0, il = arguments.length; i < il; i++) s += arguments[i].length;
    for (var r = Array(s), k = 0, i = 0; i < il; i++)
        for (var a = arguments[i], j = 0, jl = a.length; j < jl; j++, k++)
            r[k] = a[j];
    return r;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.report2 = exports.reportMsg = exports.report = exports.workBegin = exports.workDone = exports.createWorkDoneProgressImpl = exports.showInformationWithActionsImpl = exports.showInformation = exports.showWarningWithActionsImpl = exports.showWarning = exports.showErrorWithActionsImpl = exports.showError = void 0;
exports.showError = function (conn) { return function (s) { return function () { return conn.window.showErrorMessage(s); }; }; };
exports.showErrorWithActionsImpl = function (conn) { return function (s) { return function (actions) { return function () {
    var _a;
    return (_a = conn.window).showErrorMessage.apply(_a, __spreadArrays([s], actions));
}; }; }; };
exports.showWarning = function (conn) { return function (s) { return function () { return conn.window.showWarningMessage(s); }; }; };
exports.showWarningWithActionsImpl = function (conn) { return function (s) { return function (actions) { return function () {
    var _a;
    return (_a = conn.window).showWarningMessage.apply(_a, __spreadArrays([s], actions));
}; }; }; };
exports.showInformation = function (conn) { return function (s) { return function () { return conn.window.showInformationMessage(s); }; }; };
exports.showInformationWithActionsImpl = function (conn) { return function (s) { return function (actions) { return function () {
    var _a;
    return (_a = conn.window).showInformationMessage.apply(_a, __spreadArrays([s], actions));
}; }; }; };
exports.createWorkDoneProgressImpl = function (conn) { return function () { return conn.window.createWorkDoneProgress(); }; };
exports.workDone = function (reporter) { return function () { return reporter.done(); }; };
exports.workBegin = function (reporter) { return function (_a) {
    var title = _a.title;
    return function () { return reporter.begin(title, undefined, undefined, true); };
}; };
exports.report = function (reporter) { return function (percentage) { return function () { return reporter.report(percentage); }; }; };
exports.reportMsg = function (reporter) { return function (msg) { return function () { return reporter.report(msg); }; }; };
exports.report2 = function (reporter) { return function (percentage) { return function (msg) { return function () { return reporter.report(percentage, msg); }; }; }; };
