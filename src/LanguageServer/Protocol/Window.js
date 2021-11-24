"use strict";
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.report2 = exports.reportMsg = exports.report = exports.workBegin = exports.workDone = exports.createWorkDoneProgressImpl = exports.showInformationWithActionsImpl = exports.showInformation = exports.showWarningWithActionsImpl = exports.showWarning = exports.showErrorWithActionsImpl = exports.showError = void 0;
var showError = function (conn) { return function (s) { return function () {
    return conn.window.showErrorMessage(s);
}; }; };
exports.showError = showError;
var showErrorWithActionsImpl = function (conn) {
    return function (s) {
        return function (actions) {
            return function () {
                var _a;
                return (_a = conn.window).showErrorMessage.apply(_a, __spreadArray([s], actions, false));
            };
        };
    };
};
exports.showErrorWithActionsImpl = showErrorWithActionsImpl;
var showWarning = function (conn) { return function (s) { return function () {
    return conn.window.showWarningMessage(s);
}; }; };
exports.showWarning = showWarning;
var showWarningWithActionsImpl = function (conn) {
    return function (s) {
        return function (actions) {
            return function () {
                var _a;
                return (_a = conn.window).showWarningMessage.apply(_a, __spreadArray([s], actions, false));
            };
        };
    };
};
exports.showWarningWithActionsImpl = showWarningWithActionsImpl;
var showInformation = function (conn) { return function (s) { return function () {
    return conn.window.showInformationMessage(s);
}; }; };
exports.showInformation = showInformation;
var showInformationWithActionsImpl = function (conn) {
    return function (s) {
        return function (actions) {
            return function () {
                var _a;
                return (_a = conn.window).showInformationMessage.apply(_a, __spreadArray([s], actions, false));
            };
        };
    };
};
exports.showInformationWithActionsImpl = showInformationWithActionsImpl;
var createWorkDoneProgressImpl = function (conn) { return function () {
    return conn.window.createWorkDoneProgress();
}; };
exports.createWorkDoneProgressImpl = createWorkDoneProgressImpl;
var workDone = function (reporter) { return function () {
    return reporter.done();
}; };
exports.workDone = workDone;
var workBegin = function (reporter) {
    return function (_a) {
        var title = _a.title;
        return function () {
            return reporter.begin(title, undefined, undefined, true);
        };
    };
};
exports.workBegin = workBegin;
var report = function (reporter) { return function (percentage) { return function () {
    return reporter.report(percentage);
}; }; };
exports.report = report;
var reportMsg = function (reporter) { return function (msg) { return function () {
    return reporter.report(msg);
}; }; };
exports.reportMsg = reportMsg;
var report2 = function (reporter) {
    return function (percentage) {
        return function (msg) {
            return function () {
                return reporter.report(percentage, msg);
            };
        };
    };
};
exports.report2 = report2;
