var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
export var showError = function (conn) { return function (s) { return function () {
    return conn.window.showErrorMessage(s);
}; }; };
export var showErrorWithActionsImpl = function (conn) {
    return function (s) {
        return function (actions) {
            return function () {
                var _a;
                return (_a = conn.window).showErrorMessage.apply(_a, __spreadArray([s], actions, false));
            };
        };
    };
};
export var showWarning = function (conn) { return function (s) { return function () {
    return conn.window.showWarningMessage(s);
}; }; };
export var showWarningWithActionsImpl = function (conn) {
    return function (s) {
        return function (actions) {
            return function () {
                var _a;
                return (_a = conn.window).showWarningMessage.apply(_a, __spreadArray([s], actions, false));
            };
        };
    };
};
export var showInformation = function (conn) { return function (s) { return function () {
    return conn.window.showInformationMessage(s);
}; }; };
export var showInformationWithActionsImpl = function (conn) {
    return function (s) {
        return function (actions) {
            return function () {
                var _a;
                return (_a = conn.window).showInformationMessage.apply(_a, __spreadArray([s], actions, false));
            };
        };
    };
};
export var createWorkDoneProgressImpl = function (conn) { return function () {
    return conn.window.createWorkDoneProgress();
}; };
export var workDone = function (reporter) { return function () {
    return reporter.done();
}; };
export var workBegin = function (reporter) {
    return function (_a) {
        var title = _a.title;
        return function () {
            return reporter.begin(title, undefined, undefined, true);
        };
    };
};
export var report = function (reporter) { return function (percentage) { return function () {
    return reporter.report(percentage);
}; }; };
export var reportMsg = function (reporter) { return function (msg) { return function () {
    return reporter.report(msg);
}; }; };
export var report2 = function (reporter) {
    return function (percentage) {
        return function (msg) {
            return function () {
                return reporter.report(percentage, msg);
            };
        };
    };
};
