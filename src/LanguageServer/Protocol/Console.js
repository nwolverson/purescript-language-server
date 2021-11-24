"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.error = exports.warn = exports.info = exports.log = void 0;
var log = function (conn) { return function (s) { return function () {
    return conn.console.log(s);
}; }; };
exports.log = log;
var info = function (conn) { return function (s) { return function () {
    return conn.console.info(s);
}; }; };
exports.info = info;
var warn = function (conn) { return function (s) { return function () {
    return conn.console.warn(s);
}; }; };
exports.warn = warn;
var error = function (conn) { return function (s) { return function () {
    return conn.console.error(s);
}; }; };
exports.error = error;
