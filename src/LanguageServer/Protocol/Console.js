export var log = function (conn) { return function (s) { return function () {
    return conn.console.log(s);
}; }; };
export var info = function (conn) { return function (s) { return function () {
    return conn.console.info(s);
}; }; };
export var warn = function (conn) { return function (s) { return function () {
    return conn.console.warn(s);
}; }; };
export var error = function (conn) { return function (s) { return function () {
    return conn.console.error(s);
}; }; };
