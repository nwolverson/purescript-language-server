exports.parseShellQuote = function (str) {
    return require('shell-quote').parse(str);
};