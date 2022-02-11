export var getDocuments = function (documents) { return function () {
    return documents.all();
}; };
export var getDocument = function (documents) { return function (uri) { return function () {
    return documents.get(uri);
}; }; };
export var onDidSaveDocument = function (documents) {
    return function (f) {
        return function () {
            return documents.onDidSave(function (p) { return f(p)(); });
        };
    };
};
export var onDidOpenDocument = function (documents) {
    return function (f) {
        return function () {
            return documents.onDidOpen(function (p) { return f(p)(); });
        };
    };
};
export var onDidCloseDocument = function (documents) {
    return function (f) {
        return function () {
            return documents.onDidClose(function (p) { return f(p)(); });
        };
    };
};
export var onDidChangeContent = function (documents) {
    return function (f) {
        return function () {
            return documents.onDidChangeContent(function (p) { return f(p)(); });
        };
    };
};
