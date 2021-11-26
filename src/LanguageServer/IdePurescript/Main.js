exports.version = function () {
  try {
    return require("./package.json").version;
  } catch (e) {
    return e.message;
  }
};
