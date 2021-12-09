exports.parseShellQuote = function (str) {
  return require("shell-quote").parse(str);
};

exports.getOsTmpDir = function () {
  return require("os").tmpdir();
};

exports.getHash = function (str) {
  return require("crypto").createHash("sha256").update(str).digest("hex");
};

exports.copyFile = (src) => (dest) => {
  return () => {
    return require("fs").promises.copyFile(src, dest);
  };
};
