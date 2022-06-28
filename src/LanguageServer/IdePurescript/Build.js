export function parseShellQuote (str) {
  return require("shell-quote").parse(str);
}

export function getOsTmpDir() {
  return require("os").tmpdir();
};

export function getHash(str) {
  return require("crypto").createHash("sha256").update(str).digest("hex");
};

export const copyFile = (src) => (dest) => {
  return () => {
    return require("fs").promises.copyFile(src, dest);
  };
};
