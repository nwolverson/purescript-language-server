export function version () {
  try {
    return require("./package.json").version;
  } catch (e) {
    return e.message;
  }
}
