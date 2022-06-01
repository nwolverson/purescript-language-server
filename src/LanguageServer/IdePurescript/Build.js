export function parseShellQuote (str) {
  return require("shell-quote").parse(str);
}
