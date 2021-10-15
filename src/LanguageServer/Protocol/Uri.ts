import { URI } from "vscode-uri";

export const uriToFilename = (uri: string) => () => URI.parse(uri).fsPath;
export const filenameToUri = (filename: string) => () =>
  URI.file(filename).toString();
