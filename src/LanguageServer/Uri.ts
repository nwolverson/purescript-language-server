import Uri from 'vscode-uri';

export const uriToFilename = (uri: string) => () => Uri.parse(uri).fsPath;
export const filenameToUri = (filename : string) => () => Uri.file(filename).toString();

