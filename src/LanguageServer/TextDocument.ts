import { TextDocument, Position } from 'vscode-languageserver';

export const getText = (document: TextDocument) => () => document.getText();

export const getUri = (document: TextDocument) => document.uri;

export const getLanguageId = (document: TextDocument) => document.languageId;

export const getVersion = (document: TextDocument) => () => document.version;

export const getLineCount = (document: TextDocument) => () => document.lineCount;

export const offsetAtPosition = (document: TextDocument) => (pos: Position) => () => document.offsetAt(pos);

export const positionAtOffset = (document: TextDocument) => (offset: number) => () => document.positionAt(offset);

