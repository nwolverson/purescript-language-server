import { TextDocuments, TextDocumentChangeEvent } from 'vscode-languageserver';

export const getDocuments = (documents: TextDocuments) => () => documents.all();

export const getDocument = (documents: TextDocuments) => (uri: string) => () => documents.get(uri);

export const onDidSaveDocument = (documents: TextDocuments) => (f: (e: TextDocumentChangeEvent) => () => void) => () => documents.onDidSave(p => f(p)());

export const onDidOpenDocument = (documents: TextDocuments) => (f: (e: TextDocumentChangeEvent) => () => void) => () => documents.onDidOpen(p => f(p)());

export const onDidCloseDocument = (documents: TextDocuments) => (f: (e: TextDocumentChangeEvent) => () => void) => () => documents.onDidClose(p => f(p)());

export const onDidChangeContent = (documents: TextDocuments) => (f: (e: TextDocumentChangeEvent) => () => void) => () => documents.onDidChangeContent(p => f(p)());
