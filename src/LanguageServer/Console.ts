import { Connection } from 'vscode-languageserver';

export const log = (conn: Connection) => (s: string) => () => conn.console.log(s);
export const info = (conn: Connection) => (s: string) => () => conn.console.info(s);
export const warn = (conn: Connection) => (s: string) => () => conn.console.warn(s);
export const error = (conn: Connection) => (s: string) => () => conn.console.error(s);