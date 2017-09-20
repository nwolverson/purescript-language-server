import { IConnection } from 'vscode-languageserver';

export const log = (conn: IConnection) => (s: string) => () => conn.console.log(s);
export const info = (conn: IConnection) => (s: string) => () => conn.console.info(s);
export const warn = (conn: IConnection) => (s: string) => () => conn.console.warn(s);
export const error = (conn: IConnection) => (s: string) => () => conn.console.error(s);
