import { Connection, MessageActionItem } from "vscode-languageserver/node";

export const showError = (conn: Connection) => (s: string) => () =>
  conn.window.showErrorMessage(s);
export const showErrorWithActionsImpl =
  (conn: Connection) =>
  (s: string) =>
  <T extends MessageActionItem>(actions: T[]) =>
  () =>
    conn.window.showErrorMessage(s, ...actions);

export const showWarning = (conn: Connection) => (s: string) => () =>
  conn.window.showWarningMessage(s);
export const showWarningWithActionsImpl =
  (conn: Connection) =>
  (s: string) =>
  <T extends MessageActionItem>(actions: T[]) =>
  () =>
    conn.window.showWarningMessage(s, ...actions);

export const showInformation = (conn: Connection) => (s: string) => () =>
  conn.window.showInformationMessage(s);
export const showInformationWithActionsImpl =
  (conn: Connection) =>
  (s: string) =>
  <T extends MessageActionItem>(actions: T[]) =>
  () =>
    conn.window.showInformationMessage(s, ...actions);
