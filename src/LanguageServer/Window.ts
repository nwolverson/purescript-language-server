import { IConnection, MessageActionItem } from "vscode-languageserver/lib/main";

export const showError = (conn: IConnection) => (s: string) => () => conn.window.showErrorMessage(s);
export const showErrorWithActionsImpl = (conn: IConnection) => (s: string) => <T extends MessageActionItem>(actions: T[]) => () => conn.window.showErrorMessage(s, ...actions);

export const showWarning = (conn: IConnection) => (s: string) => () => conn.window.showWarningMessage(s);
export const showWarningWithActionsImpl = (conn: IConnection) => (s: string) => <T extends MessageActionItem>(actions: T[]) => () => conn.window.showWarningMessage(s, ...actions);

export const showInformation = (conn: IConnection) => (s: string) => () => conn.window.showInformationMessage(s);
export const showInformationWithActionsImpl = (conn: IConnection) => (s: string) => <T extends MessageActionItem>(actions: T[]) => () => conn.window.showInformationMessage(s, ...actions);

