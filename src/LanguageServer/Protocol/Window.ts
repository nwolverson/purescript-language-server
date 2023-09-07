import {
  Connection,
  MessageActionItem,
  WorkDoneProgressServerReporter,
} from "vscode-languageserver/node.js";

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

export const createWorkDoneProgressImpl = (conn: Connection) => () =>
  conn.window.createWorkDoneProgress();

export const workDone = (reporter: WorkDoneProgressServerReporter) => () =>
  reporter.done();
export const workBegin =
  (reporter: WorkDoneProgressServerReporter) =>
  ({ title }: { title: string }) =>
  () =>
    reporter.begin(title, undefined, undefined, true);
export const report =
  (reporter: WorkDoneProgressServerReporter) => (percentage: number) => () =>
    reporter.report(percentage);
export const reportMsg =
  (reporter: WorkDoneProgressServerReporter) => (msg: string) => () =>
    reporter.report(msg);
export const report2 =
  (reporter: WorkDoneProgressServerReporter) =>
  (percentage: number) =>
  (msg: string) =>
  () =>
    reporter.report(percentage, msg);
