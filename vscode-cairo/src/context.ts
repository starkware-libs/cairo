import * as vscode from "vscode";
import { Config } from "./config";
import { RootLogOutputChannel } from "./logging";

export class Context {
  public static create(extensionContext: vscode.ExtensionContext): Context {
    const log = new RootLogOutputChannel(
      vscode.window.createOutputChannel("Cairo", {
        log: true,
      }),
    );
    const statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);

    extensionContext.subscriptions.push(log);
    extensionContext.subscriptions.push(statusBarItem);

    return new Context(extensionContext, log, statusBarItem);
  }

  public readonly config: Config = new Config();

  private constructor(
    public readonly extension: vscode.ExtensionContext,
    public readonly log: RootLogOutputChannel,
    public readonly statusBarItem: vscode.StatusBarItem,
  ) {}
}
