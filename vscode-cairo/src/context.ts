import * as vscode from "vscode";
import { Config } from "./config";

export class Context {
  public static create(extensionContext: vscode.ExtensionContext): Context {
    const log = vscode.window.createOutputChannel("Cairo Extension", {
      log: true,
    });
    extensionContext.subscriptions.push(log);

    return new Context(extensionContext, log);
  }

  public readonly config: Config = new Config();

  private constructor(
    public readonly extension: vscode.ExtensionContext,
    public readonly log: vscode.LogOutputChannel,
  ) {}
}
