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
    extensionContext.subscriptions.push(log);

    return new Context(extensionContext, log);
  }

  public readonly config: Config = new Config();

  private constructor(
    public readonly extension: vscode.ExtensionContext,
    public readonly log: RootLogOutputChannel,
  ) {}
}
