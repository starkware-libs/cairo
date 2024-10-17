import * as vscode from "vscode";
import { Config } from "./config";
import { RootLogOutputChannel } from "./logging";
import { StatusBar } from "./statusBar";

export class Context {
  public static create(extensionContext: vscode.ExtensionContext): Context {
    const log = new RootLogOutputChannel(
      vscode.window.createOutputChannel("Cairo", {
        log: true,
      }),
    );

    extensionContext.subscriptions.push(log);

    const context = new Context(extensionContext, log);
    context.statusBar = new StatusBar(context);

    return context;
  }

  public readonly config: Config = new Config();
  public statusBar!: StatusBar;

  private constructor(
    public readonly extension: vscode.ExtensionContext,
    public readonly log: RootLogOutputChannel,
  ) {}
}
