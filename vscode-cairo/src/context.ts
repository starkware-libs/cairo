import * as vscode from "vscode";

export class Context {
  public static create(extensionContext: vscode.ExtensionContext): Context {
    const log = vscode.window.createOutputChannel("Cairo Extension", {
      log: true,
    });
    extensionContext.subscriptions.push(log);

    return new Context(extensionContext, log);
  }

  private constructor(
    public readonly extension: vscode.ExtensionContext,
    public readonly log: vscode.LogOutputChannel,
  ) {}

  /**
   * Get Cairo extension-specific configuration.
   *
   * @remarks
   * This is equivalent to calling:
   * ```
   * vscode.workspace.getConfiguration("cairo1")
   * ```
   */
  public get config(): vscode.WorkspaceConfiguration {
    return vscode.workspace.getConfiguration("cairo1");
  }
}
