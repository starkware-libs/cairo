import * as vscode from "vscode";
import * as lc from "vscode-languageclient/node";
import type { Context } from "./context";
import { Scarb } from "./scarb";

const CAIRO_STATUS_BAR_COMMAND = "cairo1.statusBar.clicked";

export class StatusBar {
  private statusBarItem: vscode.StatusBarItem;

  constructor(private readonly context: Context) {
    this.statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left);

    this.context.extension.subscriptions.push(this.statusBarItem);
  }

  public async setup(client?: lc.LanguageClient): Promise<void> {
    this.context.extension.subscriptions.push(
      vscode.workspace.onDidChangeConfiguration(() => {
        this.update();
      }),
    );

    this.context.extension.subscriptions.push(
      vscode.commands.registerCommand(CAIRO_STATUS_BAR_COMMAND, () => {
        if (client) {
          client.outputChannel.show();
        } else {
          vscode.window.showWarningMessage("Cairo Language Server is not active");
        }
      }),
    );

    await this.initializeStatusBarItem();
  }

  private async initializeStatusBarItem(): Promise<void> {
    this.statusBarItem.command = CAIRO_STATUS_BAR_COMMAND;
    this.statusBarItem.text = "Cairo";
    this.statusBarItem.tooltip = "Cairo Language";

    await this.updateScarbVersion();
  }

  async updateScarbVersion(): Promise<void> {
    try {
      const scarb = await Scarb.find(vscode.workspace.workspaceFolders?.[0], this.context);
      if (scarb) {
        const version = await scarb.getVersion(this.context);
        this.statusBarItem.tooltip = `Cairo Language\n${version}`;
      }
    } catch (error) {
      this.context.log.error(`Error getting Scarb version: ${error}`);
    }
  }

  /**
   * Updates the status bar item based on the current workspace configuration.
   */
  private async update(): Promise<void> {
    const config = vscode.workspace.getConfiguration("cairo1");
    const showInStatusBar = config.get<boolean>("showInStatusBar", true);

    if (showInStatusBar) {
      await this.updateScarbVersion();
      this.showStatusBarItem();
    } else {
      this.hideStatusBarItem();
    }
  }

  public showStatusBarItem(): void {
    this.statusBarItem.show();
  }

  public hideStatusBarItem(): void {
    this.statusBarItem.hide();
  }
}
