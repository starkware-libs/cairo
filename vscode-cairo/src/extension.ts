import * as vscode from "vscode";
import * as lc from "vscode-languageclient/node";
import { setupLanguageServer } from "./cairols";
import { Context } from "./context";
import { Scarb } from "./scarb";

let client: lc.LanguageClient | undefined;
let statusBarItem: vscode.StatusBarItem;

export async function activate(extensionContext: vscode.ExtensionContext) {
  const ctx = Context.create(extensionContext);

  if (ctx.config.get("enableLanguageServer")) {
    client = await setupLanguageServer(ctx);
  } else {
    ctx.log.warn("language server is disabled");
    ctx.log.warn("note: set `cairo1.enableLanguageServer` to `true` to enable it");
  }

  statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
  extensionContext.subscriptions.push(statusBarItem);

  updateStatusBar(ctx);

  extensionContext.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration((e) => {
      if (e.affectsConfiguration("cairo1.showStatusBar")) {
        updateStatusBar(ctx);
      }
    }),
  );
}

async function updateStatusBar(ctx: Context) {
  const config = vscode.workspace.getConfiguration("cairo1");
  const showStatusBar = config.get<boolean>("showStatusBar", true);

  if (showStatusBar) {
    const editor = vscode.window.activeTextEditor;
    if (editor && editor.document.languageId === "cairo") {
      statusBarItem.text = "Cairo";
      statusBarItem.tooltip = "Cairo Language";

      // Get Scarb version
      try {
        const scarb = await Scarb.find(vscode.workspace.workspaceFolders?.[0], ctx);
        if (scarb) {
          const version = await scarb.getVersion(ctx);
          statusBarItem.tooltip = `Cairo Language\n${version}`;
        }
      } catch (error) {
        ctx.log.error(`Error getting Scarb version: ${error}`);
      }

      statusBarItem.show();
    } else {
      statusBarItem.hide();
    }
  } else {
    statusBarItem.hide();
  }
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  if (statusBarItem) {
    statusBarItem.dispose();
  }

  return client.stop();
}
