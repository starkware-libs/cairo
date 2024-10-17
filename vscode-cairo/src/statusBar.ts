import * as vscode from "vscode";
import { Context } from "./context";
import { Scarb } from "./scarb";

export async function setupStatusBar(ctx: Context) {
  ctx.extension.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration((e) => {
      if (e.affectsConfiguration("cairo1.enableStatusBar")) {
        updateStatusBar(ctx);
      }
    }),
  );

  updateStatusBar(ctx);
}

async function updateStatusBar(ctx: Context) {
  const config = vscode.workspace.getConfiguration("cairo1");
  const enableStatusBar = config.get<boolean>("enableStatusBar", true);

  if (enableStatusBar) {
    ctx.statusBarItem.text = "Cairo";
    ctx.statusBarItem.tooltip = "Cairo Language";

    try {
      const scarb = await Scarb.find(vscode.workspace.workspaceFolders?.[0], ctx);
      if (scarb) {
        const version = await scarb.getVersion(ctx);
        ctx.statusBarItem.tooltip = `Cairo Language\n${version}`;
      }
    } catch (error) {
      ctx.log.error(`Error getting Scarb version: ${error}`);
    }

    ctx.statusBarItem.show();
  } else {
    ctx.statusBarItem.hide();
  }
}
