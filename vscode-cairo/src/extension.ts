import * as vscode from "vscode";
import * as lc from "vscode-languageclient/node";
import { setupLanguageServer } from "./cairols";
import { Context } from "./context";

let client: lc.LanguageClient | undefined;

export async function activate(extensionContext: vscode.ExtensionContext) {
  const ctx = Context.create(extensionContext);

  if (ctx.config.get<boolean>("enableLanguageServer")) {
    client = await setupLanguageServer(
      vscode.workspace.getConfiguration(),
      ctx.extension,
      ctx.log,
    );
  } else {
    ctx.log.appendLine(
      "Language server is not enabled. Use the cairo1.enableLanguageServer config",
    );
  }
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
