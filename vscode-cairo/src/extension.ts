import * as vscode from "vscode";
import * as lc from "vscode-languageclient/node";
import { setupLanguageServer } from "./cairols";
import { Context } from "./context";

let client: lc.LanguageClient | undefined;

export async function activate(extensionContext: vscode.ExtensionContext) {
  const ctx = Context.create(extensionContext);

  if (ctx.config.get("enableLanguageServer")) {
    client = await setupLanguageServer(ctx);
  } else {
    ctx.log.warn("language server is disabled");
    ctx.log.warn(
      "note: set `cairo1.enableLanguageServer` to `true` to enable it",
    );
  }
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
