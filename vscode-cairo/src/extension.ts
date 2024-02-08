import * as vscode from "vscode";
import * as lc from "vscode-languageclient/node";
import { setupLanguageServer } from "./cairols";

let client: lc.LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext) {
  const config = vscode.workspace.getConfiguration();
  const outputChannel = vscode.window.createOutputChannel("Cairo extension");
  context.subscriptions.push(outputChannel);

  if (config.get<boolean>("cairo1.enableLanguageServer")) {
    client = await setupLanguageServer(config, context, outputChannel);
  } else {
    outputChannel.appendLine(
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
