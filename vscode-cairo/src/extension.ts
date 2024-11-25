import * as vscode from "vscode";
import * as lc from "vscode-languageclient/node";
import { Context } from "./context";
import { CairoExtensionManager } from "./extensionManager";

export async function activate(extensionContext: vscode.ExtensionContext) {
  const ctx = Context.create(extensionContext);

  if (ctx.config.get("enableLanguageServer")) {
    const extensionManager = CairoExtensionManager.fromContext(ctx);
    extensionManager.tryStartClient();

    // Notify the server when the client configuration changes.
    // CairoLS pulls configuration properties it is interested in by itself, so it
    // is unnecessary to attach any details in the notification payload.
    ctx.extension.subscriptions.push(
      vscode.workspace.onDidChangeConfiguration(
        async () => {
          await extensionManager
            .getClient()
            ?.sendNotification(lc.DidChangeConfigurationNotification.type, {
              settings: "",
            });
        },
        null,
        ctx.extension.subscriptions,
      ),
    );

    // React to workspace folders changes (additions, deletions)
    ctx.extension.subscriptions.push(
      vscode.workspace.onDidChangeWorkspaceFolders(
        async (event) => {
          if (event.added.length) {
            extensionManager.handleWorkspaceFoldersAdded(event.added);
          }

          if (event.removed.length) {
            extensionManager.handleWorkspaceFoldersRemoved();
          }
        },
        null,
        ctx.extension.subscriptions,
      ),
    );
    ctx.extension.subscriptions.push({ dispose: extensionManager.stopClient });
    ctx.statusBar.setup(extensionManager.getClient());
  } else {
    ctx.log.warn("language server is disabled");
    ctx.log.warn("note: set `cairo1.enableLanguageServer` to `true` to enable it");
  }
}
