import * as vscode from "vscode";
import * as lc from "vscode-languageclient/node";
import { LSExecutable, setupLanguageServer } from "./cairols";
import { Context } from "./context";
import { handleWorkspaceFoldersAdded, handleWorkspaceFoldersRemoved } from "./workspace";

let client: lc.LanguageClient | undefined;
let runningExecutable: LSExecutable | undefined;

export async function startClient(ctx: Context) {
  const setupResult = await setupLanguageServer(ctx);
  if (!setupResult) {
    return;
  }
  const [newClient, newExecutable] = setupResult;
  client = newClient;
  runningExecutable = newExecutable;
}

export async function stopClient() {
  await client?.stop();
  client = undefined;
  runningExecutable = undefined;
}

export async function activate(extensionContext: vscode.ExtensionContext) {
  const ctx = Context.create(extensionContext);

  if (ctx.config.get("enableLanguageServer")) {
    await startClient(ctx);
    // Notify the server when the client configuration changes.
    // CairoLS pulls configuration properties it is interested in by itself, so it
    // is unnecessary to attach any details in the notification payload.
    ctx.extension.subscriptions.push(
      vscode.workspace.onDidChangeConfiguration(
        async () => {
          if (client != undefined) {
            await client.sendNotification(lc.DidChangeConfigurationNotification.type, {
              settings: "",
            });
          }
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
            handleWorkspaceFoldersAdded(event.added, runningExecutable, ctx);
          }

          if (event.removed.length) {
            handleWorkspaceFoldersRemoved(runningExecutable, ctx);
          }
        },
        null,
        ctx.extension.subscriptions,
      ),
    );
  } else {
    ctx.log.warn("language server is disabled");
    ctx.log.warn("note: set `cairo1.enableLanguageServer` to `true` to enable it");
  }

  await ctx.statusBar.setup(client);
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }

  return client.stop();
}
