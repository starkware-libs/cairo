import * as lc from "vscode-languageclient/node";
import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { setupLanguageClient } from "./cairols";
import { Context } from "./context";

export type WorkspacePath = string;
export type ClientsMap = Map<WorkspacePath, lc.LanguageClient>;
const clients: ClientsMap = new Map();

function deleteClient(folder: vscode.WorkspaceFolder) {
  const client = clients.get(folder.uri.toString());
  if (client) {
    clients.delete(folder.uri.toString());
    client.stop();
  }
}

function getClientForFile(uri: vscode.Uri): lc.LanguageClient | undefined {
  let matchFunction: (entry: [k: string, v: LanguageClient]) => boolean;
  if (uri.scheme === "vfs") {
    // eslint-disable-next-line no-useless-escape
    const pathComponents = uri.path.replace(/[\[\]]/g, "").split("/");
    const workspaceName = pathComponents[1];
    if (!workspaceName) {
      throw new Error(`Could not extract workspace name from path: ${uri.path}`);
    }
    matchFunction = ([k]) => {
      return k.endsWith(workspaceName);
    };
  } else {
    matchFunction = ([k]) => {
      return uri.fsPath.startsWith(k);
    };
  }

  const matchingEntry = Array.from(clients.entries()).find(matchFunction);

  if (matchingEntry) {
    return matchingEntry[1];
  }
  return undefined;
}

async function initializeClient(ctx: Context, folder: vscode.WorkspaceFolder) {
  if (!clients.has(folder.uri.path.toString())) {
    const client = await setupLanguageClient(ctx, folder);
    clients.set(folder.uri.path.toString(), client);
  }
}

export function stopAllClients(): Thenable<void> | undefined {
  return Promise.all(Array.from(clients.values()).map((client) => client.stop())).then(
    () => undefined,
  );
}

export { getClientForFile, deleteClient, initializeClient };
