import * as vscode from "vscode";

import { Context } from "./context";
import {
  registerMacroExpandProvider,
  registerVfsProvider,
  registerViewAnalyzedCratesProvider,
} from "./textDocumentProviders";
import { deleteClient, initializeClient, stopAllClients } from "./client";

let _sortedWorkspaceFolders: string[] | undefined;

function sortedWorkspaceFolders(): string[] {
  if (_sortedWorkspaceFolders === void 0) {
    _sortedWorkspaceFolders = vscode.workspace.workspaceFolders
      ? vscode.workspace.workspaceFolders
          .map((folder) => {
            let result = folder.uri.toString();

            if (result.charAt(result.length - 1) !== "/") {
              result = result + "/";
            }
            return result;
          })
          .sort((a: string, b: string) => {
            return a.length - b.length;
          })
      : [];
  }
  return _sortedWorkspaceFolders;
}

vscode.workspace.onDidChangeWorkspaceFolders(() => (_sortedWorkspaceFolders = undefined));

function getOuterMostWorkspaceFolder(folder: vscode.WorkspaceFolder): vscode.WorkspaceFolder {
  const sorted = sortedWorkspaceFolders();
  for (const element of sorted) {
    let uri = folder.uri.toString();
    if (uri.charAt(uri.length - 1) !== "/") {
      uri = uri + "/";
    }
    if (uri.startsWith(element)) {
      return vscode.workspace.getWorkspaceFolder(vscode.Uri.parse(element))!;
    }
  }
  return folder;
}

export function activate(extensionContext: vscode.ExtensionContext) {
  const ctx = Context.create(extensionContext);
  if (!ctx.config.get("enableLanguageServer")) {
    ctx.log.warn("language server is disabled");
    ctx.log.warn("note: set `cairo1.enableLanguageServer` to `true` to enable it");
    return;
  }

  registerVfsProvider(ctx);
  registerMacroExpandProvider(ctx);
  registerViewAnalyzedCratesProvider(ctx);

  async function didOpenTextDocument(document: vscode.TextDocument): Promise<void> {
    // We are only interested in language mode text
    if (document.languageId !== "cairo" || !["file", "vfs"].includes(document.uri.scheme)) {
      return;
    }
    const uri = document.uri;

    let folder = vscode.workspace.getWorkspaceFolder(uri);

    // Files outside a folder
    // TODO(arcticae): Those should probably be handled with a "default client"
    // it is unclear on how it should work exactly
    if (!folder) {
      return;
    }

    // If we have nested workspace folders we only start a server on the outermost workspace folder.
    folder = getOuterMostWorkspaceFolder(folder);
    await initializeClient(ctx, folder);
  }

  vscode.workspace.onDidOpenTextDocument(didOpenTextDocument);
  vscode.workspace.textDocuments.forEach(didOpenTextDocument);
  vscode.workspace.onDidChangeWorkspaceFolders((event) => {
    for (const folder of event.removed) {
      deleteClient(folder);
    }
  });
}

export function deactivate(): Thenable<void> | undefined {
  return stopAllClients();
}
