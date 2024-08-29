import * as lc from "vscode-languageclient/node";
import * as vscode from "vscode";
import { Context } from "./context";
import { expandMacro, vfsProvide, viewAnalyzedCrates } from "./lspRequests";

export const registerVfsProvider = (client: lc.LanguageClient, ctx: Context) => {
  const eventEmitter = new vscode.EventEmitter<vscode.Uri>();

  const vfsProvider: vscode.TextDocumentContentProvider = {
    async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
      const res = await client.sendRequest(vfsProvide, {
        uri: uri.toString(),
      });

      return res.content ?? "";
    },
    onDidChange: eventEmitter.event,
  };

  client.onNotification("vfs/update", (param) => {
    eventEmitter.fire(param.uri);
  });

  ctx.extension.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider("vfs", vfsProvider),
  );
};

export const registerMacroExpandProvider = (client: lc.LanguageClient, ctx: Context) => {
  const uri = vscode.Uri.parse("cairo-expand-macro://expandMacro/[EXPANSION].cairo");
  const eventEmitter = new vscode.EventEmitter<vscode.Uri>();

  const tdcp: vscode.TextDocumentContentProvider = {
    async provideTextDocumentContent(): Promise<string> {
      const editor = vscode.window.activeTextEditor;
      if (!editor) return "";

      const position = editor.selection.active;

      const expanded = await client.sendRequest(expandMacro, {
        textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(editor.document),
        position,
      });

      return expanded ?? "Not available";
    },
    onDidChange: eventEmitter.event,
  };

  ctx.extension.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider("cairo-expand-macro", tdcp),
  );

  ctx.extension.subscriptions.push(
    vscode.commands.registerCommand("cairo.expandMacro", async () => {
      const document = await vscode.workspace.openTextDocument(uri);

      eventEmitter.fire(uri);

      return vscode.window.showTextDocument(document, vscode.ViewColumn.Two, true);
    }),
  );
};

export const registerViewAnalyzedCratesProvider = (client: lc.LanguageClient, ctx: Context) => {
  const uri = vscode.Uri.parse(
    "cairo-view-analyzed-crates://viewAnalyzedCrates/[ANALYZED_CRATES].txt",
  );
  const eventEmitter = new vscode.EventEmitter<vscode.Uri>();

  const tdcp: vscode.TextDocumentContentProvider = {
    provideTextDocumentContent: () => client.sendRequest(viewAnalyzedCrates),
    onDidChange: eventEmitter.event,
  };

  ctx.extension.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider("cairo-view-analyzed-crates", tdcp),
  );

  ctx.extension.subscriptions.push(
    vscode.commands.registerCommand("cairo.viewAnalyzedCrates", async () => {
      const document = await vscode.workspace.openTextDocument(uri);

      eventEmitter.fire(uri);

      return vscode.window.showTextDocument(document, vscode.ViewColumn.Two, true);
    }),
  );

  ctx.extension.subscriptions.push(
    vscode.workspace.onDidChangeTextDocument((e) => {
      const path = e.document.uri.path;
      const relevant_suffixes = [".cairo", "Scarb.toml", "Scarb.lock", "cairo_project.toml"];

      for (const suffix of relevant_suffixes) {
        if (path.endsWith(suffix)) {
          eventEmitter.fire(uri);
          break;
        }
      }
    }),
  );
};
