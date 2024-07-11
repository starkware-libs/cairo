import * as lc from "vscode-languageclient/node";
import * as vscode from "vscode";
import { Context } from "./context";

export const registerVfsProvider = (client: lc.LanguageClient, ctx: Context) => {
  const vfsProvider = new (class implements vscode.TextDocumentContentProvider {
    async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
      interface ProvideVirtualFileResponse {
        content?: string;
      }

      const res = await client.sendRequest<ProvideVirtualFileResponse>("vfs/provide", {
        uri: uri.toString(),
      });

      return res.content ?? "";
    }

    onDidChangeEmitter = new vscode.EventEmitter<vscode.Uri>();
    onDidChange = this.onDidChangeEmitter.event;
  })();
  client.onNotification("vfs/update", (param) => {
    vfsProvider.onDidChangeEmitter.fire(param.uri);
  });
  ctx.extension.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider("vfs", vfsProvider),
  );
};

const expandMacro = new lc.RequestType<
  { textDocument: { uri: string }; position: { line: number; character: number } },
  string | null,
  void
>("cairo/expandMacro");

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

      if (expanded == null) return "Not available";

      return expanded;
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
