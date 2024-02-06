import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";
import { SemanticTokensFeature } from "vscode-languageclient/lib/common/semanticTokens";

import * as lc from "vscode-languageclient/node";
import { Context } from "./context";
import { Scarb } from "./scarb";
import { StandaloneLS } from "./standalonels";

function notifyScarbMissing(ctx: Context) {
  const errorMessage =
    "This is a Scarb project, but could not find Scarb executable on this machine. " +
    "Please add Scarb to the PATH environmental variable or set the 'cairo1.scarbPath' configuration " +
    "parameter. Otherwise Cairo code analysis will not work.";
  void vscode.window.showWarningMessage(errorMessage);
  ctx.log.error(errorMessage);
}

enum ServerType {
  Standalone,
  Scarb,
}

async function getServerType(
  isScarbEnabled: boolean,
  scarb: Scarb | undefined,
  configLanguageServerPath: string | undefined,
  ctx: Context,
) {
  if (!isScarbEnabled) return ServerType.Standalone;
  if (!(await isScarbProject()) && !!configLanguageServerPath) {
    // If Scarb manifest is missing, and Cairo-LS path is explicit.
    return ServerType.Standalone;
  }
  if (await scarb?.hasCairoLS(ctx)) return ServerType.Scarb;
  return ServerType.Standalone;
}

async function isScarbProjectAt(path: string, depth: number): Promise<boolean> {
  if (depth == 0) return false;
  const isFile = await fs.promises
    .access(path + "/Scarb.toml", fs.constants.F_OK)
    .then(() => true)
    .catch(() => false);
  if (isFile) return true;
  return isScarbProjectAt(path + "/..", depth - 1);
}

async function isScarbProject(): Promise<boolean> {
  const depth = 20;
  const workspaceFolders = vscode.workspace.workspaceFolders;
  if (
    !!workspaceFolders?.[0] &&
    (await isScarbProjectAt(path.dirname(workspaceFolders[0].uri.path), depth))
  )
    return true;
  const editor = vscode.window.activeTextEditor;
  return (
    !!editor &&
    (await isScarbProjectAt(path.dirname(editor.document.uri.path), depth))
  );
}

export async function setupLanguageServer(
  ctx: Context,
): Promise<lc.LanguageClient> {
  const serverOptions = await getServerOptions(ctx);

  const clientOptions: lc.LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "cairo" },
      { scheme: "vfs", language: "cairo" },
    ],
  };

  const client = new lc.LanguageClient(
    "cairoLanguageServer",
    "Cairo Language Server",
    serverOptions,
    clientOptions,
  );

  client.registerFeature(new SemanticTokensFeature(client));

  const myProvider = new (class implements vscode.TextDocumentContentProvider {
    async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
      interface ProvideVirtualFileResponse {
        content?: string;
      }

      const res = await client.sendRequest<ProvideVirtualFileResponse>(
        "vfs/provide",
        {
          uri: uri.toString(),
        },
      );

      return res.content ?? "";
    }

    onDidChangeEmitter = new vscode.EventEmitter<vscode.Uri>();
    onDidChange = this.onDidChangeEmitter.event;
  })();
  client.onNotification("vfs/update", (param) => {
    myProvider.onDidChangeEmitter.fire(param.uri);
  });
  vscode.workspace.registerTextDocumentContentProvider("vfs", myProvider);

  client.onNotification("scarb/could-not-find-scarb-executable", () =>
    notifyScarbMissing(ctx),
  );

  client.onNotification("scarb/resolving-start", () => {
    vscode.window.withProgress(
      {
        title: "Scarb is resolving the project...",
        location: vscode.ProgressLocation.Notification,
        cancellable: false,
      },
      async () => {
        return new Promise((resolve) => {
          client.onNotification("scarb/resolving-finish", () => {
            resolve(null);
          });
        });
      },
    );
  });

  await client.start();

  return client;
}

async function getServerOptions(ctx: Context): Promise<lc.ServerOptions> {
  // TODO(mkaput): Support multi-root workspaces.
  const workspaceFolder = vscode.workspace.workspaceFolders?.[0];

  const isScarbEnabled = ctx.config.get("enableScarb", false);
  const configLanguageServerPath = ctx.config.get("languageServerPath");

  let scarb: Scarb | undefined;
  if (!isScarbEnabled) {
    ctx.log.warn("Scarb integration is disabled");
    ctx.log.warn("note: set `cairo1.enableScarb` to `true` to enable it");
  } else {
    try {
      scarb = await Scarb.find(workspaceFolder, ctx);
    } catch (e) {
      ctx.log.error(`${e}`);
      ctx.log.error("note: Scarb integration is disabled due to this error");
    }
  }

  const serverType = await getServerType(
    isScarbEnabled,
    scarb,
    configLanguageServerPath,
    ctx,
  );

  let serverExecutable: lc.Executable | undefined;
  if (serverType === ServerType.Scarb) {
    serverExecutable = scarb!.languageServerExecutable();
  } else {
    try {
      const ls = await StandaloneLS.find(workspaceFolder, scarb, ctx);
      serverExecutable = ls.languageServerExecutable();
    } catch (e) {
      ctx.log.error(`${e}`);
    }
  }
  if (serverExecutable == undefined) {
    ctx.log.error("failed to start CairoLS");
    throw new Error("failed to start CairoLS");
  }
  ctx.log.debug(
    `using CairoLS: ${serverExecutable.command} ${serverExecutable.args?.join(" ") ?? ""}`.trimEnd(),
  );

  return {
    run: serverExecutable,
    debug: serverExecutable,
  };
}
