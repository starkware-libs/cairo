import * as vscode from "vscode";
import { SemanticTokensFeature } from "vscode-languageclient/lib/common/semanticTokens";

import * as lc from "vscode-languageclient/node";
import { Context } from "./context";
import { Scarb } from "./scarb";
import { isScarbProject } from "./scarbProject";
import { StandaloneLS } from "./standalonels";

export interface LanguageServerExecutableProvider {
  languageServerExecutable(): lc.Executable;
}

function notifyScarbMissing(ctx: Context) {
  const errorMessage =
    "This is a Scarb project, but could not find Scarb executable on this machine. " +
    "Please add Scarb to the PATH environmental variable or set the 'cairo1.scarbPath' configuration " +
    "parameter. Otherwise Cairo code analysis will not work.";
  void vscode.window.showWarningMessage(errorMessage);
  ctx.log.error(errorMessage);
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

  let serverExecutableProvider: LanguageServerExecutableProvider | undefined;
  try {
    serverExecutableProvider = await determineLanguageServerExecutableProvider(
      workspaceFolder,
      scarb,
      ctx,
    );
  } catch (e) {
    ctx.log.error(`${e}`);
  }

  const serverExecutable = serverExecutableProvider?.languageServerExecutable();
  if (serverExecutable == undefined) {
    ctx.log.error("failed to start CairoLS");
    throw new Error("failed to start CairoLS");
  }

  insertLanguageServerExtraEnv(serverExecutable, ctx);

  ctx.log.debug(`using CairoLS: ${quoteServerExecutable(serverExecutable)}`);

  const run = serverExecutable;

  const debug = structuredClone(serverExecutable);
  debug.options ??= {};
  debug.options.env ??= {};
  debug.options.env["CAIRO_LS_LOG"] ??= "cairo_lang_language_server=debug";

  return { run, debug };
}

async function determineLanguageServerExecutableProvider(
  workspaceFolder: vscode.WorkspaceFolder | undefined,
  scarb: Scarb | undefined,
  ctx: Context,
): Promise<LanguageServerExecutableProvider> {
  const standalone = () => StandaloneLS.find(workspaceFolder, scarb, ctx);

  // If Scarb is missing or is disabled, always fallback to standalone CairoLS.
  if (!scarb) {
    return await standalone();
  }

  // If Scarb manifest is missing, and standalone CairoLS path is explicit.
  if (!(await isScarbProject()) && ctx.config.has("languageServerPath")) {
    return await standalone();
  }

  // Otherwise, always prefer Scarb CairoLS.
  if (await scarb.hasCairoLS(ctx)) {
    return scarb;
  }

  return await standalone();
}

function insertLanguageServerExtraEnv(
  serverExecutable: lc.Executable,
  ctx: Context,
) {
  const extraEnv = ctx.config.get("languageServerExtraEnv");
  serverExecutable.options ??= {};
  serverExecutable.options.env ??= {};
  Object.assign(serverExecutable.options.env, extraEnv);
}

function quoteServerExecutable(serverExecutable: lc.Executable): string {
  const parts: string[] = [];

  if (serverExecutable.options?.env) {
    for (const [key, value] of Object.entries(serverExecutable.options.env)) {
      parts.push(`${key}=${value}`);
    }
  }

  parts.push(serverExecutable.command);

  if (serverExecutable.args) {
    for (const arg of serverExecutable.args) {
      parts.push(arg);
    }
  }

  return parts.filter((s) => s.trim()).join(" ");
}
