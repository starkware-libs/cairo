import * as vscode from "vscode";
import { SemanticTokensFeature } from "vscode-languageclient/lib/common/semanticTokens";

import * as lc from "vscode-languageclient/node";
import { Context } from "./context";
import { Scarb } from "./scarb";
import { isScarbProject } from "./scarbProject";
import { StandaloneLS } from "./standalonels";
import { registerMacroExpandProvider, registerVfsProvider } from "./textDocumentProviders";
import { NotificationType } from "vscode-jsonrpc/lib/common/messages";

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

export async function setupLanguageServer(ctx: Context): Promise<lc.LanguageClient> {
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

  // Notify the server when client configuration changes.
  // CairoLS pulls configuration properties it is interested in by itself, so it
  // is not needed to attach any details in the notification payload.
  const weakClient = new WeakRef(client);

  ctx.extension.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration(
      async () => {
        const client = weakClient.deref();
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

  client.registerFeature(new SemanticTokensFeature(client));

  registerVfsProvider(client, ctx);
  registerMacroExpandProvider(client, ctx);

  client.onNotification("scarb/could-not-find-scarb-executable", () => notifyScarbMissing(ctx));

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

  client.onNotification(new NotificationType<string>("corelib/version-mismatch"), (params) => {
    const errorMessage =
      "Corelib version mismatch. If you are using Scarb try reopening the project to fix this error. Resort to `scarb cache clean` if it doesn't help. ERROR: " +
      params;

    vscode.window.showErrorMessage(errorMessage);
    ctx.log.error(errorMessage);
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

  const run = serverExecutableProvider?.languageServerExecutable();
  if (run == undefined) {
    ctx.log.error("failed to start CairoLS");
    throw new Error("failed to start CairoLS");
  }

  setupEnv(run, ctx);

  ctx.log.debug(`using CairoLS: ${quoteServerExecutable(run)}`);

  return { run, debug: run };
}

async function determineLanguageServerExecutableProvider(
  workspaceFolder: vscode.WorkspaceFolder | undefined,
  scarb: Scarb | undefined,
  ctx: Context,
): Promise<LanguageServerExecutableProvider> {
  const log = ctx.log.span("determineLanguageServerExecutableProvider");
  const standalone = () => StandaloneLS.find(workspaceFolder, scarb, ctx);

  if (!scarb) {
    log.trace("Scarb is missing");
    return await standalone();
  }

  if (await isScarbProject()) {
    log.trace("this is a Scarb project");

    if (!ctx.config.get("preferScarbLanguageServer", true)) {
      log.trace("`preferScarbLanguageServer` is false, using standalone LS");
      return await standalone();
    }

    if (await scarb.hasCairoLS(ctx)) {
      log.trace("using Scarb LS");
      return scarb;
    }

    log.trace("Scarb has no LS extension, falling back to standalone");
    return await standalone();
  } else {
    log.trace("this is *not* a Scarb project, looking for standalone LS");

    try {
      return await standalone();
    } catch (e) {
      log.trace("could not find standalone LS, trying Scarb LS");
      if (await scarb.hasCairoLS(ctx)) {
        log.trace("using Scarb LS");
        return scarb;
      }

      log.trace("could not find standalone LS and Scarb has no LS extension, will error out");
      throw e;
    }
  }
}

function setupEnv(serverExecutable: lc.Executable, ctx: Context) {
  const logEnv = {
    CAIRO_LS_LOG: buildEnvFilter(ctx),
    RUST_BACKTRACE: "1",
  };

  const extraEnv = ctx.config.get("languageServerExtraEnv");

  serverExecutable.options ??= {};
  serverExecutable.options.env = {
    // Inherit env from parent process.
    ...process.env,
    ...(serverExecutable.options.env ?? {}),
    ...logEnv,
    ...extraEnv,
  };
}

function buildEnvFilter(ctx: Context): string {
  const level = convertVscodeLogLevelToRust(ctx.log.logLevel);
  return level ? `cairo_lang_language_server=${level}` : "";
}

function convertVscodeLogLevelToRust(logLevel: vscode.LogLevel): string | null {
  switch (logLevel) {
    case vscode.LogLevel.Trace:
      return "trace";
    case vscode.LogLevel.Debug:
      return "debug";
    case vscode.LogLevel.Info:
      return "info";
    case vscode.LogLevel.Warning:
      return "warn";
    case vscode.LogLevel.Error:
      return "error";
    case vscode.LogLevel.Off:
      return null;
  }
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
