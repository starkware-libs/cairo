import * as child_process from "child_process";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import * as vscode from "vscode";
import { SemanticTokensFeature } from "vscode-languageclient/lib/common/semanticTokens";

import * as lc from "vscode-languageclient/node";
import { Context } from "./context";

// Tries to find the development version of the language server executable,
// assuming the workspace directory is inside the Cairo repository.
function findDevLanguageServerAt(
  path: string,
  depth: number,
): string | undefined {
  if (depth == 0) {
    return undefined;
  }
  let candidate = path + "/target/release/cairo-language-server";
  if (fs.existsSync(candidate)) {
    return candidate;
  }
  candidate = path + "/target/debug/cairo-language-server";
  if (fs.existsSync(candidate)) {
    return candidate;
  }
  return findDevLanguageServerAt(path + "/..", depth - 1);
}

function rootPath(ctx: Context): string {
  let rootPath = ctx.extension.extensionPath;

  const workspaceFolders = vscode.workspace.workspaceFolders;
  if (workspaceFolders) {
    rootPath = workspaceFolders[0]?.uri.path || rootPath;
  }
  return rootPath;
}

function isExecutable(path: string): boolean {
  try {
    fs.accessSync(path, fs.constants.X_OK);
    return fs.statSync(path).isFile();
  } catch (e) {
    return false;
  }
}

function replacePathPlaceholders(path: string, root: string): string {
  return path
    .replace(/\${workspaceFolder}/g, root)
    .replace(/\${userHome}/g, process.env["HOME"] ?? "");
}

function findLanguageServerExecutable(ctx: Context) {
  const root = rootPath(ctx);
  const configPath = ctx.config.get<string>("languageServerPath");
  if (configPath) {
    const serverPath = replacePathPlaceholders(configPath, root);
    if (!isExecutable(serverPath)) {
      return undefined;
    }
    return serverPath;
  }

  // TODO(spapini): Use a bundled language server.
  return findDevLanguageServerAt(root, 10);
}

async function findExecutableFromPathVar(name: string) {
  const envPath = process.env["PATH"] || "";
  const envExt = process.env["PATHEXT"] || "";
  const pathDirs = envPath
    .replace(/["]+/g, "")
    .split(path.delimiter)
    .filter(Boolean);
  const extensions = envExt.split(";");
  const candidates: string[] = [];
  pathDirs.map((d) =>
    extensions.map((ext) => {
      candidates.push(path.join(d, name + ext));
    }),
  );
  const isExecutable = (path: string) =>
    fs.promises
      .access(path, fs.constants.X_OK)
      .then(() => path)
      .catch(() => undefined);
  try {
    return await Promise.all(candidates.map(isExecutable)).then((values) =>
      values.find((value) => !!value),
    );
  } catch (e) {
    return undefined;
  }
}

async function findScarbExecutablePathInAsdfDir() {
  if (os.platform() === "win32") {
    return undefined;
  }

  let asdfDataDir = process.env["ASDF_DATA_DIR"];
  if (!asdfDataDir) {
    const home = process.env["HOME"];
    if (!home) {
      return undefined;
    }
    asdfDataDir = path.join(home, ".asdf");
  }
  const scarbExecutablePath = path.join(asdfDataDir, "shims", "scarb");

  try {
    await fs.promises.access(scarbExecutablePath, fs.constants.X_OK);
    return scarbExecutablePath;
  } catch (e) {
    return undefined;
  }
}

async function findScarbExecutablePath(ctx: Context) {
  // Check config for scarb path.
  const root = rootPath(ctx);
  const configPath = ctx.config.get<string>("scarbPath");
  if (configPath) {
    const scarbPath = replacePathPlaceholders(configPath, root);
    if (!isExecutable(scarbPath)) {
      return undefined;
    }
    return scarbPath;
  }

  // Check PATH env var for scarb path.
  const envPath = await findExecutableFromPathVar("scarb");
  if (envPath) return envPath;

  return findScarbExecutablePathInAsdfDir();
}

function notifyScarbMissing(ctx: Context) {
  const errorMessage =
    "This is a Scarb project, but could not find Scarb executable on this machine. " +
    "Please add Scarb to the PATH environmental variable or set the 'cairo1.scarbPath' configuration " +
    "parameter. Otherwise Cairo code analysis will not work.";
  void vscode.window.showWarningMessage(errorMessage);
  ctx.log.error(errorMessage);
}

async function listScarbCommandsOutput(
  scarbPath: undefined | string,
  ctx: Context,
) {
  if (!scarbPath) {
    return undefined;
  }
  const child = child_process.spawn(scarbPath, ["--json", "commands"], {
    stdio: "pipe",
    cwd: rootPath(ctx),
  });
  let stdout = "";
  for await (const chunk of child.stdout) {
    stdout += chunk;
  }
  return stdout;
}

async function isScarbLsPresent(
  scarbPath: undefined | string,
  ctx: Context,
): Promise<boolean> {
  if (!scarbPath) {
    return false;
  }
  const scarbOutput = await listScarbCommandsOutput(scarbPath, ctx);
  if (!scarbOutput) return false;
  return scarbOutput
    .split("\n")
    .map((v) => v.trim())
    .filter((v) => !!v)
    .map((v) => JSON.parse(v))
    .some(
      (commands: Record<string, unknown>) =>
        !!commands["cairo-language-server"],
    );
}

async function runStandaloneLs(
  scarbPath: undefined | string,
  ctx: Context,
): Promise<undefined | child_process.ChildProcessWithoutNullStreams> {
  const executable = findLanguageServerExecutable(ctx);
  if (!executable) {
    ctx.log.error("could not find Cairo language server executable");
    ctx.log.error(
      "note: make sure CairoLS is installed and `cairo1.languageServerPath` points to it",
    );
    return;
  }
  ctx.log.debug(`using CairoLS: ${executable}`);
  return child_process.spawn(executable, {
    env: { SCARB: scarbPath },
  });
}

async function runScarbLs(
  scarbPath: undefined | string,
  ctx: Context,
): Promise<undefined | child_process.ChildProcessWithoutNullStreams> {
  if (!scarbPath) {
    return;
  }
  ctx.log.debug(`using CairoLS: ${scarbPath} cairo-language-server`);
  return child_process.spawn(scarbPath, ["cairo-language-server"], {
    cwd: rootPath(ctx),
  });
}

enum ServerType {
  Standalone,
  Scarb,
}

async function getServerType(
  isScarbEnabled: boolean,
  scarbPath: string | undefined,
  configLanguageServerPath: string | undefined,
  ctx: Context,
) {
  if (!isScarbEnabled) return ServerType.Standalone;
  if (!(await isScarbProject()) && !!configLanguageServerPath) {
    // If Scarb manifest is missing, and Cairo-LS path is explicit.
    return ServerType.Standalone;
  }
  if (await isScarbLsPresent(scarbPath, ctx)) return ServerType.Scarb;
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
  const isScarbEnabled = ctx.config.get<boolean>("enableScarb", false);
  const scarbPath = await findScarbExecutablePath(ctx);
  const configLanguageServerPath = ctx.config.get<string>("languageServerPath");

  if (!isScarbEnabled) {
    ctx.log.warn("Scarb integration is disabled");
    ctx.log.warn("note: set `cairo1.enableScarb` to `true` to enable it");
  } else if (!scarbPath) {
    ctx.log.error("could not find Scarb executable on this machine");
  } else {
    ctx.log.debug(`using Scarb: ${scarbPath}`);
  }
  const serverOptions: lc.ServerOptions =
    async (): Promise<child_process.ChildProcessWithoutNullStreams> => {
      const serverType = await getServerType(
        isScarbEnabled,
        scarbPath,
        configLanguageServerPath,
        ctx,
      );
      let child;
      if (serverType === ServerType.Scarb) {
        child = await runScarbLs(scarbPath, ctx);
      } else {
        child = await runStandaloneLs(scarbPath, ctx);
      }
      if (!child) {
        ctx.log.error("failed to start Cairo language server");
        throw new Error("Failed to start Cairo language server.");
      }
      // Forward stderr to vscode logs.
      child.stderr.on("data", (data: Buffer) => {
        ctx.log.trace("Server stderr> " + data.toString());
      });
      child.on("exit", (code, signal) => {
        ctx.log.debug(
          `Cairo language server exited with code ${code} and signal ${signal}`,
        );
      });

      // Create a resolved promise with the child process.
      return child;
    };

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
