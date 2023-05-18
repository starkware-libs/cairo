import * as vscode from "vscode";
import { SemanticTokensFeature } from "vscode-languageclient/lib/common/semanticTokens";
import * as child_process from "child_process";
import * as fs from "fs";
import * as path from "path";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";
import { ChildProcessWithoutNullStreams } from "child_process";

let client: LanguageClient;

// Tries to find the development version of the language server executable,
// assuming the workspace directory is inside the Cairo repository.
function findDevLanguageServerAt(
  path: string,
  depth: number
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

function rootPath(context: vscode.ExtensionContext): string {
  let rootPath = context.extensionPath;

  const workspaceFolders = vscode.workspace.workspaceFolders;
  if (workspaceFolders) {
    rootPath = workspaceFolders[0].uri.path || rootPath;
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
  return path.replace(/\${workspaceFolder}/g, root);
}

function findLanguageServerExecutable(
  config: vscode.WorkspaceConfiguration,
  context: vscode.ExtensionContext
) {
  const root = rootPath(context);
  const configPath = config.get<string>("cairo1.languageServerPath");
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
  const envPath = process.env.PATH || "";
  const envExt = process.env.PATHEXT || "";
  const pathDirs = envPath
    .replace(/["]+/g, "")
    .split(path.delimiter)
    .filter(Boolean);
  const extensions = envExt.split(";");
  const candidates: string[] = [];
  pathDirs.map((d) =>
    extensions.map((ext) => {
      candidates.push(path.join(d, name + ext));
    })
  );
  const isExecutable = (path: string) =>
    fs.promises
      .access(path, fs.constants.X_OK)
      .then(() => path)
      .catch(() => undefined);
  try {
    return await Promise.all(candidates.map(isExecutable)).then((values) =>
      values.find((value) => !!value)
    );
  } catch (e) {
    return undefined;
  }
}

async function findScarbExecutablePath(
  config: vscode.WorkspaceConfiguration,
  context: vscode.ExtensionContext
) {
  // Check config for scarb path.
  const root = rootPath(context);
  const configPath = config.get<string>("cairo1.scarbPath");
  if (configPath) {
    const scarbPath = replacePathPlaceholders(configPath, root);
    if (!isExecutable(scarbPath)) {
      return undefined;
    }
    return scarbPath;
  }

  // Check PATH env var for scarb path.
  return await findExecutableFromPathVar("scarb");
}

function notifyScarbMissing(outputChannel: vscode.OutputChannel) {
  const errorMessage =
    "This is a Scarb project, but could not find Scarb executable on this machine. " +
    "Please add Scarb to the PATH environmental variable or set the 'cairo1.scarbPath' configuration " +
    "parameter. Otherwise Cairo code analysis will not work.";
  vscode.window.showWarningMessage(errorMessage);
  outputChannel.appendLine(errorMessage);
}

async function listScarbCommandsOutput(scarbPath: undefined | string) {
  if (!scarbPath) {
    return undefined;
  }
  const child = child_process.spawn(scarbPath, ["--json", "commands"], {
    stdio: "pipe",
  });
  let stdout = "";
  for await (const chunk of child.stdout) {
    stdout += chunk;
  }
  return stdout;
}

async function isScarbLsPresent(
  scarbPath: undefined | string
): Promise<boolean> {
  if (!scarbPath) {
    return false;
  }
  const scarbOutput = await listScarbCommandsOutput(scarbPath);
  if (!scarbOutput) return false;
  return scarbOutput
    .split("\n")
    .map((v) => v.trim())
    .filter((v) => !!v)
    .map((v) => JSON.parse(v))
    .some(
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      (commands: any) => !!commands["cairo-language-server"]
    );
}

async function runStandaloneLs(
  scarbPath: undefined | string,
  outputChannel: vscode.OutputChannel,
  config: vscode.WorkspaceConfiguration,
  context: vscode.ExtensionContext
): Promise<undefined | ChildProcessWithoutNullStreams> {
  const executable = findLanguageServerExecutable(config, context);
  if (!executable) {
    outputChannel.appendLine(
      "Cairo language server was not found. Make sure cairo-lang-server is " +
        "installed and that the configuration 'cairo1.languageServerPath' is correct."
    );
    return;
  }
  outputChannel.appendLine("Cairo language server running from: " + executable);
  return child_process.spawn(executable, {
    env: { SCARB: scarbPath },
  });
}

async function runScarbLs(
  scarbPath: undefined | string,
  outputChannel: vscode.OutputChannel
): Promise<undefined | ChildProcessWithoutNullStreams> {
  if (!scarbPath) {
    return;
  }
  outputChannel.appendLine(
    "Cairo language server running from Scarb at: " + scarbPath
  );
  return child_process.spawn(scarbPath, ["cairo-language-server"], {});
}

async function setupLanguageServer(
  config: vscode.WorkspaceConfiguration,
  context: vscode.ExtensionContext,
  outputChannel: vscode.OutputChannel
) {
  const scarbPath = await findScarbExecutablePath(config, context);
  if (scarbPath) {
    outputChannel.appendLine("Using Scarb binary from: " + scarbPath);
  }

  const serverOptions: ServerOptions =
    async (): Promise<ChildProcessWithoutNullStreams> => {
      let child;
      if (await isScarbLsPresent(scarbPath)) {
        child = await runScarbLs(scarbPath, outputChannel);
      } else {
        child = await runStandaloneLs(
          scarbPath,
          outputChannel,
          config,
          context
        );
      }
      if (!child) {
        outputChannel.appendLine("Failed to start Cairo language server.");
        throw new Error("Failed to start Cairo language server.");
      }
      // Forward stderr to vscode logs.
      child.stderr.on("data", (data: Buffer) => {
        outputChannel.appendLine("Server stderr> " + data.toString());
      });
      child.on("exit", (code, signal) => {
        outputChannel.appendLine(
          "Cairo language server exited with code " +
            code +
            " and signal" +
            signal
        );
      });

      // Create a resolved promise with the child process.
      return child;
    };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "cairo" },
      { scheme: "vfs", language: "cairo" },
    ],
  };

  client = new LanguageClient(
    "cairoLanguageServer",
    "Cairo Language Server",
    serverOptions,
    clientOptions
  );
  client.registerFeature(new SemanticTokensFeature(client));
  client.onReady().then(() => {
    const myProvider = new (class
      implements vscode.TextDocumentContentProvider
    {
      async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        const res: any = await client.sendRequest("vfs/provide", {
          uri: uri.toString(),
        });
        return res.content;
      }

      onDidChangeEmitter = new vscode.EventEmitter<vscode.Uri>();
      onDidChange = this.onDidChangeEmitter.event;
    })();
    client.onNotification("vfs/update", (param) => {
      myProvider.onDidChangeEmitter.fire(param.uri);
    });
    vscode.workspace.registerTextDocumentContentProvider("vfs", myProvider);

    client.onNotification("scarb/could-not-find-scarb-executable", () =>
      notifyScarbMissing(outputChannel)
    );

    client.onNotification("scarb/resolving-start", () => {
      vscode.window.withProgress(
        {
          title: "Scarb is resolving the project...",
          location: vscode.ProgressLocation.Notification,
          cancellable: false,
        },
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        async (_progress, _token) => {
          return new Promise((resolve) => {
            client.onNotification("scarb/resolving-finish", () => {
              resolve(null);
            });
          });
        }
      );
    });
  });
  client.start();
}

export async function activate(context: vscode.ExtensionContext) {
  const config = vscode.workspace.getConfiguration();
  const outputChannel = vscode.window.createOutputChannel("Cairo extension");
  context.subscriptions.push(outputChannel);

  if (config.get<boolean>("cairo1.enableLanguageServer")) {
    await setupLanguageServer(config, context, outputChannel);
  } else {
    outputChannel.appendLine(
      "Language server is not enabled. Use the cairo1.enableLanguageServer config"
    );
  }
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
