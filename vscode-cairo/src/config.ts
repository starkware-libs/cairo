import * as os from "os";
import * as vscode from "vscode";

interface ConfigProps {
  enableLanguageServer: boolean;
  languageServerPath: string;
  enableScarb: boolean;
  scarbPath: string;
  corelibPath: string;
  languageServerExtraEnv: null | Record<string, string | number>;
}

export class Config {
  public static ROOT: string = "cairo1";

  // TODO(mkaput): Attach configs to workspace folders when we'll support
  //  multi-root workspaces.

  get<K extends keyof ConfigProps>(prop: K): ConfigProps[K] | undefined;
  get<K extends keyof ConfigProps>(
    prop: K,
    defaultValue: ConfigProps[K],
  ): ConfigProps[K];
  public get(prop: keyof ConfigProps, defaultValue?: unknown): unknown {
    const config = vscode.workspace.getConfiguration(Config.ROOT);
    const value = config.get(prop, defaultValue);
    if (typeof value === "string" && isPropWithPlaceholders(prop)) {
      return replacePathPlaceholders(value, undefined);
    }
    return value;
  }

  public has(prop: keyof ConfigProps): boolean {
    const config = vscode.workspace.getConfiguration(Config.ROOT);
    return config.has(prop);
  }
}

function isPropWithPlaceholders(prop: keyof ConfigProps): boolean {
  return prop === "languageServerPath" || prop === "scarbPath";
}

function replacePathPlaceholders(
  path: string,
  workspaceFolder: vscode.WorkspaceFolder | undefined,
): string {
  // 1. If there is known workspace folder, replace ${workspaceFolder} with it.
  // 2. If it is undefined, assume the first folder in currently opened
  //    workspace. We could use the currently opened document to detect
  //    the correct workspace. However, that would be determined by the document
  //    user has opened on Editor startup. This could lead to unpredictable
  //    workspace selection in practice.
  // 3. If no workspace is opened, replace ${workspaceFolder} with empty string.
  const workspaceFolderPath =
    (workspaceFolder ?? vscode.workspace.workspaceFolders?.[0])?.uri.path ?? "";

  return path
    .replace(/\${workspaceFolder}/g, workspaceFolderPath)
    .replace(/\${userHome}/g, os.homedir());
}
