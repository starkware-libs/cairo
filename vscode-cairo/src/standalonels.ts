import * as path from "path";
import * as vscode from "vscode";
import * as lc from "vscode-languageclient/node";
import type { LanguageServerExecutableProvider } from "./cairols";
import type { Context } from "./context";
import type { Scarb } from "./scarb";
import { checkTool, findToolAtWithExtension } from "./toolchain";

export class StandaloneLS implements LanguageServerExecutableProvider {
  public constructor(
    public readonly path: string,
    public readonly workspaceFolder?: vscode.WorkspaceFolder | undefined,
    public readonly scarb?: Scarb | undefined,
  ) {}

  public static async find(
    workspaceFolder: vscode.WorkspaceFolder | undefined,
    scarb: Scarb | undefined,
    ctx: Context,
  ): Promise<StandaloneLS> {
    const path = (await fromConfig()) || (await fromDev());
    if (!path) {
      throw new Error("could not find CairoLS on this machine");
    }
    return new StandaloneLS(path, workspaceFolder, scarb);

    async function fromConfig(): Promise<string | undefined> {
      // TODO(mkaput): Config should probably be scoped to workspace folder.
      let configPath = ctx.config.get("languageServerPath");
      if (configPath) {
        configPath = await checkTool(configPath);
        if (configPath) {
          ctx.log.debug(`using CairoLS from config: ${configPath}`);
          return configPath;
        }
        throw new Error(
          `configured CairoLS path does not exist: ${configPath}`,
        );
      }
      return undefined;
    }

    async function fromDev(): Promise<string | undefined> {
      // In case you are Cairo compiler developer, you might have standalone
      // CairoLS built in your workspace.
      const devPath = await findDevLanguageServerAt(workspaceFolder, ctx);
      if (devPath) {
        ctx.log.debug(`found compiler dev standalone CairoLS: ${devPath}`);
        return devPath;
      }
      return undefined;
    }
  }

  languageServerExecutable(): lc.Executable {
    const exec: Required<Pick<lc.Executable, "options">> & lc.Executable = {
      command: this.path,
      options: {
        env: {},
      },
    };

    if (this.scarb?.path) {
      exec.options.env["SCARB"] = this.scarb.path;
    }

    const cwd = this.workspaceFolder?.uri.fsPath;
    if (cwd != undefined) {
      exec.options.cwd = cwd;
    }

    return exec;
  }
}

/**
 * Tries to find the development version of the language server executable,
 * assuming the workspace directory is inside the Cairo repository.
 *
 * @remarks
 *
 * This function does not attempt to go further than 10 levels up the directory
 * tree.
 */
async function findDevLanguageServerAt(
  workspaceFolder: vscode.WorkspaceFolder | undefined,
  ctx: Context,
): Promise<string | undefined> {
  let root = workspaceFolder?.uri.fsPath ?? ctx.extension.extensionPath;

  for (let i = 0; i < 10; i++) {
    for (const target of ["release, debug"]) {
      const candidate = await findToolAtWithExtension(
        path.join(root, "target", target, "cairo-language-server"),
      );
      if (candidate) {
        return candidate;
      }
    }

    root = path.basename(root);
  }

  return undefined;
}
