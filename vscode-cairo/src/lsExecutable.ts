import * as lc from "vscode-languageclient/node";
import * as vscode from "vscode";

import { Scarb } from "./scarb";
import { findScarbForWorkspaceFolder } from "./cairols";
import { Context } from "./context";
import { isScarbProject } from "./scarbProject";
import { StandaloneLS } from "./standalonels";

export async function getLSExecutables(
  workspaceFolders: readonly vscode.WorkspaceFolder[],
  ctx: Context,
): Promise<LSExecutable[]> {
  return (
    await Promise.all(
      workspaceFolders.map((workspaceFolder) => LSExecutable.tryFind(workspaceFolder, ctx)),
    )
  ).filter((x) => !!x);
}

export async function executablesEqual(executables: LSExecutable[]): Promise<boolean> {
  if (executables.length < 2) {
    return true;
  }

  for (const executable of executables) {
    if (!(await executables[0]!.equals(executable))) {
      return false;
    }
  }
  return true;
}

export class LSExecutable {
  private constructor(
    public readonly workspaceFolder: vscode.WorkspaceFolder | undefined,
    public readonly preparedInvocation: lc.Executable,
    public readonly scarb: Scarb | undefined,
    private readonly context: Context,
  ) {}

  public static async tryFind(
    workspaceFolder: vscode.WorkspaceFolder | undefined,
    ctx: Context,
  ): Promise<LSExecutable | undefined> {
    const scarb = await findScarbForWorkspaceFolder(workspaceFolder, ctx);
    try {
      const provider = await determineLanguageServerExecutableProvider(workspaceFolder, scarb, ctx);
      const preparedInvocation = provider.languageServerExecutable();
      return new LSExecutable(workspaceFolder, preparedInvocation, scarb, ctx);
    } catch (e) {
      ctx.log.error(`${e}`);
    }
    return undefined;
  }

  public async equals(other: LSExecutable): Promise<boolean> {
    const commandsEqual = this.preparedInvocation.command === other.preparedInvocation.command;

    const argsEqual =
      this.preparedInvocation.args === other.preparedInvocation.args ||
      (Array.isArray(this.preparedInvocation.args) &&
        Array.isArray(other.preparedInvocation.args) &&
        this.preparedInvocation.args.length === other.preparedInvocation.args.length &&
        this.preparedInvocation.args.every(
          (value, index) => value === other.preparedInvocation.args![index],
        ));

    // Also check the scarb versions, if there are any present
    const scarbVersionsEqual =
      (await this.scarb?.getVersion(this.context)) ===
      (await other.scarb?.getVersion(this.context));

    return commandsEqual && argsEqual && scarbVersionsEqual;
  }
}

// TODO(6740): Get rid of this interface, it's an extra level of abstraction we don't need (we call it immediataly anyways).
export interface LanguageServerExecutableProvider {
  languageServerExecutable(): lc.Executable;
}

export async function determineLanguageServerExecutableProvider(
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
