import { spawn } from "child_process";
import * as vscode from "vscode";
import * as lc from "vscode-languageclient/node";
import type { Context } from "./context";

let globalExecId = 0;

export class Scarb {
  public constructor(
    /**
     * The path to the Scarb binary on the local filesystem.
     */
    public readonly path: string,
    /**
     * The exact Scarb binary used can vary depending on workspace configs,
     * hence we associate workspace folder reference with the Scarb instance.
     */
    public readonly workspaceFolder?: vscode.WorkspaceFolder | undefined,
  ) {}

  public languageServerExecutable(): lc.Executable {
    const exec: lc.Executable = {
      command: this.path,
      args: ["cairo-language-server"],
    };

    const cwd = this.workspaceFolder?.uri.fsPath;
    if (cwd != undefined) {
      exec.options ??= {};
      exec.options.cwd = cwd;
    }

    return exec;
  }

  public hasCairoLS(ctx: Context): Promise<boolean> {
    return this.hasCommand("cairo-language-server", ctx);
  }

  private async hasCommand(command: string, ctx: Context): Promise<boolean> {
    const output = await this.execWithOutput(["--json", "commands"], ctx);

    if (!output) {
      return false;
    }

    return output
      .split("\n")
      .map((v) => v.trim())
      .filter((v) => !!v)
      .map((v) => JSON.parse(v))
      .some((commands: Record<string, unknown>) => !!commands[command]);
  }

  private async execWithOutput(
    args: readonly string[],
    ctx: Context,
  ): Promise<string> {
    const execId = globalExecId++;

    ctx.log.trace(`scarb[${execId}]: ${this.path} ${args.join(" ")}`.trimEnd());

    const child = spawn(this.path, args, {
      stdio: "pipe",
      cwd: this.workspaceFolder?.uri.fsPath,
    });

    let stdout = "";
    for await (const chunk of child.stdout) {
      stdout += chunk;
    }

    if (ctx.log.logLevel <= vscode.LogLevel.Trace) {
      if (stdout.length > 0) {
        for (const line of stdout.trimEnd().split("\n")) {
          ctx.log.trace(`scarb[${execId}]:stdout: ${line}`);
        }
      }

      let stderr = "";
      for await (const chunk of child.stderr) {
        stderr += chunk;
      }

      if (stderr.length > 0) {
        for (const line of stderr.trimEnd().split("\n")) {
          ctx.log.trace(`scarb[${execId}]:stderr: ${line}`);
        }
      }
    }

    return stdout;
  }
}
