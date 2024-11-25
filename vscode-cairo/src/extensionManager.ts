import * as lc from "vscode-languageclient/node";
import * as vscode from "vscode";
import { setupLanguageServer } from "./cairols";
import { executablesEqual, getLSExecutables, LSExecutable } from "./lsExecutable";
import { Context } from "./context";
import assert from "node:assert";

export class CairoExtensionManager {
  private constructor(
    public readonly context: Context,
    private client: lc.LanguageClient | undefined,
    private runningExecutable: LSExecutable | undefined,
  ) {}

  public static fromContext(context: Context): CairoExtensionManager {
    return new CairoExtensionManager(context, undefined, undefined);
  }

  // Starts the server, sets up the client and returns status
  public async tryStartClient(): Promise<boolean> {
    if (this.runningExecutable) {
      return false;
    }

    const setupResult = await setupLanguageServer(this.context);
    if (!setupResult) {
      return false;
    }
    const [client, runningExecutable] = setupResult;
    this.client = client;
    this.runningExecutable = runningExecutable;
    return true;
  }

  public async stopClient() {
    await this.client?.stop();
    this.client = undefined;
    this.runningExecutable = undefined;
  }

  public getClient(): lc.LanguageClient | undefined {
    return this.client;
  }

  handleWorkspaceFoldersRemoved() {
    this.tryStartClient();
  }

  async handleWorkspaceFoldersAdded(added: readonly vscode.WorkspaceFolder[]) {
    const ctx = this.context;

    const newExecutables = await getLSExecutables(added, ctx);
    if (!newExecutables.length) {
      return;
    }

    // Check if new ones are of same provider
    const newExecutablesHaveSameProvider = await executablesEqual(newExecutables);

    if (newExecutablesHaveSameProvider) {
      // In case we weren't running anything previously, we can start up a new server
      const started = await this.tryStartClient();
      if (started) {
        return;
      }
      assert(this.runningExecutable, "An executable should be present by this point"); // We disallow this by trying to run the start procedure beforehand

      const consistentWithPreviousLS = await executablesEqual([
        ...newExecutables,
        this.runningExecutable,
      ]);

      // If it's not consistent, we need to stop LS and show an error, it's better to show no analysis results than broken ones
      // For example - a person can turn on a project with incompatible corelib version
      if (!consistentWithPreviousLS) {
        await this.stopClient();
        vscode.window.showErrorMessage(
          "Analysis disabled: the added folder does not have the same version of Scarb as the rest of the workspace. Please remove this folder from the workspace, or update its Scarb version to match the others.",
        );
      }
    }
  }
}
