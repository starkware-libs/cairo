import * as vscode from "vscode";
import { Context } from "./context";
import { allUnderlyingCommandsEqual, getLSExecutables, LSExecutable } from "./cairols";
import { startClient, stopClient } from "./extension";

export async function handleWorkspaceFoldersAdded(
  newWorkspaceFolders: readonly vscode.WorkspaceFolder[],
  runningExecutable: LSExecutable | undefined,
  ctx: Context,
) {
  const newExecutables = await getLSExecutables(newWorkspaceFolders, ctx);
  if (!newExecutables.length) {
    return;
  }

  // Check if new ones are of same provider
  const newExecutablesHaveSameProvider = await allUnderlyingCommandsEqual(newExecutables, ctx);

  if (newExecutablesHaveSameProvider) {
    // If there is no process running, so we can safely start up a new one
    if (!runningExecutable) {
      ctx.log.debug(
        `Added folder(s) (${newWorkspaceFolders.map((x) => x.uri.fsPath)} ) have same provider - starting the client`,
      );
      await startClient(ctx);
      return;
    }

    // If there's a LS process running, we'd like to check if the added folder has the same LS
    const consistentWithPreviousLS = await allUnderlyingCommandsEqual(
      [...newExecutables, runningExecutable],
      ctx,
    );
    // If it's not, we need to stop LS and show an error
    if (!consistentWithPreviousLS) {
      await stopClient();
      vscode.window.showErrorMessage(
        "Added folder does not have the same scarb version. Please remove it from the workspace, or adjust the used scarb version.",
      );
    }
  }
}

export async function handleWorkspaceFoldersRemoved(
  runningExecutable: LSExecutable | undefined,
  ctx: Context,
) {
  if (!runningExecutable) {
    await startClient(ctx);
  }
}
