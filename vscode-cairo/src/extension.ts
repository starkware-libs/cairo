import * as vscode from 'vscode';
import { SemanticTokensFeature } from 'vscode-languageclient/lib/common/semanticTokens';
import * as child_process from 'child_process';
import * as fs from 'fs';

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from 'vscode-languageclient/node';

// Tries to find the development version of the language server executable,
// assuming the workspace directory is inside the Cairo repository.
function findDevLanguageServerAt(path: string, depth: number): string | undefined {
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

function findLanguageServerExecutable(
    config: vscode.WorkspaceConfiguration, context: vscode.ExtensionContext) {

    let rootPath = context.extensionPath;

    let workspaceFolders = vscode.workspace.workspaceFolders;
    if (workspaceFolders) {
        rootPath = workspaceFolders[0].uri.path || rootPath
    }

    var configPath = config.get<string>('cairo1.languageServerPath');
    if (configPath) {
        // Replace placeholders, if present.
        return configPath.replace(/\${workspaceFolder}/g, rootPath);
    }

    // TODO(spapini): Use a bundled language server.
    return findDevLanguageServerAt(rootPath, 10)
}

function setupLanguageServer(
    config: vscode.WorkspaceConfiguration, context: vscode.ExtensionContext) {

    let outputChannel = vscode.window.createOutputChannel("Cairo extension");
    context.subscriptions.push(outputChannel);

    let serverOptions: ServerOptions = () => {
        return new Promise((resolve) => {
            let executable = findLanguageServerExecutable(config, context);
            if (!executable) {
                outputChannel.appendLine(
                    "Cairo language server was not found. Make sure cairo-lang-server is " +
                    "installed and that the configuration 'cairo1.languageServerPath' is correct.");
                return;
            }
            outputChannel.appendLine("Cairo language server running from: " + executable);
            let child = child_process.spawn(executable);
            // Forward stderr to vscode logs.
            child.stderr.on('data', (data: Buffer) => {
                outputChannel.appendLine("Server stderr> " + data.toString());
            });
            child.on('exit', (code, signal) => {
                outputChannel.appendLine('Cairo language server exited with code ' + code +
                    ' and signal' + signal);
            });

            // Create a resolved promise with the child process.
            resolve(child);
        });
    }

    let clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'cairo' }],
    };

    var client: LanguageClient = new LanguageClient(
        'cairoLanguageServer', 'Cairo Language Server', serverOptions, clientOptions);
    client.registerFeature(new SemanticTokensFeature(client));
    client.start();
}

export function activate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration();

    if (config.get<boolean>('cairo1.enableLanguageServer')) {
        setupLanguageServer(config, context);
    }
}
