import * as vscode from "vscode";
import { Context } from "./context";

export interface ProgressNotificationParams {
    id: String, 
    value: "ProgressEnd" | {"ProgressBegin": {title: String}}
}

export class ProgressStatusBar {
    private statusBarItem: vscode.StatusBarItem;
    private hiding: NodeJS.Timeout | undefined;
    
    constructor(private context: Context) {
        this.statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 0);
        this.statusBarItem.hide();
        this.context.extension.subscriptions.push(this.statusBarItem);
    }

    // Show the progress bar with a given message
    public startProgress(title: String): void {
        if (this.hiding) {
            clearTimeout(this.hiding);
            this.hiding = undefined;
        }

        let new_title = `$(loading~spin) ${title}`;
        this.statusBarItem.text = new_title;
        this.statusBarItem.show();
    }
    
    // Hide the progress bar entirely
    public endProgress() { 
        if (this.hiding) {
            return; 
        }

        // Delay hiding to avoid flickering
        this.hiding = setTimeout(() => {
            this.statusBarItem.hide();
            this.hiding = undefined;
        }, 200);
    }
}
