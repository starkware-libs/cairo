# Contributing to the Cairo extension for Visual Studio Code

This document is a supplement to the [general contributing guidelines](../docs/CONTRIBUTING.md) for
the Cairo repository.
Please read that document first carefully before proceeding with the instructions below.

You might also want to check out
the [CairoLS contributing guidelines](../crates/cairo-lang-language-server/CONTRIBUTING.md).

## Development environment setup

There are three ways you can develop and run the extension.

1. ### Develop and debug from within VS Code

   Open the `vscode-cairo` directory in VS Code and press `F5` to start debugging.
   Everything should be already set up in the `vscode-cairo/.vscode` directory.

2. ### Develop in your editor of choice and run in the VS Code Extension Host

   If you happen to make the most of your development in your editor of choice (like IntelliJ or
   Helix), you can run the VS Code Extension Host from the command line, like this:

   ```sh
   # Build the extension.
   $ npm --prefix vscode-cairo run compile

   # Run the extension in the VS Code Extension Host.
   $ code "--extensionDevelopmentPath=$PWD/vscode-cairo" --wait --verbose
   ```

   The `--wait --verbose` arguments make the command wait until the Extension Host is closed.
   You can skip them if you do not want to block your terminal.

3. ### Package the extension manually and install in VS Code

   This technique is useful if you are not interested in developing the extension itself,
   but you need some unreleased changes when working on the Cairo compiler overall.

   ```sh
   # Install vsce.
   $ npm install -g vsce
   # Or, if you're using macOS.
   $ brew install vsce

   # Package the extension.
   $ cd vscode-cairo
   $ vsce package

   # Install the extension in your VS Code installation.
   # The `<version>` part will vary depending on the HEAD you are working on.
   $ code --install-extension cairo1-<version>.vsix
   ```

## Commits

If your commit/pull request is solely related to the VSCode extension, please prefix your commit
message/PR title with `VSCode: `.
