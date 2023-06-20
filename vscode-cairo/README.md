# Installation

## Install the Visual Studio Code extension

Use [NVM](https://github.com/nvm-sh/nvm#installing-and-updating) to get the correct version of Node
for this project. From the directory of this file, run:

```
nvm use
```

or install Node.js 18 LTS manually:
See troubleshooting section.

Still in the directory of this file, run:

```
sudo npm install --global @vscode/vsce
npm install
vsce package
code --install-extension cairo1*.vsix
```

## Using with Scarb bundled language server

The [Scarb] package manager comes with a bundled binary of the Cairo language server.
To use it, you need to install Scarb first.
Follow the installation instructions from [Scarb docs].

To make sure Scarb is installed properly, just run `scarb --version` in your terminal.
If Scarb is not in your system PATH variable, you can set it under `cairo1.scarbPath` in settings.

The VSCode extension will start language server from Scarb automatically.
You do not need to take any further steps.

## Install the Cairo language server

Alternatively, you can use a standalone installation of the Cairo language server.
Remember to build the language server:

```
cargo build --bin cairo-language-server --release
```

Now open vscode, find the Cairo extension and fill in the path to the cairo language server:

![image](./resources/img/extSettings.png)

It should look like:
`/path/cairo/target/release/cairo-language-server`
Where _path_ is the path to the cairo folder you got when cloning this repository.

### Having both types of language server installed

If you have both Scarb and standalone language server, the extension will decide which one to use.
The decision process is based on the type of project you run.

If your project includes the Scarb manifest file (`scarb.toml`), the one from Scarb will be used.
Otherwise, if you have set the path to language server in settings, a standalone server will be used.
You can also manually disable usage of Scarb through an option in vscode settings.

To check which language server has been started, examine logs from the extension.
It should say `Cairo language server running from{ Scarb} at: {path}` (the `Scarb` part is optional).

## Troubleshooting

### Building the extension

If `sudo npm install -g vsce` fails try this:

```
sudo apt remove nodejs
sudo apt update
sudo apt install curl dirmngr apt-transport-https lsb-release ca-certificates vim
curl -sL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt install nodejs
```

If successful, go back to `sudo npm install -g vsce` and continue from there.

### Corelib path resolution

In projects with Scarb manifest, the language server will obtain corelib from Scarb automatically.

In case your project is missing Scarb support, the language server will try to find corelib on disk.
The parent directories of the file you are currently editing will be traversed.
If you have corelib installed in a non-standard location, you can set the path in vscode settings.

# Run the extension (for development)

1. Open VSCode in the directory of this file.
2. Run:
   ```
   npm install
   npm run compile
   ```
3. Reload VSCode.
4. Press F5.

# Debugging

To make the "Debug"/"Run" CodeLens above your tests work - add this to your
~/.config/Code/User/settings.json:

```
"rust-analyzer.runnableEnv": {
   "CARGO_MANIFEST_DIR": "/path/to/workspace/repo_name/any_dir"
},
```

If you also want logs to be printed in your VSCode terminal when you click the "Debug"/"Run"
CodeLens above your tests, also add the "RUST_LOG" field:

```
"rust-analyzer.runnableEnv": {
   "CARGO_MANIFEST_DIR": "/path/to/workspace/repo_name/any_dir",
   "RUST_LOG": "debug,salsa=off,minilp=off"
},
```

Use `debug`/`trace` at your preference.

[Scarb]: http://github.com/software-mansion/scarb
[Scarb docs]: https://docs.swmansion.com/scarb
