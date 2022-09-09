# Installation

From the directory of this file, run:
```
sudo npm install -g vsce
npm install
vsce package
code --install-extension cairo*.vsix
```

Remember to build the language server:
```
cargo build --bin languageserver --release
```

## Troubleshooting

If `sudo npm install -g vsce` fails try this:
```
sudo apt remove nodejs
sudo apt update
sudo apt install curl dirmngr apt-transport-https lsb-release ca-certificates vim
curl -sL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt install nodejs
```
If successful, go back to `sudo npm install -g vsce` and continue from there.

# Run the extension (for development)

1. Open VSCode in the directory of the extension.
2. Run:
   ```
   npm install
   npm run compile
   ```
3. Reload VSCode.
4. Press F5.
