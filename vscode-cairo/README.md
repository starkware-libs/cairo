# Installation

Install Node.js 18 LTS:
See troubleshooting section.

From the directory of this file, run:
```
sudo npm install -g vsce
npm install
vsce package
code --install-extension cairo*.vsix
```

Remember to build the language server:
```
cargo bin --bin languageserver --release
```

# Run the extension (for development)

1. Open VSCode in the directory of this file.
2. Run:
   ```
   npm install
   npm run compile
   ```
3. Reload VSCode.
4. Press F5.
