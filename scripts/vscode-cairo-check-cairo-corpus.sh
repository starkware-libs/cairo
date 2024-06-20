#!/bin/bash

npm --prefix vscode-cairo run gen-cairo-corpus

if [[ $(git diff --stat) != '' ]]; then
  echo 'The Cairo corpus used in syntax highlighting test in vscode-cairo is out of date.'
  echo 'Please run `npm run gen-cairo-corpus` in vscode-cairo and commit the changes.'
  exit 1
fi
