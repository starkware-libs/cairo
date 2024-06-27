#!/bin/bash

npm --prefix vscode-cairo run gen-cairo-snippets

if [[ $(git diff --stat) != '' ]]; then
  echo 'The corpus of Cairo snippets used in syntax highlighting test in vscode-cairo is out of date.'
  echo 'Please run `npm run gen-cairo-snippets` in vscode-cairo and commit the changes.'
  exit 1
fi
