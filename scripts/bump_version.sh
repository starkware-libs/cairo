#!/bin/bash

CURRENT_VERSION='2.0.2'
NEW_VERSION="$@"
sed -i "s/$CURRENT_VERSION/$NEW_VERSION/g" \
    $(find . -type f -iname "*.toml") \
    ./scripts/bump_version.sh \
    ./vscode-cairo/package.json
