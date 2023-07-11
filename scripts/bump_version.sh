#!/bin/bash

<<<<<<< HEAD
CURRENT_VERSION='2.0.2'
||||||| 1003d5d14
CURRENT_VERSION='1.1.0'
=======
CURRENT_VERSION='1.1.1'
>>>>>>> dev-v1.1.1
NEW_VERSION="$@"
sed -i "s/$CURRENT_VERSION/$NEW_VERSION/g" \
    $(find . -type f -iname "*.toml") \
    ./scripts/bump_version.sh \
    ./vscode-cairo/package.json
