#!/bin/bash

CURRENT_VERSION='1.0.0'
NEW_VERSION="$@"
sed -i "s/$CURRENT_VERSION/$NEW_VERSION/g" $(find . -type f -iname "*.toml") ./scripts/bump_version.sh
