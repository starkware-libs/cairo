#!/bin/bash

CURRENT_VERSION='1.1.0-alpha0'
NEW_VERSION="$@"
sed -i "s/$CURRENT_VERSION/$NEW_VERSION/g" $(find . -type f -iname "*.toml") ./scripts/bump_version.sh
