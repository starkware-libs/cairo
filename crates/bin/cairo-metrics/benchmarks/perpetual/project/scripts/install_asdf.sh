#!/bin/bash
pushd $(dirname $0)/..

set -e  # Exit immediately if a command fails

# Define the ASDF installation directory (Default: $HOME/.asdf)
ASDF_DIR="$HOME/.asdf"

# Clone asdf repository if not already installed
if [ ! -d "$ASDF_DIR" ]; then
    echo "Installing asdf..."
    git clone https://github.com/asdf-vm/asdf.git "$ASDF_DIR" --branch v0.14.0
else
    echo "asdf is already installed at $ASDF_DIR"
fi

# Make asdf available in future GitHub Actions steps
echo "$HOME/.asdf/bin" >> "$GITHUB_PATH"
echo "$HOME/.asdf/shims" >> "$GITHUB_PATH"
echo "ASDF_DIR=$HOME/.asdf" >> "$GITHUB_ENV"


popd
