#!/bin/bash

set -e

echo "compiling cairo => sierra..."
cargo run --bin cairo-compile -- --replace-ids examples/cheatcode_caller.cairo ./target/output.sierra
echo "compiling sierra => casm done"

echo "compiling sierra => casm..."
cargo run --bin sierra-compile -- ./target/output.sierra ./target/out.json
echo "compiling sierra => casm done"
