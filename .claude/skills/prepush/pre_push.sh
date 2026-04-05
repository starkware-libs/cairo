#!/bin/bash

# Change to the repo root
cd "$(git rev-parse --show-toplevel)" || exit 1

# Run the scripts in sequence.
./scripts/rust_fmt.sh
./scripts/clippy.sh
./scripts/docs.sh
typos
taplo format
cargo machete

# Exit with error if any command failed
exit $?
