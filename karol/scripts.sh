#!/usr/bin/env bash
set -e

# cairo -> sierra
cargo run --bin cairo-compile -- examples/enum_flow.cairo karol/output.sierra --replace-ids
# sierra -> casm
cargo run --bin sierra-compile -- karol/output.sierra karol/output.casm

# run cairo directly
cargo run --bin cairo-run -- -p examples/enum_flow.cairo