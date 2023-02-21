#!/bin/bash

set -e

echo "compiling cairo => sierra..."
cargo run --bin cairo-compile -- --replace-ids /Users/karolbisztyga/Desktop/workspace/crypto/protostar/tests/integration/cairo1_compiler_bindings/library_functions_test/invoke_test.cairo ./target/output.sierra \
  && echo "done" || echo "failed"

echo "compiling sierra => casm..."
cargo run --bin cairo-protostar ./target/output.sierra ./target/out.json \
  && echo "done" || echo "failed"
# run tests
# cargo run --bin cairo-test -- -p ./examples

# cp ./target/out.json /Users/karolbisztyga/Desktop/workspace/crypto/protostar/tests/integration/cairo1/parser_test/compiled_test_suite.json
