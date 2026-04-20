# Ordered list of crates to publish. A crate must appear after all its dependencies.
# Sourced by release_crates.sh and yank_crates.sh.
CRATES=(
    cairo-lang-utils
    cairo-lang-debug
    cairo-lang-proc-macros
    cairo-lang-filesystem
    cairo-lang-diagnostics
    cairo-lang-syntax
    cairo-lang-syntax-codegen
    cairo-lang-parser
    cairo-lang-defs
    cairo-lang-formatter
    cairo-lang-test-utils
    cairo-lang-casm
    cairo-lang-eq-solver
    cairo-lang-project
    cairo-lang-sierra
    cairo-lang-sierra-type-size
    cairo-lang-sierra-ap-change
    cairo-lang-sierra-gas
    cairo-lang-sierra-to-casm
    cairo-lang-plugins
    cairo-lang-semantic
    cairo-lang-lowering
    cairo-lang-sierra-generator
    cairo-lang-runnable-utils
    cairo-lang-compiler
    cairo-lang-starknet-classes
    cairo-lang-starknet
    cairo-lang-executable-plugin
    cairo-lang-executable
    cairo-lang-runner
    cairo-lang-test-plugin
    cairo-lang-test-runner
    cairo-lang-execute-utils
    cairo-lang-doc
    cairo-compile
    cairo-format
    cairo-run
    cairo-execute
    cairo-test
    cairo-size-profiler
    sierra-compile
    starknet-compile
    starknet-sierra-compile
)

# Assert that the list is kept in sync with the workspace
# (workspace crate count minus 5 crates that are for internal use only).
NUM_CRATES_IN_WORKSPACE=$(find crates/ -name Cargo.toml | wc -l)
if [ "${#CRATES[@]}" -ne "$((NUM_CRATES_IN_WORKSPACE - 5))" ]; then
    echo "The number of crates to publish is not equal to the number of crates in the workspace,"
    echo "new crates were probably added, please update the list of crates to publish."
    exit 1
fi
