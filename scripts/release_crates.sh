# An optional argument, '-s | --skip-first <#num_to_skip>', can be passed to skip the first #num_to_skip crates, otherwise SKIP_FIRST is set to 0.
if [ "$1" == "-s" ] || [ "$1" == "--skip-first" ]; then
    if [ -z "$2" ]; then
        echo "Error: --skip-first requires a numeric argument."
        exit 1
    fi
    SKIP_FIRST=$2
else
    SKIP_FIRST=0
fi

# Define a list of the crates to be published.
# The order of the crates is important, as a crate must be published after its dependencies.
CRATES_TO_PUBLISH=(
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
    cairo-lang-compiler
    cairo-lang-starknet-classes
    cairo-lang-starknet
    cairo-lang-runnable-utils
    cairo-lang-executable
    cairo-lang-runner
    cairo-lang-test-plugin
    cairo-lang-test-runner
    cairo-lang-doc
    cairo-compile
    cairo-format
    cairo-run
    cairo-execute
    cairo-test
    sierra-compile
    starknet-compile
    starknet-sierra-compile
)

# Assert that the number of crates to publish is equal to the number of crates in the workspace
# - 5 (the number of crates that are for internal use only).
NUM_CRATES_IN_WORKSPACE=$(find crates/ -name Cargo.toml | wc -l)
if [ "${#CRATES_TO_PUBLISH[@]}" -ne "$((NUM_CRATES_IN_WORKSPACE - 5))" ]; then
    echo "The number of crates to publish is not equal to the number of crates in the workspace,"
    echo "new crates were probably added, please update the list of crates to publish."
    exit 1
fi

# Publish the crates.
for CRATE in "${CRATES_TO_PUBLISH[@]:$SKIP_FIRST}"; do
    cargo publish --package "$CRATE" || exit 1
done
