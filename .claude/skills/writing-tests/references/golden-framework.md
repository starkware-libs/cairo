# Golden Tests (File-Based Test Framework)

This project uses a file-based golden test framework via `cairo_lang_test_utils`. Tests are defined in plain text files with a structured format, and the framework compares actual outputs against expected outputs.

## Test File Structure

Test data files are plain text with this format:

```
//! > Test description (this becomes the test name)

//! > test_runner_name
test_function_name(arg1: value1, arg2: value2)

//! > input_tag1
input content here

//! > input_tag2
more input content

//! > output_tag1
expected output content

//! > output_tag2
more expected output

//! > ==========================================================================

//! > Next test case...
```

Key points:
- `//! >` is the tag prefix
- First tag after separator is the test name/description
- `test_runner_name` specifies which runner function to use (with optional arguments)
- Tags are separated by blank lines
- Tests are separated by `//! > ==========================================================================`

## Cairo Input: `cairo_code` + `#[target_function]`

Compiler-stage golden tests (semantic, lowering, sierra-generator, runner) take their Cairo input
as a **single `cairo_code` tag holding the whole test module**. The legacy
`function_code`/`module_code`/`function_name` input tags no longer exist. When the runner operates
on one specific function, mark it with the `#[target_function]` attribute:

```
//! > test_runner_name
test_function_lowering(expect_diagnostics: false)

//! > cairo_code
fn helper() -> felt252 {
    5
}

#[target_function]
fn foo() -> felt252 {
    helper()
}
```

Rules:
- `setup_test_function(db, inputs)` (in `cairo_lang_semantic::test_utils`) reads `cairo_code` and
  resolves the single `#[target_function]`-marked free function. Marking several functions is
  supported at the resolution level (`resolve_target_functions` returns all, in definition order),
  but single-target runners panic on more than one.
- **Module-level runners need no marker** — diagnostics runners (`test_function_diagnostics`) and
  whole-module runners (e.g. `ap_change_test`) work on the entire module. Never add a stub like
  `fn foo() {}` just to satisfy a runner.
- The attribute is declared by the test-only `TargetFunctionPlugin`
  (`cairo_lang_semantic::test_utils`). Every golden-test database must include it — build plugin
  suites through `with_target_function_plugin(...)`; the standard `*DatabaseForTesting` types
  already do.
- `cairo_code` may be a `>>> file: <path>` reference. A referenced file that is also compiled
  outside the test framework (e.g. `examples/*.cairo`, compiled by `examples_test` and `cairo-run`
  with a plain production database) must put `#[allow_attr(target_function)]` directly above the
  `#[target_function]` marker, so the attribute is accepted without the plugin.
- Rust-side synthesized module code goes through `setup_test_function_from_content` with a
  `#[target_function]` marker embedded in the string — target functions are never resolved by name.
- Exception: the expr-family runners (`setup_test_expr`/`test_expr_diagnostics`, using
  `expr_code`/`function_body`/`module_code` tags) keep their own tag family; their synthesized
  wrapper function is marker-annotated internally.

## Creating a Test Runner

### Basic Pattern (Function-based)

```rust
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

cairo_lang_test_utils::test_file_test!(
    module_name,           // Creates a module with this name
    "src/path/to/test_data",  // Relative to crate root
    {
        test_name: "test_file",  // test_name -> test_file (no extension)
    },
    test_runner_function,
    ["allowed_arg1", "allowed_arg2"]  // Arguments that can be passed in test_runner_name
);

fn test_runner_function(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    // inputs: contains all tags from the test file
    // args: contains arguments from test_runner_name(arg: value)

    // ... run your test logic ...

    TestRunnerResult::success(OrderedHashMap::from([
        ("output_tag1".into(), output1),
        ("output_tag2".into(), output2),
    ]))
}
```

### With Argument Verification

**IMPORTANT**: Always specify allowed arguments in the macro. The framework will fail if a test uses an argument not in the allowed list.

```rust
cairo_lang_test_utils::test_file_test!(
    my_tests,
    "src/test_data",
    { basic: "basic_tests" },
    test_my_feature,
    ["expect_diagnostics", "some_flag"]  // MUST list all allowed args
);
```

Common arguments:
- `expect_diagnostics`: `true`, `false`, or `warnings_only` - use with `verify_diagnostics_expectation`

### Verifying Diagnostics Expectation

When using `expect_diagnostics`, use the helper function:

```rust
use cairo_lang_test_utils::verify_diagnostics_expectation;

fn test_runner(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    // ... get diagnostics string ...

    let error = verify_diagnostics_expectation(args, &diagnostics);

    TestRunnerResult {
        outputs: OrderedHashMap::from([...]),
        error,  // Will be Some(msg) if expectation violated
    }
}
```

### Stateful Runner Pattern (TestFileRunner trait)

For runners that need to maintain state across tests:

```rust
use cairo_lang_test_utils::parse_test_file::{TestFileRunner, TestRunnerResult};

cairo_lang_test_utils::test_file_test_with_runner!(
    module_name,
    "src/test_data",
    { test_name: "test_file" },
    MyRunner
);

#[derive(Default)]
struct MyRunner {
    db: LoweringDatabaseForTesting,
}

impl TestFileRunner for MyRunner {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        runner_args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        // Access self.db for shared state
        TestRunnerResult::success(...)
    }

    fn allowed_arg(&self, arg: &str) -> bool {
        matches!(arg, "expect_diagnostics" | "some_flag")
    }
}
```

## Running and Fixing Tests

### Running Tests

```bash
cargo test -p cairo-lang-lowering test_module_name
```

### Fixing Test Outputs (CAIRO_FIX_TESTS)

When test outputs change intentionally:

```bash
CAIRO_FIX_TESTS=1 cargo test -p <crate> <test_name>
```

This updates the test file with actual outputs.

**CRITICAL**: After using `CAIRO_FIX_TESTS=1`:
1. **Always review the diff** - compare against the state before your branch
2. Use `git diff` to see exactly what changed in test data files
3. Verify changes are expected and intentional
4. Never blindly commit fixed tests without review

### Filtering Tests

```bash
CAIRO_TEST_FILTER="test_name_substring" cargo test -p <crate> <test_module>
```

### Skip Formatting Checks

```bash
CAIRO_SKIP_FORMAT_TESTS=1 cargo test ...
```

## Test Data File Locations

Convention in this codebase:
- Main tests: `src/test_data/`
- Optimization tests: `src/optimizations/test_data/`
- Inline tests: `src/inline/test_data/`
- Analysis tests: `src/analysis/test_data/`

## Common Patterns

### Before/After Comparison (Optimizations)

```rust
fn test_optimization(inputs: &OrderedHashMap<String, String>, _args: &...) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, _) = setup_test_function(db, inputs).split();

    let lowered = db.lowered_body(...);
    let before = formatted_lowered(db, Some(lowered));

    let mut modified = (*lowered).clone();
    run_optimization(&mut modified);
    let after = formatted_lowered(db, Some(&modified));

    TestRunnerResult::success(OrderedHashMap::from([
        ("before".into(), before),
        ("after".into(), after),
    ]))
}
```

### With Diagnostics

```rust
fn test_with_diagnostics(inputs: &OrderedHashMap<String, String>, args: &...) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();

    // ... run test ...

    let lowering_diagnostics = db.module_lowering_diagnostics(...).format(db);
    let combined = format!("{semantic_diagnostics}\n{lowering_diagnostics}");
    let error = verify_diagnostics_expectation(args, &combined);

    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("semantic_diagnostics".into(), semantic_diagnostics),
            ("lowering_diagnostics".into(), lowering_diagnostics),
            ("output".into(), output),
        ]),
        error,
    }
}
```

## Checklist for New Test Runners

1. [ ] Define the test runner function with correct signature
2. [ ] Use `test_file_test!` or `test_file_test_with_runner!` macro
3. [ ] **Specify all allowed arguments** in the macro (even if empty: `[]`)
4. [ ] Create test data file in appropriate `test_data/` directory
5. [ ] Include `test_runner_name` tag in every test case
6. [ ] Run tests to verify they pass
7. [ ] If using `CAIRO_FIX_TESTS=1`, review all changes before committing

## Verifying All Failure Paths

When creating a new runner, ensure tests fail appropriately:

1. **Invalid arguments**: Test passes an argument not in `allowed_args` → should fail
2. **Diagnostic expectations**:
   - `expect_diagnostics: true` with no diagnostics → should fail
   - `expect_diagnostics: false` with diagnostics → should fail
   - `expect_diagnostics: warnings_only` with errors → should fail
3. **Output mismatch**: Actual output differs from expected → should fail

Run without `CAIRO_FIX_TESTS` to verify failures are caught.
