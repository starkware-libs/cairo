# Rust test mechanics (this repo)

Self-sufficient reference: skeletons, commands, bless workflow, fixtures.

## Unit tests — the sibling-file idiom

Production file `foo.rs` declares its tests out-of-line in `foo_test.rs`,
module named `test` (singular). This is the house idiom (76 occurrences);
do not add inline `mod tests` blocks.

```rust
// in foo.rs, at the top with the other mod declarations:
#[cfg(test)]
#[path = "foo_test.rs"]
mod test;
```

```rust
// foo_test.rs
use super::*;

#[cairo_lang_test_utils::test]   // #[test] + init_logging; use instead of bare #[test]
fn behavior_under_condition() {
    assert_eq!(compute_span_end(...), expected);
}
```

Parameterized tests use the `test-case` crate (workspace dev-dependency):

```rust
#[test_case("account::account"; "account contract")]
#[test_case("erc20::erc_20"; "erc20")]
fn compiles(path: &str) { ... }
```

`rstest` (`#[fixture]`, `#[once]`) is used only in the top-level `tests/`
crate; do not introduce it elsewhere.

Run:

```sh
cargo test -p cairo-lang-semantic                 # one crate (CI adds --profile=ci-dev)
cargo test -p cairo-lang-semantic resolve         # filter by substring
cargo test -p tests e2e                           # e2e suites
```

Doctests run as part of plain `cargo test` (the repo does not use nextest),
so `///`-example code in utils crates does execute in CI.

## Golden tests — placement and workflow

Mechanics of the `//! >` file format and creating new runners are in
`references/golden-framework.md`; this section is what you need to add cases
and bless.

- Add a new case to the existing test_data file for the stage you changed:
  parser → `crates/cairo-lang-parser/src/parser_test_data/`, semantic
  diagnostics → `crates/cairo-lang-semantic/src/diagnostic_test_data/`,
  lowering → `crates/cairo-lang-lowering/src/**/test_data/`, sierra-gen →
  `crates/cairo-lang-sierra-generator/src/*_test_data/`, starknet plugin →
  `crates/cairo-lang-starknet/src/plugin/plugin_test_data/`, e2e →
  `tests/e2e_test_data/`. New file (and a matching entry in the crate's
  `test_file_test!` invocation) only when no existing file fits.
- Keep each case minimal and single-feature so future diffs are attributable;
  name the case for behavior + condition.
- Write the case with an empty/stale expected block, then bless it.

Bless workflow — the diff review after blessing is the test:

```sh
# 1. Bless, narrowly — filter to the tests you mean to update:
CAIRO_FIX_TESTS=1 cargo test -p cairo-lang-semantic diagnostics
# optionally narrow to one case inside a file:
CAIRO_FIX_TESTS=1 CAIRO_TEST_FILTER=my_case_name cargo test -p cairo-lang-semantic diagnostics

# 2. Read every hunk before staging — each must be explained by your change:
git diff -- '*test_data*'

# 3. Re-run without the env var to confirm green:
cargo test -p cairo-lang-semantic diagnostics
```

`CAIRO_SKIP_FORMAT_TESTS=1` skips the auto-format check of `cairo_code`-style
input tags while iterating; never commit with it set.

Never run a wholesale `CAIRO_FIX_TESTS=1 cargo test` over the workspace to
"fix CI" — bless only the suites your change is supposed to affect, and
justify every changed hunk in the PR.

## Shared fixtures — use, don't rebuild

- `cairo_lang_semantic::test_utils`: `SemanticDatabaseForTesting`,
  `setup_test_module`, `setup_test_function` (resolves the `#[target_function]`-marked
  function in the `cairo_code` input; see golden-framework.md).
- `cairo_lang_lowering::test_utils`: `LoweringDatabaseForTesting`
  (`::new()`, `.snapshot()`, `.with_no_gas()`, ...).
- `cairo_lang_parser::test_utils`: `get_diagnostics`, `create_virtual_file`.
- `cairo_lang_test_utils`: `compare_contents_or_fix_with_path` (single-file
  golden compare, also honors `CAIRO_FIX_TESTS`), `test_lock`.
- e2e tests share `LazyLock<Mutex<RootDatabase>>` statics to amortize corelib
  compilation — reuse them; never mutate the shared DB in a test.

Building a fresh `RootDatabase` per test is the main way to make a suite
slow; the fixtures exist so you don't.

## Pitfalls this repo actually hits

- Helper logic (span math, id keys, counters) is invisible to golden suites —
  give it a direct unit test in the sibling `_test.rs` (#10017, #10003
  escaped exactly here).
- A `let _ =` on a `Result`/`Maybe` that can carry a diagnostic is a bug
  factory (#10007); when you must ignore one, add the test that proves the
  error path is otherwise reported.
- Tests in `crates/bin/*` never run in CI (bin crates are outside the test
  matrix — `get-lowering`'s consistency test is dead today). Put logic and
  its tests in a library crate; keep bins thin.
- An opt-in flag no test enables is untested code (#9957): when adding a
  flag, add one test that runs with it on.
- When adding a helper that shadows a similar sibling (`is_fully_concrete`
  next to `is_var_free`), write the unit test on an input where they
  disagree; a copy-pasted body passes every test that treats them alike
  (#10010).
