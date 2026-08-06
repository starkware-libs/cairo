# Bug Hunt Findings — Hunter #5 (test collection & running)

Scope: `crates/cairo-lang-test-runner/src/` and `crates/cairo-lang-test-plugin/src/`
(test config parsing, test filtering, result aggregation).

## Bug 1: `FAILED` test-run summary silently drops the "filtered out" count

**File + location**: `crates/cairo-lang-test-runner/src/lib.rs`, `CompiledTestRunner::run`,
lines 154–187 (success message at line 156, failure message at line 181).

```rust
if failed.is_empty() {
    println!(
        "test result: {}. {} passed; {} failed; {} ignored; {filtered_out} filtered out;",
        "ok".bright_green(),
        passed.len(),
        failed.len(),
        ignored.len()
    );
    Ok(None)
} else {
    println!("failures:");
    ...
    bail!(
        "test result: {}. {} passed; {} failed; {} ignored",
        "FAILED".bright_red(),
        passed.len(),
        failed.len(),
        ignored.len()
    );
}
```

**Description**: When all tests pass, the final summary line includes the number of tests
that were filtered out (by `--filter`, `--ignored`, or `--include-ignored` semantics via
`filter_test_cases`). When at least one test fails, the final `bail!` message uses a
different format string that has no `{filtered_out}` placeholder at all — the count is
computed (`filter_test_cases` at line 140 returns it) but is simply never included in the
failure-path message. A user running `cairo-test --filter foo` against a suite with a
failing test has no way to tell from the final summary line how many tests were excluded by
the filter, whereas the same run succeeding would show it. This is an aggregation/reporting
inconsistency between the two code paths of the same function.

**Root cause**: The `bail!(...)` format string at line 181 omits the `{filtered_out}`
interpolation that the sibling `println!(...)` at line 156 includes, even though both paths
have `filtered_out` in scope (bound at line 140-145) and conceptually should report the same
information.

**How I verified this** (concretely, not just by inspection): I temporarily added the test
below into `crates/cairo-lang-test-runner/src/test.rs`, ran it with
`cargo test -p cairo-lang-test-runner --lib test_failure_summary_reports_filtered_out_count`,
observed it fail as expected, confirming the bug, and then reverted the file (via
`git checkout`) to leave the repo clean — no permanent change was made to the repo.

Actual captured output from the failing run:
```
test result: FAILED. 1 passed; 1 failed; 0 ignored
```
(note: "0 filtered out" nowhere in the message, even though `excluded_by_filter` — one of the
three tests in the fixture — was filtered out by the `filter: "keep"` config; the real
filtered count in this repro was 6, not 0, since the fixture's crate root pulls in a few
compiler-synthesized entries, but regardless of the exact number, the phrase "filtered out"
never appears in the FAILED message).

**Full test code** (uses only the crate's own public API — `TestRunner`/`TestRunConfig` —
exactly like the existing tests in the same file; no internals-poking):

```rust
// Add to crates/cairo-lang-test-runner/src/test.rs (uses only std + the crate's public API).
use crate::{TestRunConfig, TestRunner};

/// Creates a throwaway Cairo project directory with the given `lib.cairo` contents.
fn make_temp_project(lib_cairo: &str) -> std::path::PathBuf {
    let mut dir = std::env::temp_dir();
    dir.push(format!(
        "cairo_test_runner_hunt_{}_{}",
        std::process::id(),
        std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos()
    ));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(
        dir.join("cairo_project.toml"),
        "[crate_roots]\ncontracts = \".\"\n\n[config.global]\nedition = \"2023_11\"\n",
    )
    .unwrap();
    std::fs::write(dir.join("lib.cairo"), lib_cairo).unwrap();
    dir
}

/// Regression test: when tests fail *and* some tests were filtered out, the final "FAILED"
/// summary line should report the filtered-out count, just like the "ok" summary line does.
#[test]
fn test_failure_summary_reports_filtered_out_count() {
    let dir = make_temp_project(
        r#"
        #[test]
        fn keep_pass() {
            assert!(true);
        }

        #[test]
        fn keep_fail() {
            assert!(false);
        }

        #[test]
        fn excluded_by_filter() {
            assert!(true);
        }
        "#,
    );

    let config = TestRunConfig {
        filter: "keep".to_string(),
        include_ignored: false,
        ignored: false,
        profiler_config: None,
        gas_enabled: true,
        print_resource_usage: false,
    };

    let runner = TestRunner::new(&dir, false, false, config).unwrap();
    let message = match runner.run() {
        Err(err) => err.to_string(),
        Ok(_) => panic!("expected the run to fail because `keep_fail` fails its assertion"),
    };

    // `excluded_by_filter` was filtered out by `filter: "keep"`, so the failure summary
    // should mention it, the same way the passing-path summary does (lib.rs:156).
    assert!(
        message.contains("filtered out"),
        "expected the FAILED summary to report the filtered out test count, got: {message:?}"
    );
}
```

**How to verify**: Drop the test above into
`crates/cairo-lang-test-runner/src/test.rs` and run
`cargo test -p cairo-lang-test-runner --lib test_failure_summary_reports_filtered_out_count -- --nocapture`.
It fails today with an assertion showing the captured message
`"test result: FAILED. 1 passed; 1 failed; 0 ignored"` (no "filtered out" substring at all).
The fix is simply to add `; {filtered_out} filtered out` to the `bail!` format string at
line 181, mirroring line 156.

**Severity**: Low/cosmetic (CLI reporting only, no incorrect pass/fail verdicts), but a
genuine, easily fixed, currently-real discrepancy in result aggregation/reporting.

---

## Secondary observation (edge case, not filed as a full bug — low confidence)

**File + location**: `crates/cairo-lang-test-plugin/src/test_config.rs`,
`try_extract_test_config`, lines 50–53:

```rust
let test_attr = attrs.iter().find(|attr| attr.id.long(db) == TEST_ATTR);
let ignore_attr = attrs.iter().find(|attr| attr.id.long(db) == IGNORE_ATTR);
let available_gas_attr = attrs.iter().find(|attr| attr.id.long(db) == AVAILABLE_GAS_ATTR);
let should_panic_attr = attrs.iter().find(|attr| attr.id.long(db) == SHOULD_PANIC_ATTR);
```

**Description**: `Iterator::find` returns only the *first* attribute matching each name. If a
function carries the same test-configuration attribute more than once (e.g. two
`#[available_gas(...)]` attributes, or two `#[should_panic(...)]` attributes, or two
`#[ignore]`/`#[test]` attributes — all syntactically legal to write), every attribute after
the first is silently ignored: no diagnostic is emitted about the duplicate/conflicting
attribute, and the test's behavior depends on source-order in the attribute list without any
indication to the author that the second attribute had no effect.

**Root cause**: no duplicate-attribute detection anywhere in `try_extract_test_config`; only
per-attribute argument well-formedness is checked, not attribute-count well-formedness.

I have **not** demonstrated this concretely (would need to compile a fixture with duplicated
attributes and confirm no diagnostic + confirm which value "wins"; plausible but not
guaranteed to be considered a bug versus accepted/unspecified behavior, since duplicating a
config attribute is unusual user error to begin with). Labeling this **suspected, not
demonstrated** and reporting it only as a secondary, low-confidence observation rather than a
primary finding.

---

## Investigated and found correct (no bug)

For completeness, areas closely read but found to match their documented/tested behavior:

- `filter_test_cases` (`lib.rs:279-307`) ignored/include-ignored/filter interaction — matches
  all four existing unit tests (`test_filter_test_cases*`) exactly, including the
  double-flag (`ignored && include_ignored`) case.
- `extract_available_gas` (`test_config.rs:126-167`) — the `static` branch does an early
  `return None` from the whole function (bypassing `.on_none()`), and the suffixed-literal
  branch also does an early `return None` after pushing its own diagnostic — so, contrary to
  first appearance, neither produces a double diagnostic by itself.
- `extract_panic_bytes` / should_panic exact-match comparison in
  `run_single_test` (`lib.rs:459-467`) — `PanicExpectation::Exact` vs `Any` dispatch is
  correct; tuple/string/short-string/literal encoding in `extract_string_panic_bytes` matches
  the decode side exercised by `test_format_for_panic`.
- The `unwrap_or_default()` on `TerminalShortString::numeric_value` in
  `extract_panic_bytes` (`test_config.rs:192,211`) silently turns a malformed short string
  (non-ASCII or bad escape) into felt `0` with no diagnostic — but this exactly mirrors the
  identical `unwrap_or_default()` pattern in `cairo-lang-semantic`'s
  `short_string_to_semantic` (`crates/cairo-lang-semantic/src/expr/compute.rs:3782`), i.e. it
  is consistent with how the rest of the compiler treats short strings everywhere, not a
  test-plugin-specific discrepancy — did not file.
- `zip_eq` pairing of `failed` names with `failed_run_results` (`lib.rs:165`) — traced that
  every push to `failed`/`summary.failed_run_results` happens in lockstep per
  `update_summary` call; cannot desync.
- Double-diagnostic pattern (attribute-specific well-formedness checked unconditionally even
  when `#[test]` is absent, producing two diagnostics for one malformed non-test attribute) —
  present for `available_gas`, `should_panic`, and `ignore` alike as the same root cause; the
  `available_gas` instance of this is already filed per team memory, so did not re-file any
  attribute instance of this shared root cause.

## Files checked

- `/home/user/cairo/crates/cairo-lang-test-runner/src/lib.rs`
- `/home/user/cairo/crates/cairo-lang-test-runner/src/test.rs`
- `/home/user/cairo/crates/cairo-lang-test-runner/test_data/lib.cairo`
- `/home/user/cairo/crates/cairo-lang-test-runner/test_data/cairo_project.toml`
- `/home/user/cairo/crates/cairo-lang-test-runner/Cargo.toml`
- `/home/user/cairo/crates/cairo-lang-test-plugin/src/lib.rs`
- `/home/user/cairo/crates/cairo-lang-test-plugin/src/test_config.rs`
- `/home/user/cairo/crates/cairo-lang-test-plugin/src/plugin.rs`
- `/home/user/cairo/crates/cairo-lang-test-plugin/src/inline_macros/assert.rs` (skimmed;
  belongs more to macro/plugin codegen than test-config/filtering/aggregation, deprioritized)
- `/home/user/cairo/crates/bin/cairo-test/src/main.rs` (CLI arg wiring into `TestRunConfig`)
- `/home/user/cairo/crates/cairo-lang-syntax/src/node/ast_ext.rs` (helper methods used by
  `test_config.rs`, e.g. `numeric_value_and_suffix`, `numeric_value`, `string_value` —
  read to understand callers' behavior, not to file syntax-crate bugs)
- `/home/user/cairo/crates/cairo-lang-semantic/src/expr/compute.rs` (spot-checked
  `short_string_to_semantic` for comparison against `test_config.rs`'s identical pattern)
- Did **not** read `crates/cairo-lang-syntax/src/node/ast.rs` (per CLAUDE.md instruction).

Confirmed via actual build+test run (`cargo test -p cairo-lang-test-runner --lib`) that the
crate's existing 6 unit tests all pass, and confirmed Bug 1 with a temporary test that was
reverted afterward (repo left clean, `git status` verified empty for the touched file).
