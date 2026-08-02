//! TEMPORARY migration helper - not part of this crate's stable API.
//!
//! Mechanically rewrites golden test-data files (the `//! > tag` files parsed by
//! [`crate::parse_test_file`]) from the legacy tag families:
//!   * `function_code` (+ optional `module_code`) (+ `function_name`)
//!   * `cairo_code` + `function_name`
//!
//! to the unified single `cairo_code` tag (with a `#[target_function]` marker where a marker is
//! actually needed), matching the dual-format contract landed in
//! `cairo_lang_semantic::test_utils` (see `test_module_code`, `setup_test_function`,
//! `TARGET_FUNCTION_ATTR`). `expr_code`/`function_body` family files are left completely
//! untouched - that family isn't part of this migration.
//!
//! This is a mechanical *text* rewrite - it never invokes the Cairo parser or compiler. Anchoring
//! a `#[target_function]` marker on the right `fn` item is done with a small brace-depth scanner
//! (see [`insert_target_function_marker`]) that only matches a `fn <name>` line sitting at
//! brace-depth 0 (i.e. a real top-level/free-function item, the same population
//! `module_free_functions_ids` - and so `resolve_target_functions` - draws from; `impl`/`mod`
//! bodies are always at depth >= 1). Zero or more-than-one such matches are refused rather than
//! guessed at; see [`RewriteOutcome::NeedsManualReview`].
//!
//! ## Usage
//! As a library, from another (throwaway) test/bin in this workspace:
//! ```ignore
//! use cairo_lang_test_utils::migrate_target_function::migrate_file;
//! let report = migrate_file(std::path::Path::new(
//!     "crates/cairo-lang-sierra-generator/src/function_generator_test_data/simple",
//! ))?;
//! println!("{report:?}");
//! ```
//! `migrate_file` rewrites the file in place and returns a report of what happened per test.
//! Rewriting is per-test and all-or-nothing per test: any test flagged `NeedsManualReview` is
//! left byte-for-byte untouched (its tags are dumped back exactly as parsed) while the rest of
//! the file's tests are still safely rewritten - ambiguity in one test never blocks progress on
//! the others, and it's never silently guessed at either.
//!
//! Ad hoc, from the command line (no binary target is wired up - this is deliberately only
//! reachable through `cargo test`, since it depends on this crate's `testing`-gated code):
//! ```text
//! MIGRATE_FILE=crates/cairo-lang-sierra-generator/src/function_generator_test_data/simple \
//!     cargo test -p cairo-lang-test-utils --features testing \
//!     migrate_target_function::tests::migrate_file_from_env -- --ignored --nocapture
//! ```
//!
//! Runner classification (whether a runner needs a specific resolved target function at all, vs.
//! whole-module diagnostics where no target is needed - see [`classify_runner`]) is a small,
//! explicit table that must be extended as more runners are covered by an actual migration pass;
//! unknown runners conservatively default to "needs a target".
//!
//! This tool is temporary scaffolding for the golden-test-data migration and will be deleted once
//! that migration is complete.

use std::path::Path;

use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::parse_test_file::{Test, dump_to_test_file, parse_test_file};

/// Mirrors `cairo_lang_semantic::test_utils::TARGET_FUNCTION_ATTR`. Duplicated here (rather than
/// depending on `cairo-lang-semantic`, which would drag a heavy dependency into this otherwise
/// lightweight crate) - keep the two literals in sync.
const TARGET_FUNCTION_ATTR: &str = "target_function";

/// Whether a test runner needs to resolve one specific target function out of the test module
/// content, or is happy with whole-module diagnostics (no target function needed at all).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TargetFunctionNeed {
    /// The runner resolves and inspects a single function (e.g. sierra/lowering generation).
    NeedsTarget,
    /// The runner only reports diagnostics for the whole module; no function needs targeting
    /// (e.g. `cairo_lang_semantic::test_utils::test_function_diagnostics`, which routes straight
    /// through `setup_test_module_ex` when given `cairo_code` without `function_name`).
    NoTargetNeeded,
}

/// Extracts the runner-name portion of a `test_runner_name` tag value, dropping any trailing
/// `(arg: val, ...)` argument list (mirrors the parsing in
/// `cairo_lang_test_utils::parse_test_file::run_test_file`).
pub fn runner_name(tag_value: &str) -> &str {
    tag_value.split_once('(').map_or(tag_value, |(name, _)| name).trim()
}

/// Classifies a runner name (already stripped of `(args...)`, see [`runner_name`]) by whether it
/// needs a resolved target function. Extend this table as more runners get migrated; unknown
/// runners conservatively default to [`TargetFunctionNeed::NeedsTarget`].
pub fn classify_runner(name: &str) -> TargetFunctionNeed {
    match name {
        "test_function_diagnostics" => TargetFunctionNeed::NoTargetNeeded,
        _ => TargetFunctionNeed::NeedsTarget,
    }
}

/// The result of attempting to migrate a single test block.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RewriteOutcome {
    /// Nothing needed changing: an `expr_code`/`function_body` test, an already-unified
    /// `cairo_code`-only test, or a `cairo_code` + `function_name` test whose `cairo_code` is a
    /// `>>> file: <path>` reference (per the decided mitigation: never edit the referenced
    /// example file, so these are left alone entirely).
    Unchanged,
    /// The test's tags were rewritten.
    Rewritten,
    /// The rewrite couldn't be done safely (ambiguous or missing `fn <function_name>` anchor);
    /// left byte-for-byte untouched. The string explains why.
    NeedsManualReview(String),
}

/// Returns whether `function_code` is exactly a useless single free-function stub, e.g.
/// `fn foo() {}` (any identifier, only that one item, empty body). Only meaningful for runners
/// that don't need a target function - see [`TargetFunctionNeed::NoTargetNeeded`].
fn is_trivial_stub(function_code: &str) -> bool {
    let trimmed = function_code.trim();
    let Some(after_fn) = trimmed.strip_prefix("fn ") else { return false };
    let Some(paren_idx) = after_fn.find('(') else { return false };
    let name = after_fn[..paren_idx].trim();
    let is_ident = !name.is_empty() && name.chars().all(|c| c.is_alphanumeric() || c == '_');
    is_ident && after_fn[paren_idx..].trim() == "() {}"
}

/// Per-line brace delta, ignoring braces inside string/char literals or after a `//` line
/// comment. Good enough for the formatted, comment-light golden test fixtures this tool targets;
/// not a general Cairo lexer.
fn brace_delta(line: &str) -> i32 {
    let mut delta = 0;
    let mut chars = line.chars().peekable();
    let mut in_string = false;
    let mut in_char = false;
    while let Some(c) = chars.next() {
        if in_string {
            if c == '\\' {
                chars.next();
            } else if c == '"' {
                in_string = false;
            }
            continue;
        }
        if in_char {
            if c == '\\' {
                chars.next();
            } else if c == '\'' {
                in_char = false;
            }
            continue;
        }
        match c {
            '/' if chars.peek() == Some(&'/') => break,
            '"' => in_string = true,
            '\'' => in_char = true,
            '{' => delta += 1,
            '}' => delta -= 1,
            _ => {}
        }
    }
    delta
}

/// Strips a leading `pub` / `pub(...)` visibility modifier (and the whitespace after it), if any.
fn strip_visibility(s: &str) -> &str {
    if let Some(rest) = s.strip_prefix("pub(")
        && let Some(idx) = rest.find(')')
    {
        return rest[idx + 1..].trim_start();
    }
    if let Some(rest) = s.strip_prefix("pub ") {
        return rest.trim_start();
    }
    s
}

/// Whether `line` (at brace-depth 0) is exactly a `fn <name>` item declaration for `name`,
/// guarding against substring collisions (`fn name_but_longer` must not match `name`).
fn line_declares_fn(line: &str, name: &str) -> bool {
    let trimmed = strip_visibility(line.trim_start());
    let Some(after_fn) = trimmed.strip_prefix("fn ") else { return false };
    let after_fn = after_fn.trim_start();
    let Some(rest) = after_fn.strip_prefix(name) else { return false };
    // Word boundary: the character right after `name` must not continue an identifier (otherwise
    // e.g. function_name "foo" would wrongly match a `fn foo_bar(...)` line).
    !rest.starts_with(|c: char| c.is_alphanumeric() || c == '_')
}

/// Finds the single top-level (brace-depth 0) line declaring `fn <function_name>` in `code`.
/// Returns the (0-based) line index, or an error describing why no single unambiguous anchor was
/// found - e.g. the name only appears nested inside an `impl`/`mod` body, or as a substring of
/// another identifier, or more than once at the top level.
fn find_top_level_fn_line(code: &str, function_name: &str) -> Result<usize, String> {
    let mut depth: i32 = 0;
    let mut matches = Vec::new();
    for (line_idx, line) in code.lines().enumerate() {
        if depth == 0 && line_declares_fn(line, function_name) {
            matches.push(line_idx);
        }
        depth += brace_delta(line);
    }
    match matches.len() {
        1 => Ok(matches[0]),
        0 => Err(format!(
            "No top-level `fn {function_name}` item found (only nested in an impl/mod body, or \
             not present at all) - refusing to guess."
        )),
        n => Err(format!(
            "Found {n} top-level candidates for `fn {function_name}` - ambiguous, refusing to \
             guess."
        )),
    }
}

/// Inserts a `#[target_function]` marker on the line directly above the top-level `fn
/// <function_name>` item in `code`. Returns an error (never a guess) if that anchor isn't unique.
pub fn insert_target_function_marker(code: &str, function_name: &str) -> Result<String, String> {
    let line_idx = find_top_level_fn_line(code, function_name)?;
    let mut lines: Vec<String> = code.lines().map(str::to_string).collect();
    let indent: String = lines[line_idx].chars().take_while(|c| c.is_whitespace()).collect();
    lines.insert(line_idx, format!("{indent}#[{TARGET_FUNCTION_ATTR}]"));
    Ok(lines.join("\n"))
}

/// Rebuilds a tag map in its original order, replacing the (contiguous-in-role, not necessarily
/// contiguous-in-position) `module_code`/`function_code` pair with a single `cairo_code` tag at
/// the position of whichever of the two appeared first, optionally dropping other tags.
fn rebuild_tags(
    attrs: &OrderedHashMap<String, String>,
    cairo_code: &str,
    drop: &[&str],
) -> OrderedHashMap<String, String> {
    let mut out = OrderedHashMap::default();
    let mut inserted_cairo_code = false;
    for (key, value) in attrs.iter() {
        if key == "module_code" || key == "function_code" {
            if !inserted_cairo_code {
                out.insert("cairo_code".to_string(), cairo_code.to_string());
                inserted_cairo_code = true;
            }
            continue;
        }
        if drop.contains(&key.as_str()) {
            continue;
        }
        out.insert(key.clone(), value.clone());
    }
    out
}

/// Migrates the legacy `function_code` (+ optional `module_code`) (+ `function_name`) family for
/// one test.
fn migrate_legacy_family(
    attrs: &OrderedHashMap<String, String>,
) -> (OrderedHashMap<String, String>, RewriteOutcome) {
    let function_code = &attrs["function_code"];
    let module_code = attrs.get("module_code").map(String::as_str).unwrap_or("");
    // Mirrors `cairo_lang_semantic::test_utils::test_module_code`'s legacy join exactly, so spans
    // don't shift.
    let merged = if module_code.is_empty() {
        function_code.clone()
    } else {
        format!("{module_code}\n{function_code}")
    };

    let runner = attrs.get("test_runner_name").map(String::as_str).unwrap_or("");
    match classify_runner(runner_name(runner)) {
        TargetFunctionNeed::NeedsTarget => {
            // Span-preserving: rename the tags, but keep `function_name` exactly as-is so
            // `setup_test_function` resolves the target exactly like it does today. No
            // `#[target_function]` marker is inserted here - doing so would add a line and shift
            // every span inside the function body by one (see `insert_target_function_marker`'s
            // docs and the module doc above); that's out of scope for a span-preserving rewrite.
            let out = rebuild_tags(attrs, &merged, &[]);
            (out, RewriteOutcome::Rewritten)
        }
        TargetFunctionNeed::NoTargetNeeded => {
            // No function is ever resolved for this runner class, so `function_name` is dropped,
            // and a trivial stub `function_code` (which only existed to satisfy the old
            // mandatory-function_code API) is dropped too - both span-preserving, since the stub
            // only ever sits *after* `module_code`.
            let cairo_code =
                if is_trivial_stub(function_code) { module_code.to_string() } else { merged };
            let out = rebuild_tags(attrs, &cairo_code, &["function_name"]);
            (out, RewriteOutcome::Rewritten)
        }
    }
}

/// Migrates the `cairo_code` + `function_name` family (cairo-lang-runner) for one test: keeps
/// `cairo_code` as-is, removes `function_name`, and inserts a `#[target_function]` marker.
fn migrate_cairo_code_function_name_family(
    attrs: &OrderedHashMap<String, String>,
) -> (OrderedHashMap<String, String>, RewriteOutcome) {
    let cairo_code = &attrs["cairo_code"];
    if cairo_code.trim_start().starts_with(">>> file:") {
        // Decided mitigation (no code left to write, per the infra node): the reference points at
        // an example `.cairo` file outside the test-data file; never edit example files, and
        // leave these tests untouched (keeping `function_name`).
        return (attrs.clone(), RewriteOutcome::Unchanged);
    }
    let function_name = &attrs["function_name"];
    match insert_target_function_marker(cairo_code, function_name) {
        Ok(new_cairo_code) => {
            let out = rebuild_tags_keep_cairo_code(attrs, &new_cairo_code);
            (out, RewriteOutcome::Rewritten)
        }
        Err(reason) => (attrs.clone(), RewriteOutcome::NeedsManualReview(reason)),
    }
}

/// Like [`rebuild_tags`], but `cairo_code` already exists (no `module_code`/`function_code` to
/// fold in) - only its content is replaced in place, and `function_name` is dropped.
fn rebuild_tags_keep_cairo_code(
    attrs: &OrderedHashMap<String, String>,
    new_cairo_code: &str,
) -> OrderedHashMap<String, String> {
    let mut out = OrderedHashMap::default();
    for (key, value) in attrs.iter() {
        if key == "function_name" {
            continue;
        }
        if key == "cairo_code" {
            out.insert(key.clone(), new_cairo_code.to_string());
            continue;
        }
        out.insert(key.clone(), value.clone());
    }
    out
}

/// Migrates a single test's tags. Never mutates `test`; returns the new tag map plus what
/// happened. See the module docs for the family dispatch rules.
pub fn migrate_test(test: &Test) -> (OrderedHashMap<String, String>, RewriteOutcome) {
    let attrs = &test.attributes;
    if attrs.contains_key("expr_code") || attrs.contains_key("function_body") {
        // That family is explicitly out of scope for this migration.
        return (attrs.clone(), RewriteOutcome::Unchanged);
    }
    if attrs.contains_key("function_code") {
        return migrate_legacy_family(attrs);
    }
    if attrs.contains_key("cairo_code") && attrs.contains_key("function_name") {
        return migrate_cairo_code_function_name_family(attrs);
    }
    // Already unified (bare `cairo_code`) or some other shape this tool doesn't know about: leave
    // it alone rather than guess.
    (attrs.clone(), RewriteOutcome::Unchanged)
}

/// A report of what [`migrate_tests`]/[`migrate_file`] did to a file's tests.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct MigrationReport {
    pub rewritten: Vec<String>,
    pub unchanged: Vec<String>,
    /// `(test_name, reason)` pairs left untouched because they need a human to look at them.
    pub needs_manual_review: Vec<(String, String)>,
}

/// Migrates every test in `tests` in memory (no disk I/O). Tests flagged for manual review are
/// returned with their original tags, unmodified.
pub fn migrate_tests(
    tests: &OrderedHashMap<String, Test>,
) -> (OrderedHashMap<String, Test>, MigrationReport) {
    let mut out = OrderedHashMap::default();
    let mut report = MigrationReport::default();
    for (name, test) in tests.iter() {
        let (new_attrs, outcome) = migrate_test(test);
        match outcome {
            RewriteOutcome::Rewritten => report.rewritten.push(name.clone()),
            RewriteOutcome::Unchanged => report.unchanged.push(name.clone()),
            RewriteOutcome::NeedsManualReview(reason) => {
                report.needs_manual_review.push((name.clone(), reason));
            }
        }
        out.insert(name.clone(), Test { attributes: new_attrs, line_num: test.line_num });
    }
    (out, report)
}

/// Migrates a single golden test-data file in place (parses it with
/// [`crate::parse_test_file::parse_test_file`], rewrites tags, and dumps it back with
/// [`crate::parse_test_file::dump_to_test_file`] - the exact same format the test framework
/// itself reads/writes, so untouched tags round-trip byte-for-byte). Tests needing manual review
/// are left untouched in place; nothing is silently guessed.
pub fn migrate_file(path: &Path) -> std::io::Result<MigrationReport> {
    let tests = parse_test_file(path)?;
    let (new_tests, report) = migrate_tests(&tests);
    dump_to_test_file(new_tests, path.to_str().expect("non-utf8 path"))?;
    Ok(report)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    fn test_with(attrs: &[(&str, &str)]) -> Test {
        Test {
            attributes: attrs.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect(),
            line_num: 1,
        }
    }

    /// A literal golden-test-data file, in exactly the format `dump_to_test_file` produces:
    /// tag order preserved, `====` separators, no incidental whitespace churn.
    const SAMPLE_FILE: &str = "\
//! > Simple function call

//! > test_runner_name
test_function_generator

//! > function_code
fn foo(a: felt252) -> felt252 {
    bar(a)
}

//! > function_name
foo

//! > module_code
fn bar(x: felt252) -> felt252 {
    x
}

//! > sierra_code
some sierra output

//! > ==========================================================================

//! > Second test

//! > test_runner_name
test_function_generator

//! > function_code
fn baz() -> felt252 {
    1
}

//! > function_name
baz

//! > sierra_code
other sierra output
";

    #[test]
    fn test_roundtrip_unmodified_file_is_byte_identical() {
        let dir = tempdir();
        let path = dir.join("sample_test_file");
        std::fs::write(&path, SAMPLE_FILE).unwrap();

        let tests = parse_test_file(&path).unwrap();
        dump_to_test_file(tests, path.to_str().unwrap()).unwrap();

        let roundtripped = std::fs::read_to_string(&path).unwrap();
        assert_eq!(roundtripped, SAMPLE_FILE);
        std::fs::remove_dir_all(dir).ok();
    }

    /// A tiny per-process-unique scratch directory (avoids depending on any other crate's test
    /// data, and avoids collisions between parallel test threads).
    fn tempdir() -> std::path::PathBuf {
        let dir = std::env::temp_dir()
            .join(format!("cairo_migrate_target_function_test_{}", std::process::id()))
            .join(format!("{:?}", std::thread::current().id()));
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn test_expr_code_family_is_untouched() {
        let test = test_with(&[
            ("test_runner_name", "test_expr_diagnostics"),
            ("expr_code", "1 + 1"),
            ("module_code", "fn unrelated() {}"),
            ("function_body", ""),
            ("expected_diagnostics", ""),
        ]);
        let (new_attrs, outcome) = migrate_test(&test);
        assert_eq!(outcome, RewriteOutcome::Unchanged);
        assert_eq!(new_attrs, test.attributes);
    }

    #[test]
    fn test_legacy_family_needs_target_keeps_function_name_no_marker() {
        let test = test_with(&[
            ("test_runner_name", "test_function_generator"),
            ("function_code", "fn foo(a: felt252) -> felt252 {\n    bar(a)\n}"),
            ("function_name", "foo"),
            ("module_code", "fn bar(x: felt252) -> felt252 {\n    x\n}"),
            ("sierra_code", "..."),
        ]);
        let (new_attrs, outcome) = migrate_test(&test);
        assert_eq!(outcome, RewriteOutcome::Rewritten);
        assert!(!new_attrs.contains_key("function_code"));
        assert!(!new_attrs.contains_key("module_code"));
        assert_eq!(new_attrs.get("function_name").map(String::as_str), Some("foo"));
        assert_eq!(
            new_attrs.get("cairo_code").map(String::as_str),
            Some(
                "fn bar(x: felt252) -> felt252 {\n    x\n}\nfn foo(a: felt252) -> felt252 {\n    \
                 bar(a)\n}"
            )
        );
        // Tag order preserved: `cairo_code` sits where `function_code` (the earlier of the two
        // original tags) used to be, and `function_name` keeps its original position after it.
        assert_eq!(
            new_attrs.keys().cloned().collect::<Vec<_>>(),
            vec!["test_runner_name", "cairo_code", "function_name", "sierra_code"]
        );
    }

    #[test]
    fn test_legacy_family_no_target_drops_stub_and_function_name() {
        let test = test_with(&[
            ("test_runner_name", "test_function_diagnostics(expect_diagnostics: true)"),
            ("function_code", "fn foo() {}"),
            ("function_name", "foo"),
            ("module_code", "fn abc() {}\n\nfn abc(a: felt252) {}"),
            ("expected_diagnostics", "..."),
        ]);
        let (new_attrs, outcome) = migrate_test(&test);
        assert_eq!(outcome, RewriteOutcome::Rewritten);
        assert!(!new_attrs.contains_key("function_code"));
        assert!(!new_attrs.contains_key("function_name"));
        assert_eq!(
            new_attrs.get("cairo_code").map(String::as_str),
            Some("fn abc() {}\n\nfn abc(a: felt252) {}")
        );
    }

    #[test]
    fn test_legacy_family_no_target_keeps_non_stub_function_code() {
        let test = test_with(&[
            ("test_runner_name", "test_function_diagnostics(expect_diagnostics: true)"),
            ("function_code", "fn foo() { let _x: felt252 = 1; }"),
            ("function_name", "foo"),
            ("module_code", "fn abc() {}"),
            ("expected_diagnostics", "..."),
        ]);
        let (new_attrs, outcome) = migrate_test(&test);
        assert_eq!(outcome, RewriteOutcome::Rewritten);
        assert!(!new_attrs.contains_key("function_name"));
        assert_eq!(
            new_attrs.get("cairo_code").map(String::as_str),
            Some("fn abc() {}\nfn foo() { let _x: felt252 = 1; }")
        );
    }

    #[test]
    fn test_cairo_code_function_name_family_inserts_marker_removes_name() {
        let test = test_with(&[
            ("test_runner_name", "test_profiling"),
            (
                "cairo_code",
                "fn pow2_14000() {\n    pow2_by_add_loop(14000, 1);\n}\n\nfn pow2_by_add_loop(n: \
                 felt252) {\n}",
            ),
            ("function_name", "pow2_14000"),
            ("expected_profiling_info", "..."),
        ]);
        let (new_attrs, outcome) = migrate_test(&test);
        assert_eq!(outcome, RewriteOutcome::Rewritten);
        assert!(!new_attrs.contains_key("function_name"));
        assert_eq!(
            new_attrs.get("cairo_code").map(String::as_str),
            Some(
                "#[target_function]\nfn pow2_14000() {\n    pow2_by_add_loop(14000, 1);\n}\n\nfn \
                 pow2_by_add_loop(n: felt252) {\n}"
            )
        );
    }

    #[test]
    fn test_cairo_code_function_name_family_file_ref_untouched() {
        let test = test_with(&[
            ("test_runner_name", "test_profiling"),
            ("cairo_code", ">>> file: examples/fib.cairo"),
            ("function_name", "main"),
            ("expected_profiling_info", "..."),
        ]);
        let (new_attrs, outcome) = migrate_test(&test);
        assert_eq!(outcome, RewriteOutcome::Unchanged);
        assert_eq!(new_attrs, test.attributes);
    }

    #[test]
    fn test_ambiguous_impl_only_match_needs_manual_review() {
        let test = test_with(&[
            ("test_runner_name", "test_profiling"),
            (
                "cairo_code",
                "struct S {}\nimpl SImpl of Trait<S> {\n    fn target() -> felt252 { 1 }\n}",
            ),
            ("function_name", "target"),
            ("expected_profiling_info", "..."),
        ]);
        let (new_attrs, outcome) = migrate_test(&test);
        assert!(matches!(outcome, RewriteOutcome::NeedsManualReview(_)), "{outcome:?}");
        // Never silently rewritten: attributes are untouched.
        assert_eq!(new_attrs, test.attributes);
    }

    #[test]
    fn test_substring_collision_is_not_matched() {
        let test = test_with(&[
            ("test_runner_name", "test_profiling"),
            ("cairo_code", "fn target_but_longer() -> felt252 { 1 }"),
            ("function_name", "target"),
            ("expected_profiling_info", "..."),
        ]);
        let (_new_attrs, outcome) = migrate_test(&test);
        assert!(matches!(outcome, RewriteOutcome::NeedsManualReview(_)), "{outcome:?}");
    }

    #[test]
    fn test_multiple_top_level_matches_needs_manual_review() {
        // Not valid Cairo (duplicate name), but exercises the ambiguity guard itself.
        let code = "fn dup() -> felt252 { 1 }\nfn dup() -> felt252 { 2 }";
        let err = find_top_level_fn_line(code, "dup").unwrap_err();
        assert!(err.contains("ambiguous"), "{err}");
    }

    #[test]
    fn test_is_trivial_stub() {
        assert!(is_trivial_stub("fn foo() {}"));
        assert!(is_trivial_stub("  fn bar() {}  "));
        assert!(!is_trivial_stub("fn foo() { 1 }"));
        assert!(!is_trivial_stub("fn foo(a: felt252) {}"));
        // Only an exact `() {}` (matching the formatter's canonical output) counts as trivial -
        // this deliberately doesn't try to normalize arbitrary internal whitespace.
        assert!(!is_trivial_stub("fn bar()   {  }"));
    }

    #[test]
    fn test_runner_name_strips_args() {
        assert_eq!(runner_name("test_profiling"), "test_profiling");
        assert_eq!(
            runner_name("test_function_generator(future_sierra: true)"),
            "test_function_generator"
        );
    }

    /// Ad hoc entry point: set `MIGRATE_FILE` to a path (relative to the current working
    /// directory when running `cargo test`, typically a crate's manifest dir) and run this
    /// specific ignored test to migrate that one file in place. See the module docs for the full
    /// invocation.
    #[test]
    #[ignore = "ad hoc migration entry point, not part of the regular test suite"]
    fn migrate_file_from_env() {
        let path = std::env::var("MIGRATE_FILE").expect("Set MIGRATE_FILE to the file to migrate.");
        let report = migrate_file(Path::new(&path)).expect("Failed to migrate file.");
        println!("{report:#?}");
        assert!(
            report.needs_manual_review.is_empty(),
            "Some tests need manual review: {:#?}",
            report.needs_manual_review
        );
    }
}
