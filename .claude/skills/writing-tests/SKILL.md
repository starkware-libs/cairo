---
name: writing-tests
description: Use when writing or changing tests in this repo, deciding what to test or where a test belongs, adding a regression test for a bug fix, reviewing the test portion of a PR, deciding which golden outputs to bless and reviewing the blessed diff, or adding a benchmark. Trigger phrases include "add a test", "write tests for", "is this covered", "does this need a test", "review the tests here", "which layer should this test go in". Also use for the mechanics of the golden framework itself — the //! > file format, creating test_file_test! runners, or how CAIRO_FIX_TESTS works (covered in references/golden-framework.md).
---

# Writing tests for the Cairo compiler

Goal: the right tests, not more tests. A test earns its keep by failing when
behavior regresses and staying green through refactors. This repo's history
shows the dominant failure mode is missing cases (error paths, boundaries),
not over-testing — when in doubt about a rejection path, write the case.

## Modes

- **Writing tests** (developing a change): continue below.
- **Reviewing tests** (judging a PR's test portion): walk
  `references/review-checklist.md` top to bottom.

## Test-placement decision tree

Pick exactly one home layer per behavior — the lowest layer that can observe
it. A behavior covered at its home layer is not re-asserted at other layers.

1. **Pure Rust logic** (span arithmetic, id/key construction, data structures,
   formatting helpers) → Rust unit test in the sibling `foo_test.rs` file.
   History: off-by-one bugs in helpers (#10017 span end, #10003 n_steps)
   escaped because only golden layers were exercised and they never observe
   helper internals. See `references/rust.md`.
2. **Stage output or diagnostic** (parser tree, semantic/lowering diagnostic,
   lowering text, sierra-gen phase output, plugin expansion, formatter output)
   → new case in the existing golden test_data file of that stage's crate.
   A new case in an existing file beats a new file: the runner, wiring, and
   review habits already exist. See `references/rust.md`; framework mechanics
   in `references/golden-framework.md`.
3. **Language or runtime semantics** (what executing the Cairo code does:
   corelib behavior, arithmetic, storage/events/syscalls) → Cairo `#[test]`
   in `corelib/src/test/` or `crates/cairo-lang-starknet/cairo_level_tests/`.
   The VM is the only execution oracle this repo has — codegen goldens prove
   output stability, not that the generated code runs correctly (#10069:
   empty-enum `Store` derive produced invalid code for 3 years while its
   plugin golden stayed green). See `references/cairo.md`.
4. **Libfunc-level codegen, gas, or ap-change** → one minimal case in
   `tests/e2e_test_data/libfuncs/`. This is the de-facto oracle for
   `sierra-gas`/`sierra-ap-change`, which have almost no tests of their own.
5. **Whole-program regression from a reported issue** →
   `tests/bug_samples/issueNNNN.cairo`.
6. **Performance-sensitive path** → benchmark, only per
   `references/benchmarks.md` (benches run nightly, not on PRs).

Go up a layer only when the behavior is the *interaction* of stages; reserve
e2e (`tests/e2e_test_data/`) for behavior that individual stage suites cannot
express. When a plugin/codegen change alters what generated code *does* at
runtime, add the executed Cairo-level test in addition to the codegen golden —
that pair is two different behaviors (shape vs. semantics), not duplication.

## Deriving the cases

Derive cases from the intended contract (doc comment, diagnostic's purpose,
libfunc signature), not from the implementation you just wrote — tests derived
from the code inherit its bugs. Cover, in order of demonstrated payoff here:

1. **Every explicit error/rejection path.** The repo's #1 escaped-bug class:
   new features ship exhaustively tested happy paths and untested rejection
   paths (#10211 ICE on wrong generic-arg count, #9998 unterminated string,
   #10118 illegal dict type via `Default`). For each new diagnostic, add the
   golden case that triggers it; for each `Err`/`Option::None` branch, a case
   that reaches it.
2. **Boundaries**: empty, zero, one, max, max±1 (#10069 zero-variant enum,
   #10017 offset exactly at span end). Zero-variant/empty-collection inputs
   are cheap golden cases — add them when deriving, not after the bug. For
   per-element validations, one case where the offender is not the first
   element; for parser changes, one case where the input truncates
   mid-construct (the partial_trees suites exist for this).
3. **One representative per equivalence class** of the happy path. Add
   another case only when it can fail for a different reason.
4. **Feature interactions** with orthogonal machinery the change touches:
   coupons, gas-disabled mode, `#[cfg]`, snapshots, debug-info flags
   (#10081 coupon_call broke the profiler; #9957 a debug-info flag no test
   ever enabled). If your change adds a flag or mode, at least one test runs
   with it on.

## Exactness rules

- Assert outcomes, not internals: a test any behavior-preserving refactor
  breaks protects the implementation, not the behavior. House proof of a pure
  refactor is "regenerating all goldens yields zero diff" (#10120) — that only
  works if goldens assert behavior.
- Keep each golden case minimal and single-feature, so a future diff is
  attributable to one cause; name it for the behavior and condition
  (`wrong_generic_arg_count_through_alias`), never an issue number alone.
- Bug fix → ship the regression test that fails without the fix, at the home
  layer. 5 of 9 escaped defects sampled from history shipped fixes with no
  test, leaving the bug class unguarded; reviewers here ask "add an affected
  test if this does anything" — pre-empt them.
- Reuse the existing fixtures: `*DatabaseForTesting`, `setup_test_function`,
  shared e2e DBs. A helper may hide boilerplate, never the input and expected
  value that make this test this test.
- No unseeded randomness, wall-clock time, or test-order dependence; tests
  share salsa DBs, so never mutate shared state in a test body.
- Deletion is part of the change: when your change makes cases redundant
  (two cases that now fail together, a case subsumed by a new one), remove or
  merge them in the same PR.

## Property testing (not yet adopted)

No property-testing framework exists in this workspace, so do not write
property tests today, and do not add the dependency inside an unrelated PR —
adoption is a team decision. What you can do: when a change touches one of the
candidates below, suggest adoption *once* (a review note or follow-up task,
not a nag on every PR). Recommended framework: **proptest** (composable
strategies suit compiler-shaped inputs better than quickcheck's one-strategy-
per-type). Evidence-backed candidates, each motivated by an escaped bug:

- Span/offset arithmetic round-trips — #10017 (inclusive-vs-exclusive end).
- Formatter idempotence: `format(format(x)) == format(x)`.
- Bounded-int/integer libfunc ranges over full domains — the #10069 class.
- Sibling-predicate agreement (e.g. `is_var_free` vs `is_fully_concrete` on
  inputs where they must coincide) — #10010, where a copy-pasted helper
  silently diverged.

Once adopted: use a property test where the contract is a relationship over
all inputs (round-trips, idempotence, two paths agreeing) — one property
replaces a family of example cases; keep a few named examples as
documentation; seed generators with domain-valid inputs; commit the failure
persistence file so found bugs stay found; then regenerate this skill so this
section becomes full guidance.

## References

- `references/review-checklist.md` — walk when reviewing a PR's tests.
- `references/rust.md` — Rust unit/golden mechanics: skeletons, commands, the
  bless-then-review workflow.
- `references/golden-framework.md` — the `cairo_lang_test_utils` file-based
  framework itself: `//! >` format, writing runners, CAIRO_FIX_TESTS.
- `references/cairo.md` — Cairo-language tests: corelib, cairo_level_tests,
  bug_samples, cairo-test runner flags.
- `references/benchmarks.md` — when a change warrants a benchmark and how the
  nightly perf gate works.
