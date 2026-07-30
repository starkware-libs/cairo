# Reviewing the test portion of a PR

Walk the items in order; they are ordered by what this repo's history shows
matters most. Cite the item (and its evidence PR) when flagging, so the author
sees the ask is earned, not imported.

## 1. Does the change need tests at all?

- Behavior change (new diagnostic, changed codegen, new corelib function,
  changed runtime semantics) → yes, at the home layer per SKILL.md's tree.
- Bug fix → a regression test that fails without the fix. Ask for the
  demonstration ("what did this test print before the fix?"). This is the
  repo's weakest habit: 13 substantive fixes in the last 300 merged PRs
  shipped no test, and 5 of 9 sampled escaped defects (#10007, #10017,
  #10190, #9957, #10118) left their bug class unguarded after the fix.
- Pure refactor → existing tests pass unchanged. Changed assertions during a
  "refactor" mean it wasn't one, or the tests were change-detectors — flag
  either way. Exception: goldens that encode incidental representation
  (statement locations, sierra ids, contract-class artifacts embedding the
  compiler version) legitimately re-bless; if the same suite re-blesses on
  every refactor, file that the golden is structure-sensitive rather than
  blocking the PR.

## 2. Error paths and negatives present?

The #1 escaped-defect class here: happy path exhaustively tested, rejection
path untested. For each new diagnostic, `Err` branch, or validation added:
is there a case that triggers it? Wrong-arity/wrong-kind inputs after a
resolver change (#10211 — ICE instead of diagnostic), malformed literals
(#9998), illegal type arguments (#10118), swallowed `Result`s (#10007 —
`let _ =` on a diagnostic-producing call). Reviewer precedent: "why? add an
affected test if this does anything" (#9854), "Where do we catch them?
Please do test" (#10169).

## 3. Boundaries covered?

Empty/zero-variant input (#10069 — zero-variant enum in `Store` derive,
escaped 3 years), a value exactly at a span/range end (#10017 — inclusive
vs. exclusive `TextSpan.end`), off-by-one counts (#10003). If the change
handles N things, there is a case for N=0 and the boundary N. Two boundary
shapes this repo demonstrably misses:

- Validation applied per element of a list or sequence: include a case where
  the offending element is not the first (#10238 — macro-param kind
  validation silently stopped after the first param).
- Parser or recovery changes: include a case where the input truncates
  mid-construct, i.e. EOF arrives where the next token is expected — the
  `partial_trees` test_data suites exist for exactly this (#10254, #10256,
  #10227 all fixed truncation bugs in one quarter).

## 4. Right layer, no double-assertion?

- Each new test at its home layer per the decision tree; the same behavior
  is not asserted at two example-based layers.
- Codegen/plugin changes whose contract is runtime behavior have an executed
  Cairo-level test, not only a codegen golden (#10069's fix shipped only the
  golden — the actual runtime behavior is still untested).
- New flag or mode (debug-info, gas-disabled, coupons): at least one test
  runs with it enabled — an opt-in path no test enables is an untested layer
  (#9957, #10081).
- Prefer "new case in an existing test_data file" over a new file or new
  runner when the stage already has a suite.
- A new Rust helper or predicate whose contract differs subtly from an
  existing sibling (`is_fully_concrete` vs. `is_var_free`) has a unit test on
  an input where the two answers differ — golden suites cannot observe the
  distinction, which is how a copy-pasted body escaped in #10010.

## 5. Would each test survive a behavior-preserving refactor?

Assertions on outcomes and contracts, not call sequences or incidental
output. For goldens: is the case minimal and single-feature, so its future
diffs stay attributable? A golden that snapshots half the compiler's output
for one property should be narrowed.

## 6. Golden diffs reviewed, not wholesale-blessed?

Re-blessed hunks read line-by-line: do the diffs match the PR's stated
intent, and nothing else? Wide reblesses are legitimate for cross-cutting
diagnostics/codegen changes (cb0fcd799 touched 63 test_data files for one
new warning) — but every hunk must be explained by the change. A version-bump
style diff mixed into a logic change is a smell.

## 7. Deterministic?

No unseeded randomness, wall-clock, sleeps, or order dependence; shared
salsa test DBs (`SHARED_DB`, e2e `LazyLock` DBs) are never mutated by a test.
(History shows near-zero flakiness — 3 `#[ignore]`s in the whole tree, all
corelib print tests; keep it that way.)

## 8. Critical-tier change: deeper bar

Touching `cairo-lang-sierra-gas`, `cairo-lang-sierra-ap-change`, or
`sierra-to-casm` invocations: these guard prover soundness and have almost
no direct unit coverage (ap-change: zero own tests) — require an e2e libfunc
case exercising the changed cost/ap path, executed (`test_data_input`/
`test_data_output`), not just casm-golden. Touching the parallel warmup
(`cairo-lang-compiler` rayon paths): no concurrency tooling exists in-repo,
so require the change to keep all shared state behind the existing
`Mutex`/salsa patterns and say so in review — flag any new bare shared state.

## 9. Property-test candidate touched?

If the change touches one of SKILL.md's property-testing candidates
(span/offset arithmetic, formatter output, bounded-int/libfunc ranges,
sibling predicates), note once in review that this is a property-test
candidate and adoption of proptest would guard the whole input space — a
suggestion, not a blocker, and only if the thread doesn't already have one.
No property tests can be requested until the framework is adopted.

## 10. Anything now redundant?

Cases the change subsumes (two cases that can only fail together, an old
workaround case for behavior now handled generally) are deleted or merged in
this PR, not left to rot.

## 11. Performance-sensitive path touched?

Compile-time hot paths, memory-heavy structures, language-server flows:
check `references/benchmarks.md` for whether a bench (or a canary) is
warranted — and remember benches gate nightly only, so a perf claim in the
PR description needs local `cargo bench` evidence (#8554 was reverted for a
perf regression every functional test missed).
