# Benchmarks (this repo)

Self-sufficient reference for the perf-testing infrastructure and when to
touch it.

## What exists

All benches live in the top-level `tests/` crate (`tests/benches/`):

- `compile.rs` (criterion) — compile-phase timings: cairo→sierra,
  cairo→diagnostics, cairo→testing and cache variants, over fixture projects
  in `tests/benches/examples/` and `tests/benches/staking/`.
- `common/canaries.rs` — generated canary programs (`big_array`,
  `large_struct`, `wide_enum`, `deep_nesting`, `nested_loops`) that turn
  super-linear compile-time blowups into visible dashboard step-changes.
- `dhat_compile.rs`, `dhat_ls_flow.rs` — heap-allocation profiling
  (feature-gated: `--features dhat`).
- `ls_reexec.rs` — language-server re-execution timing; the only measurement
  sensitive to salsa incrementality (early-cutoff) regressions.

## How they run and gate

Nightly only (`.github/workflows/nightly.yml`, `benchmark` job):
`cargo bench --bench compile|dhat_compile|dhat_ls_flow|ls_reexec`, published
to the gh-pages dashboard via github-action-benchmark with
`fail-on-alert: true` at a 150% threshold. Nothing perf-related runs on PRs.

Consequences:

- A perf regression lands silently and alerts the next night — #8554 passed
  every functional test and was reverted later purely for performance. For a
  change on a compile-time hot path, run the relevant bench locally against
  base and branch and put the numbers in the PR description.
- Salsa/incrementality changes (query keys, interning, early-cutoff) are
  invisible to every functional layer; `ls_reexec` is the only detector
  (#10190's wrong cache keying was caught this way). Run it locally for such
  changes.

## Local comparison recipe

```sh
git checkout main && cargo bench --bench compile -- --save-baseline base
git checkout <branch> && cargo bench --bench compile -- --baseline base
# heap: cargo bench --bench dhat_compile --features dhat
# incrementality: cargo bench --bench ls_reexec
```

Criterion compares against the named baseline on the same machine — the only
noise-safe comparison; never compare numbers recorded on different machines.

## When to add a benchmark

- The change targets a known compile-time hot path (lowering, sierra-gen,
  parser on large files) or a throughput contract → extend an existing bench
  group in `compile.rs` with a representative fixture, rather than a new
  harness.
- The change introduces a structure whose cost could scale super-linearly
  with program size → add a canary shape to `common/canaries.rs`; that is
  what the canary set is for.
- Otherwise, don't: a benchmark nobody watches implies watched-ness. The
  dashboard set is deliberately small; keep it that way.

Two kinds, never conflated: representative workloads (`examples/`,
`staking/` fixtures — support throughput/regression claims) vs. canary
microbenches (localize a blowup to a shape). Cite workload numbers for
"X% faster" claims; canary numbers only for localization.

A confirmed nightly alert gets the failing-test treatment: bisect, fix, or
explicitly accept with rationale in the PR — never silent acceptance.
