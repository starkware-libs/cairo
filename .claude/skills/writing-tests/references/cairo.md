# Cairo-language test mechanics (this repo)

Self-sufficient reference for tests written in Cairo and executed on the VM.
Use this layer when the behavior under test is what the code *does when run*
(corelib semantics, arithmetic, Starknet storage/events/syscalls) — codegen
goldens only prove output stability, and the VM is this repo's execution
oracle (lambdaclass cairo-vm, via cairo-lang-runner).

## Where tests live

- `corelib/src/test/*.cairo` — corelib behavior (e.g. `integer_test.cairo`,
  `byte_array_test.cairo`, `dict_test.cairo`). New corelib functionality gets
  its cases in the matching file; new file only for a new module.
- `crates/cairo-lang-starknet/cairo_level_tests/` — Starknet runtime:
  contracts under `contracts/`, components under `components/`, tests in
  `*_test.cairo` (storage, events, syscalls, dispatchers, components).
- `tests/bug_samples/issueNNNN.cairo` — whole-program regression repros from
  reported issues; add one when a user-reported program broke the pipeline
  and no smaller layer expresses it.

## Skeleton

```cairo
#[test]
fn take_advance_by_saturates_at_end() {
    let mut iter = array![1_u8, 2].into_iter().take(2);
    assert_eq!(iter.advance_by(3), Result::Err(1));
}

#[test]
#[should_panic(expected: 'index out of range')]
fn array_at_out_of_range_panics() { ... }

#[test]
#[available_gas(1000000)]   // cap gas when the test could loop
fn bounded_loop() { ... }
```

Assertion/attribute vocabulary: `assert!`, `assert_eq!`, `assert_ne!`,
`#[should_panic(expected: ...)]`, `#[available_gas(N)]`, `#[ignore]`.
Compile-rejection cases (code that must *not* compile) are not written here —
those are semantic-diagnostic golden cases (see `rust.md`).

## Running

```sh
cargo run --profile=release --bin cairo-test -- corelib/
cargo run --profile=release --bin cairo-test -- crates/cairo-lang-starknet/cairo_level_tests/ --starknet
cargo run --profile=release --bin cairo-test -- tests/bug_samples --starknet
# useful flags:
#   --filter <substr>      run matching tests only
#   --include-ignored / --ignored
#   --gas-disabled         run without gas accounting
#   --run-profiler <kind>  profile the run (cairo|scoped|sierra)
```

These three invocations are exactly what CI's `cairotest` job runs; if they
pass locally, the layer is green. `--profile=release` matters — debug-profile
runs of corelib are painfully slow.

## What belongs here vs. lower layers

- New/changed corelib function → Cairo `#[test]` with boundary cases (empty,
  zero, max, max±1; both `Ok`/`Err` or panic paths). History: zero-variant
  and boundary inputs are where 3-year escapes lived (#10069, #10017-class).
- Starknet plugin change that alters generated code's runtime behavior →
  executed test here (a contract exercising the storage/event path), in
  addition to the plugin_test_data golden. The golden pins the shape; this
  layer proves the generated code actually runs (#10069's invalid generated
  code kept a green golden for 3 years).
- Panics/gas: a function documented to panic gets a `#[should_panic]` case;
  gas-sensitive behavior gets a case under `--gas-disabled` semantics only if
  its contract differs there.
- Keep `#[ignore]` for genuinely unassertable behavior (today: only the three
  corelib print tests) and say why in a comment.
