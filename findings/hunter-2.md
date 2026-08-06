# Bug Hunter #2 — Findings

Area: runnable entry-point wrapper codegen.
Primary file: `crates/cairo-lang-runnable-utils/src/builder.rs` (the whole crate — only `builder.rs` + `lib.rs` exist).

## Summary

I did a deep line-by-line trace of the wrapper codegen (builtin discovery, fp-offset
assignment, param push order, segment-arena synthesis + finalization loop, output/panic
handling, and the standalone `builtin_list` path), cross-checking against the two real
consumers (`cairo-lang-runner` testing path and `cairo-lang-executable` execution path)
and the VM-side hint handlers in `cairo-lang-runner/src/casm_run/mod.rs`.

The core logic is carefully written and, as far as I can demonstrate, **correct** — builtin
ordering, fp offsets, arg-push order, the segment-arena N→N-1 adjacency loop, the
panic-indicator layout, and the runner `user_args`/`param_index` alignment all check out
(details in "Things I verified clean" below). I did **not** find a demonstrable
functional/ordering/off-by-one bug in the happy path.

I am reporting **one real robustness defect** (Result-returning public function panics on a
class of malformed input instead of erroring) and **one minor code-smell** (dead redundant
guards). I am labeling the robustness item honestly: it is reachable through the public API
but only with a function signature that violates the *documented* execution-mode contract,
so a maintainer may classify it as "unsupported input" rather than a bug.

---

## Finding 1 (robustness / suspected bug — medium-low confidence)

**File / location:**
`crates/cairo-lang-runnable-utils/src/builder.rs:457` (and the twin at `:525`).

```rust
// line 456-457, in EntryCodeHelper::process_params
let non_proof_signature_params =
    if self.config.testing { param_types } else { &param_types[..(param_types.len() - 2)] };
```
```rust
// line 522-526, in EntryCodeHelper::process_output
let non_proof_return_types = if self.config.testing {
    return_types
} else {
    &return_types[..(return_types.len() - 1)]
};
```

**Description:**
`create_entry_code_from_params` (and therefore the public wrappers
`RunnableBuilder::create_wrapper_info` / `assemble_function_program`) is declared
`-> Result<..., BuildError>`, i.e. it advertises graceful error reporting. But in
non-testing (execution) mode it unconditionally slices `param_types[..len-2]` and
`return_types[..len-1]`. If an execution-mode function is passed with fewer than 2
parameters, `param_types.len() - 2` underflows `usize` → panic ("attempt to subtract with
overflow" in debug, or an out-of-bounds slice panic in release). Likewise fewer than 1
return type panics at line 525 (and `return_types.last().unwrap()` at line 547).

So a caller gets a **panic / process abort** instead of a `BuildError`, defeating the
`Result` contract.

**Root cause:**
The code assumes the documented execution signature
`(Span<felt252>, Array<felt252>) -> Array<felt252>` (see the `EntryCodeConfig::testing`
doc, lines 248-257: "the function signature is expected to be …") and strips the trailing
input/output params/return with a bare subtraction, with no length check and no conversion
to `BuildError::MissingFunction`-style error. The invariant is enforced upstream by the
`#[executable]` plugin for the in-tree callers, but the function is `pub` and the invariant
is never checked at this boundary.

**Reachability:** In-tree it is not hit — `cairo-lang-executable` only feeds
plugin-validated signatures, and `cairo-lang-runner` always uses `EntryCodeConfig::testing()`
(which takes the full-slice branch and cannot underflow). It is reachable only by an
external library user calling the public API directly with an execution-mode config and a
non-conforming signature.

**Full test code (Rust unit test on the real public API):**
Add to a new `#[cfg(test)]` module in the crate (or any downstream crate that depends on
`cairo-lang-runnable-utils`). It exercises the public `create_entry_code_from_params`
with a legal-but-argument-less execution-mode config.

```rust
use cairo_lang_runnable_utils::builder::{create_entry_code_from_params, EntryCodeConfig};

#[test]
fn execution_mode_underflow_panics_instead_of_erroring() {
    // Zero params, zero returns, execution (non-testing) mode.
    // Documented contract wants Result<_, BuildError>; instead this panics on
    // `param_types.len() - 2` (0usize - 2) at builder.rs:457.
    let param_types: Vec<(cairo_lang_sierra::ids::GenericTypeId, i16)> = vec![];
    let return_types: Vec<(cairo_lang_sierra::ids::GenericTypeId, i16)> = vec![];
    let config = EntryCodeConfig::executable(/* allow_unsound */ false, /* builtin_list */ None);

    // EXPECTED (correct behavior): returns Err(BuildError::...).
    // ACTUAL (bug): panics with "attempt to subtract with overflow".
    let result = std::panic::catch_unwind(|| {
        create_entry_code_from_params(&param_types, &return_types, 0, config)
    });
    assert!(
        result.is_ok(),
        "create_entry_code_from_params panicked on a short signature; \
         a Result-returning API should return Err instead of panicking"
    );
}
```

**How to verify:**
`cargo test -p cairo-lang-runnable-utils execution_mode_underflow_panics_instead_of_erroring`.
The `catch_unwind` will capture a panic (test asserts it *should not* have panicked, so the
assert fails today, demonstrating the defect). Alternatively, remove the `catch_unwind`
wrapper and observe the test binary aborts with a subtract-overflow panic originating at
`builder.rs:457`. Trace-only confirmation (no build needed): with `param_types == &[]` and
`config.testing == false`, control reaches line 457 and evaluates `&param_types[..(0 - 2)]`
→ `usize` underflow.

**Suggested fix:** at the top of `process_params` / `process_output` (or in
`create_entry_code_from_params` before dispatching), when `!config.testing` verify
`param_types.len() >= 2` and `return_types.len() >= 1` (and ideally that the trailing types
match the expected `Span`/`Array` shape), returning a `BuildError` otherwise.

---

## Finding 2 (minor — dead/redundant guard, not a functional bug)

**File / location:** `crates/cairo-lang-runnable-utils/src/builder.rs:414` and `:546`.

```rust
// line 413-419
if self.has_post_calculation_loop {
    if !self.config.testing {          // <-- always true here
        casm_build_extend!(self.ctx, localvar local;);
        self.local_exprs.insert(BuiltinName::output, self.ctx.get_unadjusted(local).clone());
    }
    ...
}
```

**Description / root cause:** `has_post_calculation_loop` is defined (line 411) as
`self.got_segment_arena && !self.config.testing`. So inside any
`if self.has_post_calculation_loop { … }` block, `!self.config.testing` is already
guaranteed. The nested `if !self.config.testing` at line 414 is therefore dead-always-true.
The same redundancy appears implicitly around the post-loop copy at line 570-581 (only
runs under `has_post_calculation_loop`, which already implies non-testing). Purely a
readability/maintenance smell — no behavioral impact. Worth simplifying (removing the inner
guard) to avoid future confusion.

---

## Things I verified clean (traced, no bug found)

- **Builtin canonical ordering** (`builtin_ty_to_vm_name`, lines 364-374): relative order
  pedersen < range_check < bitwise < ec_op < poseidon < range_check96 < add_mod < mul_mod
  matches the canonical VM order used in the standalone `builtin_list`
  (`cairo-lang-executable/src/test.rs:45-57`). `segment_arena` is intentionally the last map
  entry and is `.skip(1)`-excluded from the fp-read set (line 389) because it is synthesized
  by the entry code, not passed by the VM.
- **fp-offset assignment** (`process_builtins`, lines 385-406, and the `builtin_list` path,
  lines 312-319): both assign offset 3 to the *last* canonical builtin and grow toward
  `output` (largest offset). This matches the Cairo calling convention where the first
  argument (`output`) sits at the most-negative fp offset. The two paths are mutually
  consistent.
- **Arg push order** (`process_params` loop, lines 458-493): builtins are pushed in
  `param_types` (canonical) order via `tempvar _builtin = var`, `segment_arena` in its
  signature position, matching what `FUNCTION` expects.
- **Segment-arena finalization loop** (`validate_segment_arena`, lines 601-634): iterates
  `n_segments - 1` times over 3-cell info entries (`prev_end = infos[1]`,
  `curr_start = infos[3]`, `infos += 3`), i.e. exactly the N-1 adjacencies between N
  segments. No off-by-one. `[-3]/[-2]/[-1]` = infos/n_constructed/n_destructed matches the
  arena metadata layout and the `assert n_segments = n_finalized` finalization check.
- **Panic-path layout** (`process_output`, lines 546-568): size==3 reads
  `[panic_flag, start, end]` in ascending-address order via `next_unprocessed_deref`
  (deepest cell first), matching the runner's `handle_main_return_value`
  (`cairo-lang-runner/src/lib.rs:407-425`, `values[0]` = flag, `values[len-2]`/`[len-1]` =
  start/end). `AddMarker` emitted only in the size==3 case is correct: its sole consumer
  (`cairo-execute/src/main.rs:294`) uses the last marker to print panic data on the failing
  assert; the size==2 nopanic case cannot panic so needs no marker.
- **`next_unprocessed_deref`** (lines 513-519): first call yields `[ap - total]`
  (lowest/first return cell), last yields `[ap-1]`; the trailing
  `assert_eq!(unprocessed_return_size, 0)` (line 569) guards exact consumption.
- **`process_builtins_output` + assert** (lines 637-648): `self.builtin_vars`
  (= `new_builtin_vars`) is a superset of `self.builtins`; `segment_arena` (if present) is
  removed earlier by `validate_segment_arena`, so the final `assert!(builtin_vars.is_empty())`
  holds. The `or_insert_with` in `process_output` (lines 585-596) only fires for
  `builtin_list`-supplied extras (e.g. unused ecdsa/keccak) and correctly asserts
  `builtin_list.is_some()` otherwise (input builtins == output builtins in inferred mode
  because Sierra builtins are linearly threaded).
- **Runner arg alignment** (testing path): `WriteRunParam { index: param_index }` indices
  (line 476-483, incremented for every user param incl. zero-sized ones, and gas treated as
  the first user arg in testing mode) align with `RunnerHelper::prepare_args`
  (`cairo-lang-runner/src/lib.rs:359-398`), which prepends gas at index 0 and pushes one
  (possibly empty) entry per `is_user_arg_type` param. Zero-sized params increment
  `param_index` without emitting a hint, and the runner pushes a matching empty
  `user_args` entry — indices stay aligned.
- **Zero-arg / no-return testing path**: fully handled — `process_builtins`/`process_params`/
  `process_output` all no-op cleanly with empty slices in testing mode (no underflow because
  the `testing` branches use the full slice).
- `find_function` (ends_with match), `type_size`/`type_long_id`/`type_info`,
  `create_metadata`, `create_code_footer`, `BuildError` helpers (`stmt_indices`,
  `is_ap_overflow_error`) — straightforward, no issues.

## Files checked

- `crates/cairo-lang-runnable-utils/src/builder.rs` (full file — target)
- `crates/cairo-lang-runnable-utils/src/lib.rs`
- `crates/cairo-lang-runnable-utils/Cargo.toml`
- `crates/cairo-lang-runner/src/lib.rs` (consumer: testing path, `prepare_args`,
  `handle_main_return_value`, `get_results_data`, gas handling)
- `crates/cairo-lang-executable/src/compile.rs` (consumer: execution path)
- `crates/cairo-lang-executable/src/test.rs` (standalone `builtin_list` canonical order)
- `crates/cairo-lang-runner/src/casm_run/mod.rs` (VM-side handlers for `WriteRunParam`,
  `AddMarker`, `AddRelocationRule`)
- `crates/bin/cairo-execute/src/main.rs` (marker consumption)
