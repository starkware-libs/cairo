# Bug Hunter #4 — runner result/gas/profiling

Scope: `crates/cairo-lang-runner/src/lib.rs`, `crates/cairo-lang-runner/src/profiling.rs`,
and gas-accounting/result-parsing helpers reachable from them.

## Bug 1: `get_initial_available_gas` double-charges the function's required gas,
causing spurious "Out of gas" panics even when the caller supplies exactly the
amount the runner's own API says is required

**File/location**: `crates/cairo-lang-runner/src/lib.rs:479-496`

```rust
pub fn get_initial_available_gas(
    &self,
    func: &Function,
    available_gas: Option<usize>,
) -> Result<usize, RunnerError> {
    let Some(available_gas) = available_gas else {
        return Ok(0);
    };

    // In case we don't have any costs - it means no gas equations were solved (and we are in
    // the case of no gas checking enabled) - so the gas builtin is irrelevant, and we
    // can return any value.
    let Some(required_gas) = self.initial_required_gas(func) else {
        return Ok(0);
    };

    available_gas.checked_sub(required_gas).ok_or(RunnerError::NotEnoughGasToCall)
}

pub fn initial_required_gas(&self, func: &Function) -> Option<usize> {
    let gas_info = &self.builder.metadata().gas_info;
    require(!gas_info.function_costs.is_empty())?;
    Some(
        gas_info.function_costs[&func.id]
            .iter()
            .map(|(token_type, val)| val.into_or_panic::<usize>() * token_gas_cost(*token_type))
            .sum(),
    )
}
```

`get_initial_available_gas` is called from `prepare_args` (`lib.rs:360-362`) and its
return value becomes the literal initial value written into the `GasBuiltin` cell that
the compiled Sierra program receives as its first implicit argument (via the
`ExternalHint::WriteRunParam` mechanism used in `EntryCodeConfig::testing()` — see
`crates/cairo-lang-runnable-utils/src/builder.rs:472-484`; in testing mode the wrapper
does **not** perform any additional gas deduction of its own — whatever value
`get_initial_available_gas` returns is exactly what the Sierra function starts executing
with).

**Description**: `initial_required_gas(func)` computes `function_costs[func.id]`, i.e.
the total statically-required gas that must be present for `func` to be safely callable
(as computed by the sierra-gas equation solver — see
`crates/cairo-lang-sierra-gas/src/gas_info.rs:17`, documented as "The costs of calling
the given function"). This is exactly the amount the compiled function's own entry-point
gas check(s) will attempt to withdraw from the counter it is handed. `get_initial_available_gas`
then *also* subtracts this same `required_gas` from `available_gas` before handing the
result to the VM as the counter's starting value. The subtraction is redundant/incorrect:
the function is left with `available_gas - required_gas` in its counter, and its own
first `withdraw_gas`/`get_available_gas` check then needs to withdraw `required_gas` again
out of that already-reduced amount — i.e. the same reservation is charged twice. Concretely:
if a caller supplies exactly `available_gas == initial_required_gas(func)` (which the
sufficiency check `available_gas.checked_sub(required_gas)` treats as the minimum sufficient
amount, since anything less already returns `Err(NotEnoughGasToCall)`), the counter the VM
actually starts with is `0`, and the function's own entry gas check then immediately fails
with `Out of gas`, regardless of how cheap the actual requested computation is.

**Root cause**: `available_gas.checked_sub(required_gas)` is used both as (a) the
sufficiency gate (correct: `available_gas` must be `>= required_gas`) and (b) the value
passed on as the new counter (incorrect: this re-reserves `required_gas` a second time on
top of what the compiled program's own gas-checks already reserve). The initial gas
counter handed to the VM should be `available_gas` itself (once verified `>= required_gas`),
not `available_gas - required_gas`.

**Demonstration** (normal usage of the real public `SierraCasmRunner` API — compiles an
inline Cairo snippet, no internals-poking):

```rust
// crates/cairo-lang-runner/tests/gas_required_boundary_test.rs
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_runner::{Arg, RunResultValue, SierraCasmRunner};
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::program_generator::SierraProgramWithDebug;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

#[test]
fn exactly_required_gas_should_be_sufficient_but_panics() {
    let db = RootDatabase::builder().detect_corelib().build().unwrap();
    // Trivial function: with n == 0 the loop breaks immediately on the first
    // iteration, this is the cheapest possible execution of this function.
    let cairo_code = "fn foo(mut n: felt252) -> felt252 { \
        loop { if n == 0 { break 0; } n = n - 1; } \
    }";
    let test_module = setup_test_module(&db, cairo_code).unwrap();
    let crate_input = test_module.crate_id.long(&db).clone().into_crate_input(&db);
    DiagnosticsReporter::stderr().with_crates(&[crate_input]).allow_warnings().ensure(&db).unwrap();

    let SierraProgramWithDebug { program: sierra_program, .. } =
        db.get_sierra_program(vec![test_module.crate_id]).expect("get_sierra_program failed");
    let sierra_program = replace_sierra_ids_in_program(&db, sierra_program);

    // `Some(Default::default())` enables real gas-usage-checked metadata computation,
    // exactly as done by every caller that wants gas accounting (see
    // tests/examples_test.rs::run_function and cairo-lang-test-runner).
    let runner = SierraCasmRunner::new(
        sierra_program.clone(),
        Some(Default::default()),
        OrderedHashMap::default(),
        None,
    )
    .unwrap();
    let func = runner.find_function("foo").unwrap();

    // `initial_required_gas` is the runner's own public API for "how much gas is needed
    // to call this function". Its use in `get_initial_available_gas` treats this value as
    // the sufficiency threshold: anything below it is rejected with `NotEnoughGasToCall`.
    let required_gas = runner.initial_required_gas(func).expect("gas checking enabled");

    // Call with exactly the required amount, on the cheapest possible input (n = 0).
    let result = runner
        .run_function_with_starknet_context(
            func,
            vec![Arg::Value(0.into())],
            Some(required_gas),
            Default::default(),
        )
        .expect("run_function_with_starknet_context returned an Err");

    // BUG: this is a RunResultValue::Panic(["Out of gas"]) even though the workload is
    // trivial (n == 0, loop breaks immediately) and `required_gas` is, by the runner's own
    // contract, supposed to be sufficient to call the function at all.
    assert!(
        matches!(result.value, RunResultValue::Success(_)),
        "expected Success when calling with exactly initial_required_gas() on the cheapest \
         possible input, got {:?} (gas_counter={:?})",
        result.value,
        result.gas_counter
    );
}
```

**How to verify**: Save the file under `crates/cairo-lang-runner/tests/` (dev-dependencies
`cairo-lang-compiler`, `cairo-lang-semantic` (feature `testing`) and
`cairo-lang-sierra-generator` are already available in `cairo-lang-runner`'s `Cargo.toml`)
and run:

```
cargo test -p cairo-lang-runner --test gas_required_boundary_test -- --nocapture
```

Observed actual output (unmodified repo, verified live in this session):

```
initial_required_gas(foo) = Some(2970)
Running with available_gas = required_gas = 2970
value = Panic([0x4f7574206f6620676173]), gas_counter = Some(0x0)   // "Out of gas"
```

i.e. `run_function_with_starknet_context` panics with `Out of gas` and reports a final
`gas_counter` of `0`, showing the VM was started with a counter of `0` (`available_gas -
required_gas = 2970 - 2970 = 0`) rather than `2970`. Supplying `required_gas + 1000` instead
succeeds, confirming the ~`required_gas`-sized extra reservation is the culprit, not an
actual insufficiency in the requested workload (which is a single loop iteration).

I also reproduced the identical symptom (panic exactly at the reported "required" boundary,
success only once a `required_gas`-ish amount of extra headroom is added) with a
recursive function (`fn foo(n: u32) -> u32 { if n == 0 { 0 } else { 1 + foo(n - 1) } }`)
called with `n = 0`, ruling out that this is specific to `loop`-based gas equations.

**Practical impact**: Any caller of `SierraCasmRunner` (test runners, profilers, CLI users
of `cairo-run`, gas-estimation tooling) that sizes `available_gas` based on
`initial_required_gas`/`function_costs` (a documented, public, intended-for-this-purpose
API) will see spurious `Out of gas` panics for calls that should succeed. It also means the
gas actually charged to a run (`available_gas - result.gas_counter`, the formula used by
`cairo-lang-test-runner`'s `gas_usage` field and by `tests/examples_test.rs`) silently
includes an extra `required_gas`-sized constant whenever the callee's own automatic
gas-checks are triggered on top of a non-empty `function_costs` entry (this did not show up
in the two existing exact-cost regression tests, `hash_chain_gas` and the `fib`
auto-gas tests, only because those particular cases either have `function_costs == 0`
for the entry, or don't assert an exact numeric gas figure — see
`tests/examples_test.rs` `run_function`/`run_function_auto_gas_test`).

---

## Areas reviewed with no bug found

- `SierraCasmRunner::run_function`'s header/footer trace trimming
  (`crates/cairo-lang-runner/src/lib.rs:286-291`, the two `n_steps -= relocated_trace...position(...)`
  lines): traced through the exact index arithmetic by hand (using the real
  `header`+`program`+`footer` layout produced by `CairoProgram::assemble_ex` in
  `crates/cairo-lang-sierra-to-casm/src/compiler.rs:150-182` and
  `crates/cairo-lang-runnable-utils/src/builder.rs`). The subtraction of
  `position(pc > header_end)` (leading header steps) and `rev().position(pc > header_end)`
  (trailing header/footer steps) is exactly self-consistent and removes precisely the
  wrapper steps with no off-by-one.
- `SierraCasmRunner::handle_main_return_value` / `get_results_data`
  (`crates/cairo-lang-runner/src/lib.rs:401-462`): the panic/success discrimination
  (`values[0] == 0` ⇒ success, else panic with `err_data_start`/`err_data_end` span) and the
  reverse iteration over `return_types` to slice `ap` match the standard
  `core::panics::PanicResult` encoding; cross-checked against `tests/examples_test.rs`'s
  `fib_u128_pass`/`fib_u128_fail`/`fib_u128_checked_*` cases, all of which pass.
  Note: this function *would* panic (`values[values.len()-2]`/`-1`) if a real
  `PanicResult` result somehow shipped with fewer than 2 payload felts, but that is not
  reachable through normal Cairo programs (any panic wrapper always carries at least the
  `(start, end)` span), so this is not flagged as a real bug.
- `ProfilingInfo::from_trace`'s function-stack push/pop and `max_stack_trace_depth`
  handling (`crates/cairo-lang-runner/src/profiling.rs:80-182`): worked out the invariant
  by hand — the push guard `function_stack_depth < max_stack_trace_depth` (checked against
  the depth *before* the call) and the pop guard `function_stack_depth <= max_stack_trace_depth`
  (checked against the depth *after* the corresponding call, i.e. before decrementing on
  return) are equivalent for integers (`D < max ⇔ D+1 <= max`), so every push has a matching
  pop and vice versa; no leak, no double-pop, no off-by-one at the depth-cap boundary.
  `end_of_program_reached`/`unreachable!()` guard is unreachable in practice because trailing
  header/footer steps are always skipped earlier via the `load_offset` check.
- `StarknetExecutionResources::add_assign`, `token_gas_cost` table, `args_size`/`Arg::size`,
  `initialize_vm`: straightforward, no issues found.
- `ProfilingInfoProcessor` weight aggregation (statement/libfunc/user-function/Cairo-function/
  stack-trace/scoped-statement) in `profiling.rs:423-762`: filtering by `min_weight` and the
  `usize::MAX - weight` sort-descending trick are applied consistently everywhere; no
  mismatched keys or duplicate-counting found.

## Files checked

- `crates/cairo-lang-runner/src/lib.rs` (primary focus; bug found here)
- `crates/cairo-lang-runner/src/profiling.rs` (primary focus; no bug found)
- `crates/cairo-lang-runner/src/casm_run/mod.rs` (gas/resource plumbing, `run_function`,
  `RunFunctionResult`, `CairoHintProcessor`/`ResourceTracker` delegation)
- `crates/cairo-lang-runner/src/clap.rs`, `crates/cairo-lang-runner/src/short_string.rs`
  (skimmed; out of the gas/result-parsing/profiling scope, nothing notable)
- `crates/cairo-lang-runnable-utils/src/builder.rs` (entry-code/header/footer generation,
  needed to validate the trace-trimming and gas-writing logic in `lib.rs`)
- `crates/cairo-lang-sierra-to-casm/src/compiler.rs` (`assemble_ex`, to confirm
  header/program/footer memory layout)
- `crates/cairo-lang-sierra-gas/src/gas_info.rs`, `crates/cairo-lang-sierra-gas/src/compute_costs.rs`
  (semantics of `function_costs`)
- `crates/cairo-lang-test-runner/src/lib.rs` (consumer of `initial_required_gas`/gas_counter,
  used to confirm the intended gas-usage-reporting contract)
- `tests/examples_test.rs`, `crates/cairo-lang-runner/src/profiling_test.rs` (existing test
  patterns; ran the existing `run_function_test`/`run_function_auto_gas_test` suites live —
  all pass on the unmodified tree, which is why Bug 1 wasn't already caught: none of the
  existing tests assert an exact gas figure for a function with a non-trivial
  `function_costs` entry)

I additionally wrote and ran a scratch investigation test locally (compiled and executed
live against the unmodified tree to confirm the exact panic/gas-counter values quoted
above), then reverted/removed it — the working tree is clean; the reproduction test above
is provided in full so it can be re-created and re-run independently.
