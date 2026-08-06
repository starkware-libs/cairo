# Supervisor #1 — Adjudication (runner / runnable-utils / executable / test-runner group)

Repo: /home/user/cairo. Group covers hunters H1–H5. Each finding below carries an
independent trace and a test-legitimacy judgment. Standards applied: confirm only if the
bug is real AND demonstrable through normal idiomatic use of the real public API.

Headline result: **H4's gas "double-charge" is REJECTED — it is by-design and documented,
and I disproved the double-charge empirically.** Two genuine novel finds remain (H3, H5-1),
plus one robustness downgrade (H2-1) and two nits.

---

## H4 Bug 1 — `get_initial_available_gas` "double-charges" required gas → REJECTED (by-design)

**Cited:** `crates/cairo-lang-runner/src/lib.rs:479-496` (`get_initial_available_gas`),
`:498-507` (`initial_required_gas`).

**Claim:** subtracting `required_gas` from `available_gas` before writing the GasBuiltin
starting value double-charges the entry cost, so `available_gas == initial_required_gas`
spuriously "Out of gas". **Verdict: REJECTED.** This is the intended, documented contract,
and the empirical premise ("required_gas is the minimum sufficient budget") is false.

### Independent trace

The Cairo gas model has two distinct quantities:
- a **runtime** `GasBuiltin` counter, modified **only** by `withdraw_gas`/`redeposit_gas`
  (verified: `crates/cairo-lang-sierra-to-casm/src/invocations/gas.rs:59-92` — `withdraw_gas`
  is the sole place the counter cell is decremented at runtime; the per-libfunc costs are
  **not** individually withdrawn at runtime), and
- a **compile-time** gas *wallet*, initialized at every function entry to
  `function_costs[func.id]` (verified: `crates/cairo-lang-sierra-to-casm/src/annotations.rs:181`,
  `GasWallet::Value(metadata.gas_info.function_costs[&func.id].clone())`).

`withdraw_gas` moves gas from the runtime counter (`-r`) into the wallet (`+r`); ordinary
libfuncs drain the wallet symbolically. Hence the invariant

```
total_remaining_gas  =  compile_time_wallet  +  runtime_counter
```

is preserved across `withdraw_gas`. At entry the compiler *presumes* the wallet already
holds `function_costs[func]`. Therefore, to make the caller's total budget equal exactly
`available_gas`, the runtime counter must be seeded with `available_gas − function_costs`.
That is precisely what `get_initial_available_gas` does. Writing `available_gas` to the
counter instead (H4's proposed "fix") would give the function `function_costs + available_gas`
total — i.e. it would silently *under*-charge by the entry cost.

`initial_required_gas` = `function_costs[func]` = "The costs of *calling* the given function"
(`crates/cairo-lang-sierra-gas/src/gas_info.rs:17-18`). The `NotEnoughGasToCall` gate
(`lib.rs:495`) guarantees only enough to **enter** the function — never enough to **complete**
a body that performs gas-costed work (e.g. a loop, whose per-iteration cost is withdrawn
dynamically from the runtime counter, not from `function_costs`).

**This behavior is explicitly documented:** `lib.rs:260-262` — *"The cost of the function is
deducted from `available_gas` before the execution begins."*

### Empirical disproof of the double-charge

I compiled H4's own function (`fn foo(mut n: felt252) -> felt252 { loop { if n==0 {break 0;} n=n-1; } }`)
via the real public `SierraCasmRunner` API and swept `available_gas`:

```
initial_required_gas(foo) = 2970
avail = 2970 (req+0)   -> Out of gas, final_counter=0
avail = 3670 (req+700) -> Out of gas, final_counter=700   (failing withdraw leaves counter untouched)
avail = 3970 (req+1000)-> Success,    final_counter=1900
avail = 4970 (req+2000)-> Success,    final_counter=2900
```

Two facts fall out and both refute the claim:

1. **No double charge.** True consumption = `avail − final_counter` = `3970−1900 = 2070`, and
   identically `4970−2900 = 2070`. The measured cost (2070) is *less than* `required_gas`
   (2970). If the function re-withdrew `required_gas` at runtime (H4's model), consumption
   would be `≥ 2970`. It is 2070 — the entry cost is carried in the compile-time wallet and
   is **never** re-withdrawn at runtime. The `available_gas − gas_counter` accounting the
   whole ecosystem uses (e.g. `tests/examples_test.rs:219`) reports the true cost, not an
   inflated one.
2. **`required_gas` is not "sufficient to run".** Completing even the cheapest path `foo(0)`
   needs `avail ≈ 3970 > required_gas`, because the loop's `withdraw_gas` pulls a fresh
   per-iteration batch (~1000) out of the runtime counter (= `avail − 2970`). So passing
   exactly `required_gas` is correctly rejected/exhausted. H4's premise that
   `available_gas == initial_required_gas` is the "intended minimum" is simply wrong; the
   API name says *ToCall*, not *ToComplete*.

Corroboration: H4's proposed fix (write `available_gas` to the counter) would change the
measured consumption of any function with non-zero entry cost, breaking the existing exact
regression `hash_chain_gas` (`tests/examples_test.rs:287-291`, asserts
`available_gas − gas_counter == 10480 + 3·Pedersen`), which passes today *with* the
subtraction.

**Test-legitimacy:** H4's repro uses the real API cleanly, but it encodes a wrong oracle
(`assert Success` at exactly `required_gas`). The behavior it flags is correct. REJECTED.

---

## H3 Bug 1 — `#[executable]` with ≥101 params → felt252 short-string overflow (E2008) → CONFIRMED (novel, low severity)

**Cited:** `crates/cairo-lang-executable-plugin/src/lib.rs:176-182`.

**Independent trace:** verified the generator emits, per param, a raw
`format!("… .expect('Failed to deserialize param #{param_idx}');")` with no bound/truncation
on `param_idx` (source read at :176-182). String-length math:
`"Failed to deserialize param #"` = **29** ASCII bytes; a felt252 short-string holds ≤ 31
bytes. Param index digit counts: 1 digit → 30B (ok), 2 digits → 31B (ok, at the cap),
3 digits → 32B. The first 3-digit index is `100`, i.e. the **101st** parameter. 32 bytes
with leading byte `'F'`(0x46) packs to ≈ `0x46·2^248 ≈ 70·2^248`, well above the STARK prime
`P ≈ 12.3·2^248`, so it fails literal range validation → `E2008`, anchored (via
`.mapped(db, param)`) onto the user's own signature. A 100-param function (max index 99,
2 digits, 31 B) still compiles — the boundary H3 states is exact.

**Test-legitimacy:** the repro drives the real plugin+semantic pipeline through the crate's
own golden harness (`setup_test_module` + `expand_module_text`); no internals poked. 101
`felt252` params is valid Cairo — unusual, but a deterministic, unconditional, workaround-less
compile failure for correct source, and the diagnostic misleadingly blames the user.
**CONFIRMED**, severity low (extreme param count). Novel (no executable items in memory). I
did not re-run the live test (math is airtight and the source matches), but the mechanism is
fully verified.

---

## H5 Bug 1 — FAILED summary line silently drops the `filtered_out` count → CONFIRMED (novel, cosmetic)

**Cited:** `crates/cairo-lang-test-runner/src/lib.rs:154-187`.

**Independent trace (source read):** `filtered_out` is bound at `:140` and in scope on both
branches. The success branch (`:156`) prints `"… ; {filtered_out} filtered out;"`; the
`bail!` failure branch (`:180-186`) uses a different format string with **no**
`{filtered_out}` placeholder. So an all-pass run reports the filtered count and a run with
any failure does not — a genuine reporting inconsistency between the two paths of one
function. Real, trivially fixable (mirror the placeholder into the `bail!`).

**Test-legitimacy:** H5's repro uses only the public `TestRunner`/`TestRunConfig` API, exactly
like the crate's existing tests. **CONFIRMED**, severity low/cosmetic (no effect on pass/fail
verdicts). Novel. H5 correctly avoided re-filing the already-in-memory
`#[available_gas]` double-diagnostic and `verify_diagnostics_expectation` `'error:'` items
(see its "found correct" section) — nothing to dedup.

---

## H2 Finding 1 — `create_entry_code_from_params` usize-underflow panic (<2 params / 0 returns, execution mode) → SUSPECTED (robustness), downgraded from confirmed

**Cited:** `crates/cairo-lang-runnable-utils/src/builder.rs:457`, `:525`, `:547`.

**Independent trace (source read):** in non-testing (execution) mode the code unconditionally
slices `&param_types[..(param_types.len()-2)]` (:457) and `&return_types[..(return_types.len()-1)]`
(:525) and calls `return_types.last().unwrap()` (:547). With `param_types == &[]` (or len 1),
`len-2` underflows `usize` → panic, defeating the `-> Result<_, BuildError>` contract. Confirmed
reachable via the public API. **However**, this requires an *execution-mode* signature that
violates the documented contract `(Span<felt252>, Array<felt252>) -> Array<felt252>`
(`EntryCodeConfig` doc, `builder.rs:248-257`). Every in-tree caller is safe: `cairo-lang-runner`
always uses `EntryCodeConfig::testing()` (full-slice branch, no underflow), and
`cairo-lang-executable` only feeds `#[executable]`-plugin-validated signatures.

**Test-legitimacy judgment:** the repro is honest but (a) feeds contract-violating input the
compiler never produces, and (b) relies on `catch_unwind` to detect a panic. This is a
robustness/`Result`-contract gap, not a bug demonstrable through idiomatic use. **Downgrade to
SUSPECTED (robustness).** Reasonable hardening: length-check + `BuildError` when
`!testing && (param_types.len()<2 || return_types.is_empty())`. Not a user-facing bug.

## H2 Finding 2 — dead/redundant `if !self.config.testing` guard → NIT (confirmed dead, no behavior)

**Cited:** `crates/cairo-lang-runnable-utils/src/builder.rs:414` (and analogous :570).

**Trace (source read):** `has_post_calculation_loop = got_segment_arena && !config.testing`
(`:411`). Inside `if self.has_post_calculation_loop { … }` (`:413`) the nested
`if !self.config.testing` (`:414`) is therefore always true — dead guard, purely a
readability smell, zero behavioral impact. **Classified as NIT** (accept H2's own labeling).

---

## H5 secondary — duplicate test-config attribute silently ignored → SUSPECTED (nit)

**Cited:** `crates/cairo-lang-test-plugin/src/test_config.rs:50-53`.

`try_extract_test_config` uses `attrs.iter().find(...)` for each of `#[test]`/`#[ignore]`/
`#[available_gas]`/`#[should_panic]`, returning only the first match; a second copy of the
same attribute is silently dropped with no diagnostic. Real and plausible, but undemonstrated
(H5 did not build a fixture, and it is arguably accepted/unspecified behavior for unusual user
error). **SUSPECTED (nit).** Matches H5's own low-confidence labeling.

---

## H1 — Runtime execution (`casm_run/`): no bug → ACCEPTED CLEAN

H1 reports no demonstrable bug after a full audit. I reviewed the reachability arguments and
they hold up as written:
- **C1** (`InitSquashData` empty-dict panic) is guarded by the CASM lowering
  (`felt252_dict.rs:297-319`: the hint runs only in the `SquashDictNotEmpty` branch, so
  `n_accesses ≥ 1`). Not reachable.
- **C2/C3** (circuit add/mul gate solve arms) rest on the fixed sub/inverse offset convention
  in `sierra/.../circuit.rs:1085-1100` (unknown always in `lhs`, `rhs` always known in
  topological order); the `(Some,None)` mul panic is a defensive-only path needing a malformed
  (non-topological) circuit — unsupported input.
- **C4** (`positive_modulus` returning `modulus`) cannot occur on the only caller's Bézout
  input; **C7** (empty-span keccak `(0,0)`) needs out-of-contract input the corelib never
  produces.
None meets the confirm bar (all require unsupported/unreachable input). **ACCEPTED as clean.**
The C3 mul-gate panic and C7 keccak observation are legitimate defensive-robustness notes, not
bugs.

---

## Verdict summary

| Finding | Verdict | Note |
|---|---|---|
| H4-1 gas double-charge | **REJECTED** | By-design + documented (lib.rs:260-262); empirically disproved (true cost 2070 < required 2970; needs avail>required to complete) |
| H3-1 ≥101-param executable E2008 | **CONFIRMED** (novel) | Deterministic compile failure of valid Cairo; low severity (extreme param count) |
| H5-1 FAILED summary drops `filtered_out` | **CONFIRMED** (novel) | Cosmetic reporting inconsistency; trivial fix |
| H2-1 usize underflow on short exec signature | **SUSPECTED** (robustness) | Downgraded: needs contract-violating input + catch_unwind; not idiomatic |
| H2-2 dead `!testing` guard | **NIT** | Confirmed dead code, no behavior |
| H5-2 duplicate attribute ignored | **SUSPECTED** (nit) | Undemonstrated; plausibly accepted behavior |
| H1 (casm_run) | **CLEAN** | No bug; reachability negatives verified |

No DUPs against Team Memory (H5 already steered clear of the two filed test-plugin items).

---

## Files checked (merged across H1–H5 + supervisor traces, deduped)

Runner / runnable-utils:
- `crates/cairo-lang-runner/src/lib.rs`
- `crates/cairo-lang-runner/src/profiling.rs`
- `crates/cairo-lang-runner/src/profiling_test.rs`
- `crates/cairo-lang-runner/src/clap.rs`
- `crates/cairo-lang-runner/src/short_string.rs`
- `crates/cairo-lang-runner/src/casm_run/mod.rs`
- `crates/cairo-lang-runner/src/casm_run/circuit.rs`
- `crates/cairo-lang-runner/src/casm_run/dict_manager.rs`
- `crates/cairo-lang-runner/src/casm_run/contract_address.rs`
- `crates/cairo-lang-runner/src/casm_run/circuit_test.rs`
- `crates/cairo-lang-runner/src/casm_run/test.rs`
- `crates/cairo-lang-runnable-utils/src/builder.rs`
- `crates/cairo-lang-runnable-utils/src/lib.rs`
- `crates/cairo-lang-runnable-utils/Cargo.toml`

Executable + plugin:
- `crates/cairo-lang-executable/src/executable.rs`
- `crates/cairo-lang-executable/src/lib.rs`
- `crates/cairo-lang-executable/src/compile.rs`
- `crates/cairo-lang-executable/src/debug_info.rs`
- `crates/cairo-lang-executable/src/test.rs`
- `crates/cairo-lang-executable/src/compile_test_data/basic`
- `crates/cairo-lang-executable/Cargo.toml`
- `crates/cairo-lang-executable-plugin/src/lib.rs`
- `crates/cairo-lang-executable-plugin/src/test.rs`
- `crates/cairo-lang-executable-plugin/src/plugin_test_data/diagnostics`
- `crates/cairo-lang-executable-plugin/src/plugin_test_data/expansion`
- `crates/cairo-lang-executable-plugin/Cargo.toml`
- `crates/cairo-lang-sierra-generator/src/executables.rs`

Test-runner + test-plugin:
- `crates/cairo-lang-test-runner/src/lib.rs`
- `crates/cairo-lang-test-runner/src/test.rs`
- `crates/cairo-lang-test-runner/test_data/lib.cairo`
- `crates/cairo-lang-test-runner/test_data/cairo_project.toml`
- `crates/cairo-lang-test-runner/Cargo.toml`
- `crates/cairo-lang-test-plugin/src/lib.rs`
- `crates/cairo-lang-test-plugin/src/test_config.rs`
- `crates/cairo-lang-test-plugin/src/plugin.rs`
- `crates/cairo-lang-test-plugin/src/inline_macros/assert.rs`

Gas / sierra-to-casm / supporting (incl. supervisor's own H4 traces):
- `crates/cairo-lang-sierra-to-casm/src/annotations.rs`  *(supervisor)*
- `crates/cairo-lang-sierra-to-casm/src/invocations/gas.rs`  *(supervisor)*
- `crates/cairo-lang-sierra-to-casm/src/invocations/felt252_dict.rs`
- `crates/cairo-lang-sierra-to-casm/src/compiler.rs`
- `crates/cairo-lang-sierra-gas/src/gas_info.rs`
- `crates/cairo-lang-sierra-gas/src/compute_costs.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/circuit.rs`

Semantic / starknet / bins / corelib / vendored:
- `crates/cairo-lang-semantic/src/semantic.rs`
- `crates/cairo-lang-semantic/src/corelib.rs`
- `crates/cairo-lang-semantic/src/expr/compute.rs`
- `crates/cairo-lang-starknet/src/plugin/consts.rs`
- `crates/cairo-lang-syntax/src/node/ast_ext.rs`
- `crates/bin/cairo-execute/src/main.rs`
- `crates/bin/cairo-test/src/main.rs`
- `tests/examples_test.rs`
- `corelib/src/starknet/info.cairo`
- vendored `cairo-vm-3.2.0/src/vm/runners/builtin_runner/modulo.rs`

Supervisor scratch test (`crates/cairo-lang-runner/tests/sup1_gas_probe_test.rs`) written,
run, and removed — working tree clean (`git status` shows only `findings/`).
