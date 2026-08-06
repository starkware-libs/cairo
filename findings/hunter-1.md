# Bug Hunter #1 — Runtime Execution (`crates/cairo-lang-runner/src/casm_run/`)

## Summary / verdict

After an exhaustive read of the whole `casm_run/` directory (hint execution, dict
management, syscall/segment handling, memory, circuit evaluation) and cross-checking
several suspicious spots against the sierra layer, the corelib, and the vendored
`cairo-vm`, **I did not find a solidly demonstrable bug that is reachable through
normal, idiomatic use of the runner's public API.**

The code in this area is unusually robust. Per the task instruction ("1 solid bug
beats 5 shaky ones; if you find nothing real, say so honestly. Do not invent bugs"),
I am reporting a clean result plus the full reachability analysis for every candidate
I chased, so a supervisor can confirm the negatives without re-deriving them.

No bug is filed. Two latent/defensive items and one out-of-contract observation are
documented below as **NOT bugs** (with justification), so they are not re-investigated.

---

## Candidates investigated and ruled out

### C1 — `InitSquashData`: `keys[0]` / `current_key().unwrap()` panic on an empty dict
- **Location:** `mod.rs:2106` and `mod.rs:2112` (`CoreHint::InitSquashData`).
- **Hypothesis:** If a `Felt252Dict` is squashed with zero accesses, `n_accesses == 0`,
  so `dict_squash_exec_scope.keys` is empty and `keys[0]` (line 2106) panics with an
  out-of-bounds index (and `current_key().unwrap()` at 2112 would also panic).
- **Why it is NOT a bug:** The CASM lowering guards this. In
  `crates/cairo-lang-sierra-to-casm/src/invocations/felt252_dict.rs:297-319` the
  `SquashDict` code computes `ptr_diff = end - start` and executes
  `jump SquashDictNotEmpty if ptr_diff != 0;` (line 306). The `InitSquashData` hint is
  emitted only in the `SquashDictNotEmpty` branch (lines 314-319). The empty case
  (`ptr_diff == 0`) `ret`s with an empty squashed dict (lines 307-310) and never runs
  the hint. Therefore `n_accesses >= 1` whenever the hint executes; `keys` is never
  empty. Not reachable.

### C2 — `circuit.rs::fill_add_gate`: only `(None, Some)` "solve" case handled
- **Location:** `circuit.rs:68-94`.
- **Hypothesis:** For a subtraction gate the unknown operand might be placed in the
  `rhs` slot, which would fall into the `_ => false` arm and never get solved.
- **Why it is NOT a bug:** Verified the offset convention in
  `crates/cairo-lang-sierra/src/extensions/modules/circuit.rs:1088-1091`. A `SubModGate`
  emits `GateOffsets { lhs: output_offset, rhs: sub_rhs, output: sub_lhs }` — i.e. the
  unknown value is always the **lhs** slot, `rhs`/`output` are known. So the runner's
  `(None, Some(rhs))` branch (solving `lhs = output - rhs`) is exactly correct, and its
  arithmetic `(res + modulus - rhs).mod_floor(modulus)` cannot underflow (BigUint,
  `res + modulus >= rhs`). The `(Some, None)` / `(None, None)` → `false` arms correctly
  mean "input not ready yet, retry on a later outer-loop pass". Correct.

### C3 — `circuit.rs::fill_mul_gate`: `panic!("Unexpected None value…")` on `(Some, None)`
- **Location:** `circuit.rs:100-117` (arm at line 115).
- **Hypothesis:** A multiply gate whose `rhs` is not yet computed would panic instead of
  retrying/erroring.
- **Why it is NOT a bug (latent/defensive only):** Regular `MulModGate` offsets
  (`circuit.rs`/sierra `circuit.rs:1094`) are `{lhs, rhs, output}` where `lhs`/`rhs` are
  inputs or earlier gate outputs, and inverse gates (`:1100`) are
  `{lhs: output_offset(unknown), rhs, output: ONE_OFFSET}` — `rhs` is always known.
  Because the compiler emits gates in topological order and the `fill_values` interleave
  (drain add gates, then one mul gate, repeat — `circuit.rs:163-180`) resolves
  dependencies, `rhs` is always `Some` when a mul gate is reached. Reaching the panic
  requires a malformed (non-topological) circuit, i.e. unsupported input, so it cannot be
  demonstrated through idiomatic usage. Noted as a defensive-panic robustness gap only.

### C4 — `circuit.rs::positive_modulus` returns `modulus` instead of `0`
- **Location:** `circuit.rs:186-189`.
- **Hypothesis:** For a negative `value` that is an exact multiple of `modulus`,
  `value_mod == 0` and the function returns `modulus - 0 == modulus`, which is outside the
  intended `[0, modulus)` range.
- **Why it is NOT a bug:** The only caller is `invert_or_nullify`'s invertible branch
  (`circuit.rs:201`), passing the Bézout coefficient `x` with `value * x ≡ 1 (mod modulus)`.
  If `x ≡ 0 (mod modulus)` then `value*x ≡ 0 ≢ 1`, contradiction; so `value_mod != 0`
  always in this path and the result is a proper representative in `[1, modulus-1]`. The
  degenerate input never occurs. Correct in practice.

### C5 — `get_execution_info` field layout vs corelib `TxInfo`
- **Location:** `mod.rs:911-971`.
- **Checked against** `corelib/src/starknet/info.cairo:284-323` (V3 `TxInfo`). The runner's
  write order (version, account_contract_address, max_fee, signature start/end,
  transaction_hash, chain_id, nonce, resource_bounds start/end, tip, paymaster_data
  start/end, nonce_data_availability_mode, fee_data_availability_mode,
  account_deployment_data start/end, proof_facts start/end) **matches exactly**, including
  the newer `proof_facts` span at the end. Correct / up to date.

### C6 — mod-builtin instance `n` field (`n_instances - i`)
- **Location:** `circuit.rs:228` (`fill_instances`).
- **Checked against** vendored `cairo-vm-3.2.0` `builtin_runner/modulo.rs`:
  `N_OFFSET = 6`, `VALUES_PTR_OFFSET = 4`, `OFFSETS_PTR_OFFSET = 5`
  (`modulo.rs:31-33`), `read_inputs` reads `n` at offset 6 and requires `n >= 1`
  (`modulo.rs:314-320`); test fixtures show `n` counting down `…,2,1`
  (`modulo.rs:862-990`). The runner writes `n_instances - i` (first instance = total,
  last = 1) — consistent with `MOD_BUILTIN_INSTANCE_SIZE = 7`, `OFFSETS_PER_GATE = 3`,
  `VALUE_SIZE = 4` (sierra `circuit.rs:45-49`). Correct.

### C7 — `keccak` of an empty span returns `(0, 0)`
- **Location:** `mod.rs:1438-1456`. `data.len() == 0` is a multiple of 17, the round loop
  runs zero times, and the all-zero state yields `(0, 0)` — which is NOT keccak256("").
- **Why it is NOT a valid bug demonstration:** The `keccak_syscall` contract is that the
  input is a non-empty, already-padded multiple of 17 u64 words; the corelib keccak path
  always pads to at least one block, so the syscall never receives an empty span through
  normal use. Reaching this requires unsupported/out-of-contract input, which the task
  explicitly excludes. Recorded as an observation only.

### Other areas verified correct (spot notes)
- **Arithmetic hints** (`mod.rs:1863-1963`): `WideMul128`, `DivMod`, `Uint256DivMod`,
  `Uint512DivModByUint256`, `SquareRoot` — limb decomposition (`div_rem(2^128)`), masks,
  and shifts all consistent; these are witness hints validated by CASM constraints.
- **`Uint256SquareRoot`** (`mod.rs:1965-1998`): `2*sqrt - remainder` cannot underflow
  because `remainder = value - sqrt^2 <= 2*sqrt` for the floor sqrt. Correct.
- **`U256InvModN`** (`mod.rs:2221-2275`): all three branches (`n==1`, non-invertible
  `g!=1`, invertible) verified, including the "force even `g` to 2" trick that guarantees
  the low limb `g0_or_no_inv` is non-zero (an even gcd could be `≡ 0 (mod 2^128)`, which
  would otherwise falsely read as "has inverse"). Correct.
- **`invert_or_nullify`** (`circuit.rs:195-207`): nullifier `= modulus/gcd` satisfies
  `value * nullifier ≡ 0`, and equals 1 when `value ≡ 0 (mod modulus)` (matches
  `circuit_test.rs` cases). Correct.
- **secp256k1/r1 syscalls** (`mod.rs:1500-1737`): `(0,0)` → identity; on-curve/subgroup
  checks gate acceptance; `get_point_from_x` parity selection is sound because `y` and
  `p-y` always have opposite parity (p odd); scalar mult reduces mod group order `Fr`
  which is exact for EC scalar multiplication. Correct.
- **sha256/sha512** (`mod.rs:1458-1496`): 8-word prev state + 16-word block →
  `[u8;64]`/`[u8;128]`, correct compress calls.
- **`DictManagerExecScope`** (`dict_manager.rs`): `idx = trackers.len()` before insert
  gives 0,1,2,… matching `dict_infos` indices; temporary-segment selection matches its
  doc comment. `DictSquashExecScope` pop/delta logic matches the standard squash
  algorithm (indices reversed to descending, smallest popped first, `current - prev - 1`).
- **`calculate_contract_address`** (`contract_address.rs`): prefix, hash-array order, and
  `mod_floor(ADDR_BOUND = 2^251 - 256)` match the documented spec.
- **`deploy`/`call_contract`/`library_call`** (`mod.rs:1005-1151`): deployed-contract
  bookkeeping, constructor-failure rollback, gas refund of `ENTRY_POINT_INITIAL_BUDGET`,
  and revert-reason propagation are all consistent.
- **`fill_values` interleave loop** (`circuit.rs:163-183`) terminates: `mulmod_idx`
  strictly increases each outer iteration until `== n_mul_mods`.

---

## How to verify (spot checks a supervisor can run quickly)
- C1 guard: read `crates/cairo-lang-sierra-to-casm/src/invocations/felt252_dict.rs:297-319`.
- C2/C3 offset convention: read
  `crates/cairo-lang-sierra/src/extensions/modules/circuit.rs:1085-1100`.
- C5 layout: diff `mod.rs:941-959` against `corelib/src/starknet/info.cairo:289-323`.
- C6 builtin `n`: read `cairo-vm-3.2.0/src/vm/runners/builtin_runner/modulo.rs:31-33,314-320`.
- Existing regression coverage: `crates/cairo-lang-runner/src/casm_run/circuit_test.rs`
  and `crates/cairo-lang-runner/src/casm_run/test.rs`.

---

## Files checked (every path inspected)
- `crates/cairo-lang-runner/src/casm_run/mod.rs` — full audit (helpers, MemBuffer,
  syscall dispatch + all syscall handlers, keccak/sha/secp, core hints incl. all
  arithmetic/dict-squash/circuit hints, deprecated hints, cheatcodes, external hints,
  formatting, `run_function`/runner setup). CLEAN.
- `crates/cairo-lang-runner/src/casm_run/circuit.rs` — full audit. CLEAN.
- `crates/cairo-lang-runner/src/casm_run/dict_manager.rs` — full audit. CLEAN.
- `crates/cairo-lang-runner/src/casm_run/contract_address.rs` — full audit. CLEAN.
- `crates/cairo-lang-runner/src/casm_run/circuit_test.rs` — read (existing tests).
- `crates/cairo-lang-runner/src/casm_run/test.rs` — read (existing tests).
- `crates/cairo-lang-sierra/src/extensions/modules/circuit.rs` — read (offset/const
  conventions, C2/C3/C6 cross-check).
- `crates/cairo-lang-sierra-to-casm/src/invocations/felt252_dict.rs` — read (C1 guard).
- `corelib/src/starknet/info.cairo` — read (C5 `TxInfo` layout).
- `~/.cargo/.../cairo-vm-3.2.0/src/vm/runners/builtin_runner/modulo.rs` — read (C6).
