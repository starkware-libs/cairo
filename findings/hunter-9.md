# Hunter 9 — sierra-type-size + sierra program/registry

> NOTE: This file was reconstructed by the orchestrator from Hunter 9's completion
> report because the agent's own write did not persist to the findings directory.
> Content is verbatim from the agent's final result.

## Result: No bug found (honest negative)

Extensive line-by-line audit of the assigned files; no genuine bug found. Did not
write a test since there was no bug to demonstrate.

### Primary files reviewed
- `crates/cairo-lang-sierra-type-size/src/lib.rs` (`get_type_size_map`, every match arm)
- `crates/cairo-lang-sierra/src/program_registry.rs` (registry construction, id-collision, statement/branch validation)
- `crates/cairo-lang-sierra/src/program.rs` (Program/TypeDeclaration/Version, `requires_gas_counter`)
- `crates/cairo-lang-sierra/src/ids.rs` (identity macros, `UserTypeId::from_string` hashing/truncation)
- `crates/cairo-lang-sierra/src/extensions/types.rs`, `type_specialization_context.rs`, `mod.rs`

### Verification method
For every non-trivial size formula in `lib.rs`, cross-checked the derived size against the
actual field layout consumed downstream in `cairo-lang-sierra-to-casm` (read-only cross-ref):
- `Enum` size = `1 + max(variant sizes)` — matches `invocations/enm.rs` padding logic.
- `Struct` size = sum of member sizes, `continue` when `!storable` — consistent with `structure.rs`.
- `Box`/`Nullable` = 1; `Array`/`Span`/`SquashedFelt252Dict`/`IntRange` = 2; `EcPoint` = 2; `EcState` = 3; `Uint128MulGuarantee` = 4 — matched against `invocations/ec.rs` cell counts.
- `Circuit*` fixed sizes matched cell-by-cell against `invocations/circuit.rs` (`assert_eq!(guarantee.len(), limb_count*2)`).
- `NonZero`/`Snapshot`/`Uninitialized` pass through wrapped type size; wrapper `calc_info` requires storable wrapped type, so the missing-dependency error path is unreachable for legal programs.
- `IntRange` fixed size 2 — `range.rs check_inner_type` restricts wrapped type to size-1 types.
- Match is exhaustive over all 42 `CoreTypeConcrete` variants (no wildcard) → missing-variant would be a compile error.

### Edge cases checked
empty struct (sum=0, `zero_sized=true`), empty enum (0 variants → size 1 tag-only), zero-sized
member propagation, overflow guards (`checked_add`/`checked_mul` on `i16`).

### Registry structure
Duplicate type/libfunc/function id detection, `TypeInfoDeclarationMismatch`, and branch/jump
validation (`validate_statement`: branch-align bijection, `BranchBackwards`,
`MultipleJumpsToSameStatement`, `JumpOutOfRange`) all traced internally consistent.

### ids.rs
`UserTypeId::from_string` Keccak truncation (`result[0] &= 3`, 250 bits) byte-for-byte identical
to `cairo-lang-starknet-classes/src/keccak.rs`. 64-bit FNV1a hash is a pre-existing pattern.

## Files checked
- crates/cairo-lang-sierra-type-size/src/lib.rs
- crates/cairo-lang-sierra/src/program_registry.rs
- crates/cairo-lang-sierra/src/program.rs
- crates/cairo-lang-sierra/src/ids.rs
- crates/cairo-lang-sierra/src/extensions/types.rs
- crates/cairo-lang-sierra/src/extensions/type_specialization_context.rs
- crates/cairo-lang-sierra/src/extensions/mod.rs
- crates/cairo-lang-sierra/src/extensions/core.rs
- crates/cairo-lang-sierra/src/extensions/modules/{structure,enm,array,boxing,non_zero,snapshot,uninitialized,range,circuit,coupon,gas,blake}.rs (read-only cross-ref)
- crates/cairo-lang-sierra-to-casm/src/invocations/{enm,ec,circuit}.rs, compiler.rs (read-only cross-ref)
