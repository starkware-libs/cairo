# Review: PR #9739 - Bumped version of dependencies

**Author:** orizi
**Branch:** `orizi/03-11-bumped_version_of_dependencies` → `main`

## Summary

This PR bumps several Rust dependencies to newer versions. The changes span 5 files (Cargo.toml, Cargo.lock, two crate-specific Cargo.tomls, and one source file).

## Dependency Changes

| Dependency | Old Version | New Version | Bump Type |
|------------|------------|-------------|-----------|
| `cairo-vm` | 3.1.0 | 3.2.0 | Minor |
| `lalrpop` / `lalrpop-util` | 0.22.2 | 0.23.1 | Minor |
| `rand` | 0.9.2 | 0.10.0 | **Major** (0.x) |
| `toml` | 0.9.5 | 1.0.6 | **Major** |
| `anstream` (dev-dep) | 0.6.19 | 1.0.0 | **Major** |

## Code Changes Analysis

### `crates/cairo-lang-runner/src/casm_run/mod.rs`

The only source code change adapts to the `rand` 0.10.0 API:

1. **Import:** `rand::Rng` → `rand::RngExt` (line 45)
   - In `rand` 0.10, the `random()` method for generating arbitrary values moved to the `RngExt` extension trait.

2. **Trait bound:** `rand::RngCore` → `rand::Rng` (line 1811, `random_ec_point` function)
   - In `rand` 0.10, the old `RngCore` trait was renamed to `Rng` (the base trait for RNGs).

Both changes are correct and consistent with the `rand` 0.10 migration guide.

## Findings

### No Issues Found (Looks Good)

1. **`rand` migration is correct.** The `RngExt` import provides the `.random()` method used at line 1822 (`rng.random()`), and the trait bound `rand::Rng` correctly replaces `rand::RngCore`. The call site at line 1991 (`rand::rng()`) returns a type implementing `rand::Rng`, so it remains compatible.

2. **`toml` 0.9 → 1.0 is safe.** The codebase only uses `toml::from_str`, `toml::to_string`, `toml::to_string_pretty`, and `toml::de::Error` — all of which have stable APIs across this version transition.

3. **`lalrpop` 0.22 → 0.23 bump** is applied consistently in both the build dependency (`cairo-lang-sierra/Cargo.toml`) and runtime dependency (`Cargo.toml` workspace for `lalrpop-util`).

4. **`anstream` 0.6 → 1.0** is a dev-dependency only (in `cairo-lang-parser`), so the risk is limited to test tooling.

5. **`cairo-vm` 3.1 → 3.2** is a minor version bump, expected to be backwards compatible.

### Potential Concern

- **Merge conflict**: The PR is currently blocked due to a merge conflict with the target branch (as noted by Graphite). This needs to be resolved before merging.

## Verdict

**Approve** — The dependency bumps are straightforward and the code adaptation for `rand` 0.10 is correct. The only blocker is the merge conflict that needs resolution.
