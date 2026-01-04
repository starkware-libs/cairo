# cairo-metrics

A benchmark harness for the Cairo compiler.

## Features

-   Walltime comparison (more metrics coming up)
-   Clean and incremental build scenarios
-   Patch-based incremental benchmarks for deterministic cache testing
-   Robust statistics (median, MAD) for noisy CI environments
-   Storage for historical tracking --- sqlite currently, mostly for local tracking, but extendible
    to a stateful RDB service.
-   Baseline comparison with TUI
-   Optional hyperfine backend for enhanced timing accuracy

## Quick Start

```bash
# Run all benchmarks (results stored in results.db)
cargo run -p cairo-metrics -- run

# Run specific benchmarks
cargo run -p cairo-metrics -- run --include corelib --exclude perpetual

# Run only clean builds (skip incremental)
cargo run -p cairo-metrics -- run --scenarios clean

# Force builtin engine (for CI without hyperfine)
cargo run -p cairo-metrics -- run --engine builtin

# Compare two runs (interactive TUI if no args)
cargo run -p cairo-metrics -- compare baseline-sha current-sha

# List stored runs
cargo run -p cairo-metrics -- list

# Show history for a benchmark
cargo run -p cairo-metrics -- history corelib-clean-full-walltime
```

## Adding Benchmarks

Benchmarks are discovered from subdirectories containing a `benchmark.toml`:
Currently they are vendored copies of the projects sans `.git`, we don't want to mess with
submodules, and the projects should be bumped manually by re-cloning new versions periodically.

```
benchmarks/
  corelib/
    benchmark.toml
  openzeppelin/
    benchmark.toml
    0-add-function.patch   # Optional: for incremental benchmarks
    1-modify-import.patch
```

### benchmark.toml

```toml
# Path to the Scarb project (relative to repo root)
path = "corelib/"

# Optional: mark as library (skips incremental benchmarks and CASM phase)
library = true
```

| Field     | Required | Description                                           |
| --------- | -------- | ----------------------------------------------------- |
| `path`    | yes      | Path to Scarb project directory                       |
| `library` | no       | If true, skip incremental benchmarks (default: false) |

### Incremental Benchmarks with Patches

For deterministic incremental build testing, add `.patch` files named `N-description.patch`:

```
0-add-println.patch
1-remove-import.patch
```

Each patch is applied to a fresh build, and the rebuild time is measured.

## Timing Engines

Two timing backends are available:

-   **builtin**: Direct process execution with Rust timing
-   **hyperfine**: Uses hyperfine for shell calibration and statistics

By default, hyperfine is used if installed. Use `--engine builtin` to force the builtin engine.
Currently not used in the CI, requires installation with `sudo apt install hyperfine`.

## Output

Results stored in SQLite include:

-   Metadata (timestamp, git commit)
-   Per-benchmark timing stats (median, mean, stddev, MAD, min, max)
-   Raw timing data for each run
