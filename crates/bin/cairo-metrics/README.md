# cairo-metrics

A benchmark harness for the Cairo compiler.

## Features

- Walltime benchmarks using `cairo-compile`
- Robust statistics (median, MAD) for noisy CI environments
- SQLite storage for historical tracking
- Baseline comparison with TUI
- Optional hyperfine backend for enhanced timing accuracy

## Quick Start

```bash
# Run all benchmarks (results stored in results.db)
cargo run -p cairo-metrics -- run

# Run specific benchmarks
cargo run -p cairo-metrics -- run --include corelib

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

```
benchmarks/
  corelib/
    benchmark.toml
  myproject/
    benchmark.toml
```

### benchmark.toml

```toml
# Path to the Cairo project (relative to repo root)
path = "corelib/"

# Optional: mark as library project
library = true
```

| Field     | Required | Description                        |
| --------- | -------- | ---------------------------------- |
| `path`    | yes      | Path to Cairo project directory    |
| `library` | no       | Mark as library project (optional) |

## Timing Engines

Two timing backends are available:

- **builtin**: Direct process execution with Rust timing
- **hyperfine**: Uses hyperfine for shell calibration and statistics

By default, hyperfine is used if installed. Use `--engine builtin` to force the builtin engine.

## Output

Results stored in SQLite include:

- Metadata (timestamp, git commit)
- Per-benchmark timing stats (median, mean, stddev, MAD, min, max)
- Raw timing data for each run

## Future Work

- Incremental compilation benchmarks (patch logic implemented, awaiting cairo compiler support)
- Additional metrics beyond walltime
