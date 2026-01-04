use std::collections::HashSet;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use cairo_lang_utils::logging::init_logging;
use clap::{Args, Parser, Subcommand, ValueEnum};
use tracing::info;

use crate::compare::diff_runs;
use crate::config::MetricsConfig;
use crate::database::{Database, SqliteDatabase};
use crate::engine::{get_git_head, run_all};
use crate::format::text_diff;
use crate::tui::run_tui;

mod compare;
mod config;
mod database;
mod engine;
mod format;
mod hyperfine;
mod model;
mod runner;
mod stats;
mod tui;

fn main() -> Result<()> {
    init_logging(tracing::Level::INFO);
    let cli = Cli::parse();

    match cli.command {
        Commands::Run {
            id,
            benchmarks_dir,
            db,
            include,
            exclude,
            iterations,
            warmup,
            engine,
            scenarios,
            phases,
            metrics,
            purge,
        } => {
            let run_id = id.unwrap_or_else(get_git_head);
            let database = SqliteDatabase::open(&db.db)?;

            match purge {
                Some(PurgeMode::Old) => {
                    database.purge_run(&run_id)?;
                    info!("Purged all results for '{}'", run_id);
                }
                Some(PurgeMode::Failed) => {
                    let count = database.purge_failed_benchmarks(&run_id)?;
                    info!("Purged {} failed benchmarks for '{}'", count, run_id);
                }
                None => {}
            }

            let cached = database.list_benchmarks_for_run(&run_id)?;
            let cached: HashSet<String> = cached.into_iter().collect();

            let mut cfg = MetricsConfig::discover(&benchmarks_dir).with_context(|| {
                format!("failed to discover benchmarks in {}", benchmarks_dir.display())
            })?;

            if let Some(iterations) = iterations {
                cfg.defaults.runs = iterations;
            }
            if let Some(warmup) = warmup {
                cfg.defaults.warmup = warmup;
            }
            if !include.is_empty() {
                cfg.benchmarks.retain(|bench| include.iter().any(|p| bench.name.contains(p)));
            }
            if !exclude.is_empty() {
                cfg.benchmarks.retain(|bench| exclude.iter().all(|p| !bench.name.contains(p)));
            }

            if cfg.benchmarks.is_empty() {
                info!("No benchmarks to run (filtered out by include/exclude)");
                return Ok(());
            }

            let results = run_all(cfg, engine, &scenarios, &phases, &metrics, &cached)?;

            database.store_run(&run_id, &results)?;
            info!("Results stored with id '{}'", run_id);
        }

        Commands::Compare { baseline, current, phases, db } => match (baseline, current) {
            (Some(baseline), Some(current)) => {
                let mut base = load_result(&baseline, &db.db)?;
                let mut cur = load_result(&current, &db.db)?;

                if !phases.is_empty() {
                    let phase_strs: Vec<&str> = phases.iter().map(Phase::as_str).collect();
                    base.benchmarks.retain(|b| phase_strs.contains(&b.phase.as_str()));
                    cur.benchmarks.retain(|b| phase_strs.contains(&b.phase.as_str()));
                }

                let diff = diff_runs(&base, &cur);
                print!("{}", text_diff(&diff));
            }
            (None, None) => {
                run_tui(&db.db)?;
            }
            _ => {
                anyhow::bail!("Either provide both baseline and current, or neither (for TUI)");
            }
        },

        Commands::List { limit, db } => {
            let database = SqliteDatabase::open(&db.db)?;
            let runs = database.list_runs(limit)?;

            if runs.is_empty() {
                println!("No stored runs.");
            } else {
                println!("{:<20} {:<28} Git Commit", "ID", "Timestamp");
                println!("{}", "-".repeat(70));
                for run in runs {
                    println!(
                        "{:<20} {:<28} {}",
                        run.id,
                        run.timestamp,
                        run.git_commit.unwrap_or_else(|| "-".into())
                    );
                }
            }
        }

        Commands::History { benchmark, limit, db } => {
            let database = SqliteDatabase::open(&db.db)?;
            let entries = database.history(&benchmark, limit)?;

            if entries.is_empty() {
                println!("No history for benchmark '{}'.", benchmark);
                println!("\nAvailable benchmarks:");
                for name in database.list_benchmarks()? {
                    println!("  {}", name);
                }
            } else {
                println!("History for: {}", benchmark);
                println!();
                println!("{:<20} {:<28} {:<12} Git Commit", "Run ID", "Timestamp", "Median");
                println!("{}", "-".repeat(80));
                for entry in entries {
                    println!(
                        "{:<20} {:<28} {:<12} {}",
                        entry.run_id,
                        entry.timestamp,
                        format::format_duration(entry.median_ns),
                        entry.git_commit.unwrap_or_else(|| "-".into())
                    );
                }
            }
        }

        Commands::Purge { id, mode, db } => {
            let database = SqliteDatabase::open(&db.db)?;
            match mode {
                PurgeMode::Old => {
                    database.purge_run(&id)?;
                    info!("Purged all results for '{}'", id);
                }
                PurgeMode::Failed => {
                    let count = database.purge_failed_benchmarks(&id)?;
                    info!("Purged {} failed benchmarks for '{}'", count, id);
                }
            }
        }
    }

    Ok(())
}

fn load_result(id: &str, db_path: &Path) -> Result<model::RunResult> {
    let database = SqliteDatabase::open(db_path)?;
    database.load_run(id).with_context(|| format!("failed to load run '{}' from database", id))
}

/// Timing engine to use for benchmarks.
#[derive(Debug, Clone, Copy, ValueEnum, Default)]
pub enum Engine {
    /// Use hyperfine if available, otherwise fall back to builtin.
    #[default]
    Auto,
    /// Always use the builtin timing engine.
    Builtin,
    /// Always use hyperfine (fails if not installed).
    Hyperfine,
}

/// Purge mode for cached results.
#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum PurgeMode {
    /// Delete entire run (all benchmarks for this run ID).
    Old,
    /// Delete only failed benchmarks (success = false) for this run ID.
    Failed,
}

/// Build scenario for benchmarks.
#[derive(Debug, Clone, Copy, ValueEnum, Default, PartialEq, Eq, Hash)]
pub enum Scenario {
    /// Clean build: delete target directory before each run.
    #[default]
    Clean,
    /// Incremental build: keep target directory between runs.
    Incremental,
}

impl Scenario {
    /// Returns the scenario name as a string.
    pub fn as_str(&self) -> &str {
        match self {
            Scenario::Clean => "clean",
            Scenario::Incremental => "incremental",
        }
    }

    /// Returns default scenarios (clean only; incremental requires cairo compiler support).
    pub fn defaults() -> Vec<Scenario> {
        vec![Scenario::Clean]
    }
}

/// Metric type to measure.
#[derive(Debug, Clone, Copy, ValueEnum, Default, PartialEq, Eq, Hash)]
pub enum Metric {
    /// Wall-clock execution time.
    #[default]
    Walltime,
}

impl Metric {
    /// Returns the metric name as a string.
    pub fn as_str(&self) -> &str {
        match self {
            Metric::Walltime => "walltime",
        }
    }
}

/// Compilation phase to measure.
#[derive(Debug, Clone, Copy, ValueEnum, Default, PartialEq, Eq, Hash)]
pub enum Phase {
    /// Diagnostics validation (currently same as Sierra with library calls).
    Diagnostics,
    /// Cairo to Sierra IR generation.
    #[default]
    Sierra,
    /// Sierra to CASM code generation.
    Casm,
    /// Full compilation pipeline: source to Sierra to CASM.
    Full,
}

impl Phase {
    /// Returns the phase name as a string.
    pub fn as_str(&self) -> &str {
        match self {
            Phase::Diagnostics => "diagnostics",
            Phase::Sierra => "sierra",
            Phase::Casm => "casm",
            Phase::Full => "full",
        }
    }

    /// Returns all available phases.
    pub fn all() -> Vec<Phase> {
        vec![Phase::Diagnostics, Phase::Sierra, Phase::Casm, Phase::Full]
    }
}

#[derive(Subcommand)]
enum Commands {
    /// Run benchmarks and store results.
    Run {
        /// Identifier for this run (stored in database).
        id: Option<String>,

        /// Directory containing benchmark subdirectories with benchmark.toml files.
        #[arg(long, default_value = "crates/bin/cairo-metrics/benchmarks")]
        benchmarks_dir: PathBuf,

        #[command(flatten)]
        db: DbOption,

        /// Run only benchmarks whose name contains one of these substrings.
        #[arg(long, value_delimiter = ',')]
        include: Vec<String>,

        /// Skip benchmarks whose name contains one of these substrings.
        #[arg(long, value_delimiter = ',')]
        exclude: Vec<String>,

        /// Override default number of measured iterations for all benchmarks.
        #[arg(long)]
        iterations: Option<usize>,

        /// Override default number of warmup runs for all benchmarks.
        #[arg(long)]
        warmup: Option<usize>,

        /// Timing engine: auto (default), builtin, or hyperfine.
        #[arg(long, value_enum, default_value_t = Engine::Auto)]
        engine: Engine,

        /// Build scenarios: clean (default) or incremental (not yet supported).
        #[arg(long, value_enum, value_delimiter = ',', default_values_t = Scenario::defaults())]
        scenarios: Vec<Scenario>,

        /// Compilation phases to measure: diagnostics, sierra, casm, or full.
        #[arg(long, value_enum, value_delimiter = ',', default_values_t = Phase::all())]
        phases: Vec<Phase>,

        /// Metrics to measure.
        #[arg(long, value_enum, value_delimiter = ',', default_value = "walltime")]
        metrics: Vec<Metric>,

        /// Purge cached results: 'old' deletes all, 'failed' deletes only failed benchmarks.
        #[arg(long, value_enum)]
        purge: Option<PurgeMode>,
    },

    /// Compare two benchmark runs. Without arguments, launches interactive TUI.
    Compare {
        /// Baseline run ID from database. Omit to use TUI selector.
        baseline: Option<String>,

        /// Current run ID from database. Omit to use TUI selector.
        current: Option<String>,

        /// Filter comparison to specific phases.
        #[arg(long, value_enum, value_delimiter = ',')]
        phases: Vec<Phase>,

        #[command(flatten)]
        db: DbOption,
    },

    /// List stored benchmark runs.
    List {
        /// Maximum number of runs to show.
        #[arg(long, default_value = "100")]
        limit: u32,

        #[command(flatten)]
        db: DbOption,
    },

    /// Show history for a specific benchmark.
    History {
        /// Benchmark name to show history for.
        benchmark: String,

        /// Maximum number of results to show.
        #[arg(long, default_value = "100")]
        limit: u32,

        #[command(flatten)]
        db: DbOption,
    },

    /// Delete a run from the database.
    Purge {
        /// Run ID to delete.
        id: String,

        /// Purge mode: 'old' deletes entire run, 'failed' deletes only failed benchmarks.
        #[arg(long, value_enum, default_value_t = PurgeMode::Old)]
        mode: PurgeMode,

        #[command(flatten)]
        db: DbOption,
    },
}

/// Shared database path option.
#[derive(Debug, Args)]
struct DbOption {
    /// Path to SQLite database for storing benchmark results.
    /// Defaults to `results.db`, which is ignored by git.
    #[arg(long, default_value = "results.db")]
    db: PathBuf,
}

#[derive(Parser)]
#[command(name = "cairo-metrics", version, about = "Benchmark harness for the Cairo compiler")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}
