//! Compiles and runs a Cairo program.

use std::path::PathBuf;

use anyhow::Ok;
use cairo_lang_compiler::project::check_compiler_path;
use cairo_lang_test_runner::{self, TestRunConfig, TestRunner};
use clap::{Parser, ValueEnum};
use serde::Serialize;

/// The clap-arg equivalent of [cairo_lang_test_runner::RunProfilerConfig].
#[derive(ValueEnum, Clone, Default, Debug, Serialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "kebab-case")]
enum RunProfilerConfig {
    #[default]
    None,
    Cairo,
    Sierra,
}
impl Into<cairo_lang_test_runner::RunProfilerConfig> for RunProfilerConfig {
    fn into(self) -> cairo_lang_test_runner::RunProfilerConfig {
        match self {
            RunProfilerConfig::None => cairo_lang_test_runner::RunProfilerConfig::None,
            RunProfilerConfig::Cairo => cairo_lang_test_runner::RunProfilerConfig::Cairo,
            RunProfilerConfig::Sierra => cairo_lang_test_runner::RunProfilerConfig::Sierra,
        }
    }
}

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The path to compile and run its tests.
    path: PathBuf,
    /// Whether path is a single file.
    #[arg(short, long)]
    single_file: bool,
    /// Allows the compilation to succeed with warnings.
    #[arg(long)]
    allow_warnings: bool,
    /// The filter for the tests, running only tests containing the filter string.
    #[arg(short, long, default_value_t = String::default())]
    filter: String,
    /// Should we run ignored tests as well.
    #[arg(long, default_value_t = false)]
    include_ignored: bool,
    /// Should we run only the ignored tests.
    #[arg(long, default_value_t = false)]
    ignored: bool,
    /// Should we add the starknet plugin to run the tests.
    #[arg(long, default_value_t = false)]
    starknet: bool,
    /// Whether to run the profiler, and what results to produce. See
    /// [cairo_lang_test_runner::RunProfilerConfig]
    #[clap(short, long, default_value_t, value_enum)]
    run_profiler: RunProfilerConfig,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Check if args.path is a file or a directory.
    check_compiler_path(args.single_file, &args.path)?;

    let config = TestRunConfig {
        filter: args.filter,
        ignored: args.ignored,
        include_ignored: args.include_ignored,
        run_profiler: args.run_profiler.into(),
    };

    let runner = TestRunner::new(&args.path, args.starknet, args.allow_warnings, config)?;
    runner.run()?;

    Ok(())
}
