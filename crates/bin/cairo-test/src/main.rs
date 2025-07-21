//! Compiles and runs a Cairo program.

use std::path::PathBuf;

use anyhow::Ok;
use cairo_lang_compiler::project::check_compiler_path;
use cairo_lang_runner::profiling::ProfilerConfig;
use cairo_lang_test_runner::{TestRunConfig, TestRunner};
use clap::{Parser, ValueEnum};
use serde::Serialize;

/// A clap-arg wrapper for Option<[ProfilerConfig]>.
#[derive(ValueEnum, Clone, Default, Debug, Serialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "kebab-case")]
enum RunProfilerConfigArg {
    #[default]
    None,
    Cairo,
    Sierra,
}
impl From<RunProfilerConfigArg> for Option<ProfilerConfig> {
    fn from(val: RunProfilerConfigArg) -> Self {
        match val {
            RunProfilerConfigArg::None => None,
            RunProfilerConfigArg::Cairo => Some(ProfilerConfig::Cairo),
            RunProfilerConfigArg::Sierra => Some(ProfilerConfig::Sierra),
        }
    }
}

/// Compiles a Cairo project and runs all the functions marked as `#[test]`.
/// Exits with 1 if the compilation or run fails, otherwise 0.
#[derive(Parser, Debug)]
#[command(version, verbatim_doc_comment)]
struct Args {
    /// The Cairo project path to compile and run its tests.
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
    /// [cairo_lang_runner::profiling::ProfilerConfig]
    #[arg(short, long, default_value_t, value_enum)]
    run_profiler: RunProfilerConfigArg,
    /// Should disable gas calculation.
    #[arg(long)]
    gas_disabled: bool,
    /// Whether to print resource usage after each test.
    #[arg(long, default_value_t = false)]
    print_resource_usage: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Check if args.path is a file or a directory.
    check_compiler_path(args.single_file, &args.path)?;

    let config = TestRunConfig {
        filter: args.filter,
        ignored: args.ignored,
        include_ignored: args.include_ignored,
        profiler_config: args.run_profiler.into(),
        gas_enabled: !args.gas_disabled,
        print_resource_usage: args.print_resource_usage,
    };

    let runner = TestRunner::new(&args.path, args.starknet, args.allow_warnings, config)?;
    runner.run()?;

    Ok(())
}
