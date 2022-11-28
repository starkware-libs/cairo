use std::fs;
use std::path::PathBuf;

use anyhow::Context;
use clap::Parser;
use compiler::diagnostics::eprint_diagnostic;
use compiler::{compile, CompileArgs, CompileProjectConfig, CompileResult};
use utils::logging::init_logging;

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile
    path: String,
    /// The output file name (default: stdout).
    output: Option<String>,
    /// Replaces sierra ids with human-readable ones.
    #[arg(short, long, default_value_t = false)]
    replace_ids: bool,
}

fn main() -> anyhow::Result<()> {
    init_logging(log::LevelFilter::Off);
    log::info!("Starting Cairo compilation.");

    let args = Args::parse();

    let CompileResult { sierra_program, .. } = compile(CompileArgs {
        compile: CompileProjectConfig::LoadFromPath { path: PathBuf::from(&args.path) },
        on_diagnostic: Some(&mut eprint_diagnostic),
        replace_ids: args.replace_ids,
    })?;

    match args.output {
        Some(path) => {
            fs::write(path, format!("{}", sierra_program)).context("Failed to write output.")?
        }
        None => println!("{}", sierra_program),
    }

    Ok(())
}
