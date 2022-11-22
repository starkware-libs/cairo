use std::fs;
use std::path::Path;
use std::sync::Arc;

use anyhow::Context;
use clap::Parser;
use compiler::db::RootDatabase;
use compiler::diagnostics::check_diagnostics;
use compiler::project::setup_project;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::replace_sierra_ids_in_program;
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
    /// Replaces sierra ids with human readable ones.
    #[arg(short, long, default_value_t = false)]
    replace_ids: bool,
}

fn main() -> anyhow::Result<()> {
    init_logging(log::LevelFilter::Off);
    log::info!("Starting Cairo compilation.");

    let args = Args::parse();

    let mut db_val = RootDatabase::default();
    let db = &mut db_val;

    setup_project(db, Path::new(&args.path))?;

    if check_diagnostics(db) {
        anyhow::bail!("failed to compile: {}", args.path);
    }

    let mut sierra_program =
        db.get_sierra_program().with_context(|| "Compilation failed without any diagnostics.")?;

    if args.replace_ids {
        sierra_program = Arc::new(replace_sierra_ids_in_program(db, &sierra_program));
    }

    match args.output {
        Some(path) => fs::write(path, format!("{}", sierra_program))
            .with_context(|| "Failed to write output.")?,
        None => println!("{}", sierra_program),
    }

    Ok(())
}
