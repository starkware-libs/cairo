use std::fs;
use std::path::Path;
use std::process::ExitCode;
use std::sync::Arc;

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

fn main() -> ExitCode {
    init_logging();
    log::info!("Starting Cairo compilation.");

    let args = Args::parse();

    let mut db_val = RootDatabase::default();
    let db = &mut db_val;

    if let Err(error) = setup_project(db, Path::new(&args.path)) {
        eprintln!("{}", error);
        return ExitCode::FAILURE;
    }

    if check_diagnostics(db) {
        return ExitCode::FAILURE;
    }

    let Some(mut sierra_program) = db.get_sierra_program() else {
        eprintln!("Compilation failed without any diagnostics.");
        return ExitCode::FAILURE;
    };

    if args.replace_ids {
        sierra_program = Arc::new(replace_sierra_ids_in_program(db, &sierra_program));
    }

    match args.output {
        Some(path) => {
            fs::write(path, format!("{}", sierra_program)).expect("Failed to write output.")
        }
        None => println!("{}", sierra_program),
    }

    ExitCode::SUCCESS
}
