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
use starknet::contract_class::{ContractClass, ContractEntryPoints};

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
    let args = Args::parse();

    let mut db_val = RootDatabase::default();
    let db = &mut db_val;

    if setup_project(db, Path::new(&args.path)).is_err() {
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

    // TODO(ilya): Get entry points from the code.

    let contract = ContractClass {
        sierra_program: format!("{}", sierra_program),
        entry_points_by_type: ContractEntryPoints::default(),
    };

    let res = serde_json::to_string_pretty(&contract).unwrap();

    match args.output {
        Some(path) => fs::write(path, format!("{}", res)).expect("Failed to write output."),
        None => println!("{}", res),
    }

    ExitCode::SUCCESS
}
