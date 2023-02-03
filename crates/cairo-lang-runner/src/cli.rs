//! Compiles and runs a Cairo program.

use std::path::Path;

use anyhow::{Context, Ok};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_runner::SierraCasmRunner;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use clap::Parser;

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile and run.
    #[arg(short, long)]
    path: String,
    /// In cases where gas is available, the amount of provided gas.
    #[arg(long)]
    available_gas: Option<usize>,
    /// Whether to print the memory.
    #[arg(long, default_value_t = false)]
    print_full_memory: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let db = &mut RootDatabase::builder().detect_corelib().build()?;

    let main_crate_ids = setup_project(db, Path::new(&args.path))?;

    if DiagnosticsReporter::stderr().check(db) {
        anyhow::bail!("failed to compile: {}", args.path);
    }

    let sierra_program = db
        .get_sierra_program(main_crate_ids)
        .to_option()
        .with_context(|| "Compilation failed without any diagnostics.")?;
    let runner = SierraCasmRunner::new(
        replace_sierra_ids_in_program(db, &sierra_program),
        args.available_gas.is_some(),
    )
    .with_context(|| "Failed setting up runner.")?;
    let result = runner
        .run_function("::main", &[], args.available_gas)
        .with_context(|| "Failed to run the function.")?;
    match result.value {
        cairo_lang_runner::RunResultValue::Success(values) => {
            println!("Run completed successfully, returning {values:?}")
        }
        cairo_lang_runner::RunResultValue::Panic(values) => {
            println!("Run panicked with err values: {values:?}")
        }
    }
    if let Some(gas) = result.gas_counter {
        println!("Remaining gas: {gas}");
    }
    if args.print_full_memory {
        print!("Full memory: [");
        for cell in &result.memory {
            match cell {
                None => print!("_, "),
                Some(value) => print!("{value}, "),
            }
        }
        println!("]");
    }
    Ok(())
}
