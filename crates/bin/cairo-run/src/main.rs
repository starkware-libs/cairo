//! Compiles and runs a Cairo program.

use std::path::{Path, PathBuf};

use anyhow::Context;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::{check_compiler_path, setup_project};
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_filesystem::ids::CrateInput;
use cairo_lang_runner::casm_run::format_next_item;
use cairo_lang_runner::profiling::ProfilingInfoProcessor;
use cairo_lang_runner::{ProfilingInfoCollectionConfig, SierraCasmRunner, StarknetState};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::program_generator::SierraProgramWithDebug;
use cairo_lang_sierra_generator::replace_ids::{DebugReplacer, SierraIdReplacer};
use cairo_lang_starknet::contract::{find_contracts, get_contracts_info};
use clap::Parser;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Compiles a Cairo project and runs the function `main`.
/// Exits with 1 if the compilation or run fails, otherwise 0.
#[derive(Parser, Debug)]
#[command(version, verbatim_doc_comment)]
struct Args {
    /// The Cairo project path to compile and run.
    path: PathBuf,
    /// Whether path is a single file.
    #[arg(short, long)]
    single_file: bool,
    /// Allows the compilation to succeed with warnings.
    #[arg(long)]
    allow_warnings: bool,
    /// In cases where gas is available, the amount of provided gas.
    #[arg(long)]
    available_gas: Option<usize>,
    /// Whether to print the memory.
    #[arg(long, default_value_t = false)]
    print_full_memory: bool,
    /// Whether to run the profiler.
    #[arg(long, default_value_t = false)]
    run_profiler: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Check if args.path is a file or a directory.
    check_compiler_path(args.single_file, &args.path)?;

    let mut db_builder = RootDatabase::builder();
    db_builder.detect_corelib();
    if args.available_gas.is_none() {
        db_builder
            .skip_auto_withdraw_gas()
            .with_cfg(CfgSet::from_iter([Cfg::kv("gas", "disabled")]));
    }
    let db = &mut db_builder.build()?;

    let main_crate_inputs = setup_project(db, Path::new(&args.path))?;

    let mut reporter = DiagnosticsReporter::stderr().with_crates(&main_crate_inputs);
    if args.allow_warnings {
        reporter = reporter.allow_warnings();
    }
    if reporter.check(db) {
        anyhow::bail!("failed to compile: {}", args.path.display());
    }

    let main_crate_ids = CrateInput::into_crate_ids(db, main_crate_inputs);
    let SierraProgramWithDebug { program: mut sierra_program, debug_info } = db
        .get_sierra_program(main_crate_ids.clone())
        .to_option()
        .context("Compilation failed without any diagnostics.")?
        .clone();
    let replacer = DebugReplacer { db };
    replacer.enrich_function_names(&mut sierra_program);
    if args.available_gas.is_none() && sierra_program.requires_gas_counter() {
        anyhow::bail!("Program requires gas counter, please provide `--available-gas` argument.");
    }

    let contracts = find_contracts(db, &main_crate_ids);
    let contracts_info = get_contracts_info(db, contracts, &replacer)?;
    let sierra_program = replacer.apply(&sierra_program);

    let runner = SierraCasmRunner::new(
        sierra_program.clone(),
        if args.available_gas.is_some() { Some(Default::default()) } else { None },
        contracts_info,
        if args.run_profiler { Some(ProfilingInfoCollectionConfig::default()) } else { None },
    )
    .with_context(|| "Failed setting up runner.")?;
    let result = runner
        .run_function_with_starknet_context(
            runner.find_function("::main")?,
            vec![],
            args.available_gas,
            StarknetState::default(),
        )
        .with_context(|| "Failed to run the function.")?;

    if args.run_profiler {
        match result.profiling_info {
            Some(raw_profiling_info) => {
                let statements_functions =
                    debug_info.statements_locations.get_statements_functions_map_for_tests(db);
                let profiling_info_processor =
                    ProfilingInfoProcessor::new(Some(db), &sierra_program, statements_functions);

                let profiling_info =
                    profiling_info_processor.process(&raw_profiling_info, &Default::default());
                println!("Profiling info:\n{profiling_info}");
            }
            None => println!("Warning: Profiling info not found."),
        }
    }

    match result.value {
        cairo_lang_runner::RunResultValue::Success(values) => {
            println!("Run completed successfully, returning {values:?}")
        }
        cairo_lang_runner::RunResultValue::Panic(values) => {
            print!("Run panicked with [");
            let mut felts = values.into_iter();
            let mut first = true;
            while let Some(item) = format_next_item(&mut felts) {
                if !first {
                    print!(", ");
                }
                first = false;
                print!("{}", item.quote_if_string());
            }
            println!("].")
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
