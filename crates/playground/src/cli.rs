use std::collections::HashMap;
use std::path::Path;
use std::process::ExitCode;
use std::sync::Arc;

use cairo_rs::types::relocatable::MaybeRelocatable;
use casm::{casm, casm_extend};
use clap::Parser;
use compiler::db::RootDatabase;
use compiler::diagnostics::check_diagnostics;
use compiler::project::setup_project;
use itertools::{chain, Itertools};
use sierra::program::StatementIdx;
use sierra_gas::calc_gas_info;
use sierra_gas::gas_info::GasInfo;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::replace_sierra_ids_in_program;
use sierra_to_casm::metadata::Metadata;

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile and run.
    path: String,
    /// In cases where gas is available, the amount of provided gas.
    available_gas: Option<usize>,
}

fn main() -> ExitCode {
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

    let Some(sierra_program) = db.get_sierra_program() else {
        eprintln!("Compilation failed without any diagnostics.");
        return ExitCode::FAILURE;
    };
    let function_sizes =
        HashMap::<StatementIdx, (usize, usize)>::from_iter(sierra_program.funcs.iter().map(|f| {
            let mut input_size = 0;
            for ty in &f.signature.param_types {
                input_size += db.get_type_info(ty.clone()).unwrap().size;
            }
            let mut output_size = 0;
            for ty in &f.signature.ret_types {
                output_size += db.get_type_info(ty.clone()).unwrap().size;
            }
            (f.entry_point, (input_size, output_size))
        }));

    let sierra_program = Arc::new(replace_sierra_ids_in_program(db, &sierra_program));
    let main_func =
        if let Some(f) = sierra_program.funcs.iter().find(|f| {
            if let Some(name) = &f.id.debug_name { name.ends_with("::main") } else { false }
        }) {
            f
        } else {
            eprintln!("Main function not provided in module.");
            return ExitCode::FAILURE;
        };
    let gas_info = if args.available_gas.is_some() {
        match calc_gas_info(&sierra_program) {
            Ok(gas_info) => gas_info,
            Err(err) => {
                eprintln!(
                    "Failed calculating gas usage, it is likely a call for `get_gas` is missing. \
                     Original error: {err}."
                );
                return ExitCode::FAILURE;
            }
        }
    } else {
        GasInfo { variable_values: HashMap::new(), function_costs: HashMap::new() }
    };
    let metadata = Metadata { function_ap_change: HashMap::new(), gas_info };
    let program = match sierra_to_casm::compiler::compile(
        &sierra_program,
        &metadata,
        args.available_gas.is_some(),
    ) {
        Ok(program) => program,
        Err(err) => {
            eprintln!("Failed lowering to casm. Original error: {err}.");
            return ExitCode::FAILURE;
        }
    };
    let mut ctx = casm! {};
    for (i, ty) in main_func.signature.param_types.iter().enumerate() {
        if &main_func.signature.ret_types[i] != ty {
            eprintln!("We only support main functions with no parameters.",);
            return ExitCode::FAILURE;
        }
        if ty == &"RangeCheck".into() {
            casm_extend! {ctx,
                %{ memory[ap + 0] = segments.add() %}
                ap += 1;
            }
        } else if ty == &"GasBuiltin".into() {
            if let Some(available_gas) = args.available_gas {
                let initial_gas = available_gas
                    .checked_sub(metadata.gas_info.function_costs[&main_func.id] as usize);
                if let Some(initial_gas) = initial_gas {
                    casm_extend! {ctx,
                        [ap + 0] = initial_gas, ap++;
                    }
                } else {
                    eprintln!("Not enough gas to call function.",);
                    return ExitCode::FAILURE;
                }
            } else {
                eprintln!("GasBuiltin is required while no `available_gas` value provided.",);
                return ExitCode::FAILURE;
            }
        } else {
            eprintln!("Inputs for main are not supported.");
            return ExitCode::FAILURE;
        }
    }
    let (input_size, output_size) = function_sizes[&main_func.entry_point];

    let before_final_call = ctx.current_code_offset;
    let final_call_size = 3;
    let offset = final_call_size
        + program.debug_info.sierra_statement_info[main_func.entry_point.0].code_offset;
    casm_extend! {ctx,
        call rel offset;
        ret;
    }
    assert_eq!(before_final_call + final_call_size, ctx.current_code_offset);
    println!("{}", ctx.instructions.iter().join("\n"));
    println!("{}", program);
    let results = casm::run::run_function(
        chain!(ctx.instructions, program.instructions).collect(),
        output_size,
    );
    print!("Returned memory values: [");
    for result in &results[input_size..] {
        match result {
            None => print!("Uninitialized, "),
            Some(MaybeRelocatable::Int(value)) => print!("{value}, "),
            Some(MaybeRelocatable::RelocatableValue(relocatable)) => {
                print!("&segment[{}][{}], ", relocatable.segment_index, relocatable.offset);
            }
        }
    }
    println!("]");

    ExitCode::SUCCESS
}
