use std::collections::HashMap;
use std::fs;

use anyhow::Context;
use clap::Parser;
use sierra_gas::calc_gas_info;
use sierra_to_casm::metadata::Metadata;
use starknet::contract_class::{CasmContractClass, ContractClass, ContractEntryPoints};

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile
    file: String,
    /// The output file name (default: stdout).
    output: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let contract_class: ContractClass = serde_json::from_str(
        &fs::read_to_string(&args.file)
            .with_context(|| format!("Failed to read {}.", &args.file))?,
    )
    .with_context(|| "deserialization Failed.")?;

    let program = contract_class.sierra_program;
    let gas_info = calc_gas_info(&program).with_context(|| "Failed calculating gas variables.")?;

    let gas_usage_check = true;
    let cairo_program = sierra_to_casm::compiler::compile(
        &program,
        &Metadata { function_ap_change: HashMap::new(), gas_info },
        gas_usage_check,
    )
    .with_context(|| "Compilation failed.")?;

    let mut bytecode = vec![];
    for instruction in cairo_program.instructions {
        bytecode.extend(instruction.assemble().encode());
    }

    // TODO(ilya): Fix entry points.
    let contract =
        CasmContractClass { bytecode, entry_points_by_type: ContractEntryPoints::default() };

    let res = serde_json::to_string_pretty(&contract).with_context(|| "Serialization failed.")?;

    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write output.")?,
        None => println!("{}", res),
    }

    Ok(())
}
