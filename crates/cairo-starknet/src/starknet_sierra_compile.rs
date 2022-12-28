use std::fs;

use anyhow::Context;
use clap::Parser;
use starknet::casm_contract_class::CasmContractClass;
use starknet::contract_class::ContractClass;

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

    // TODO(ilya): Fix entry points.
    let casm_contract = CasmContractClass::from_contract_class(contract_class)
        .with_context(|| "Compilation failed.")?;

    let res =
        serde_json::to_string_pretty(&casm_contract).with_context(|| "Serialization failed.")?;

    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write output.")?,
        None => println!("{}", res),
    }

    Ok(())
}
