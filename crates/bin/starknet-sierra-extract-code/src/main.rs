use std::fs;

use anyhow::Context;
use cairo_lang_starknet_classes::contract_class::ContractClass;
use clap::Parser;

/// Extracts sierra code from a contract class file.
/// Exits with 0/1 if the extraction succeeds/fails.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The path of the file with the contract class.
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
    let sierra_program = contract_class
        .extract_sierra_program()
        .with_context(|| "Failed parsing felt252s stream into Sierra program.")?;
    match args.output {
        Some(path) => fs::write(path, sierra_program.to_string())
            .with_context(|| "Failed to write casm contract.")?,
        None => println!("{sierra_program}"),
    }
    Ok(())
}
