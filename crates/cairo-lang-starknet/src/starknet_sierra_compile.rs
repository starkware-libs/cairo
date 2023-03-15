use std::fs;

use anyhow::Context;
use cairo_lang_starknet::allowed_libfuncs::{validate_compatible_sierra_version, ListSelector};
use cairo_lang_starknet::casm_contract_class::CasmContractClass;
use cairo_lang_starknet::contract_class::ContractClass;
use clap::Parser;
use itertools::Itertools;
use serde::{Deserialize, Serialize};

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile
    file: String,
    /// The output file name (default: stdout).
    output: Option<String>,
    /// The allowed libfuncs list to use (default: most recent audited list).
    #[arg(long)]
    allowed_libfuncs_list_name: Option<String>,
    /// A file of the allowed libfuncs list to use.
    #[arg(long)]
    allowed_libfuncs_list_file: Option<String>,

    #[arg(long)]
    pythonic_hints_file: Option<String>,
}

// Hints in a format that can be executed by the python vm.
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct PythonicHints {
    pub pythonic_hints: Vec<(usize, Vec<String>)>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let list_selector =
        ListSelector::new(args.allowed_libfuncs_list_name, args.allowed_libfuncs_list_file)
            .expect("Both allowed libfunc list name and file were supplied.");
    let contract_class: ContractClass = serde_json::from_str(
        &fs::read_to_string(&args.file)
            .with_context(|| format!("Failed to read {}.", &args.file))?,
    )
    .with_context(|| "deserialization Failed.")?;
    validate_compatible_sierra_version(&contract_class, list_selector)?;
    let casm_contract = CasmContractClass::from_contract_class(contract_class)
        .with_context(|| "Compilation failed.")?;

    let res = serde_json::to_string_pretty(&casm_contract)
        .with_context(|| "Casm contract Serialization failed.")?;

    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write casm contract.")?,
        None => println!("{res}"),
    }

    if let Some(file) = args.pythonic_hints_file {
        let pythonic_hints = PythonicHints {
            pythonic_hints: casm_contract
                .hints
                .iter()
                .map(|(pc, hints)| (*pc, hints.iter().map(|hint| hint.to_string()).collect_vec()))
                .collect_vec(),
        };

        fs::write(
            file,
            serde_json::to_string_pretty(&pythonic_hints)
                .with_context(|| "Hints serialization failed.")?,
        )
        .with_context(|| "Failed to write pythonic hints.")?;
    }

    Ok(())
}
