use std::fs;

use anyhow::Context;
use cairo_lang_starknet_classes::allowed_libfuncs::ListSelector;
use cairo_lang_starknet_classes::casm_contract_class::CasmContractClass;
use cairo_lang_starknet_classes::contract_class::{ContractClass, ContractEntryPoints};
use cairo_lang_utils::bigint::BigUintAsHex;
use clap::Parser;
use serde::Deserialize;

/// Compiles a Sierra contract class into CASM contract class.
/// Exits with 0/1 if the compilation succeeds/fails.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The path of the file to compile.
    file: String,
    /// The output file name (default: stdout).
    output: Option<String>,
    /// The allowed libfuncs list to use (default: most recent audited list).
    #[arg(long)]
    allowed_libfuncs_list_name: Option<String>,
    /// A file of the allowed libfuncs list to use.
    #[arg(long)]
    allowed_libfuncs_list_file: Option<String>,
    /// Add pythonic hints.
    #[arg(long, default_value_t = false)]
    add_pythonic_hints: bool,
    /// The max bytecode size.
    #[arg(long, default_value_t = 180000)]
    max_bytecode_size: usize,
}

/// Same as `ContractClass` - but ignores `abi` in deserialization.
/// Enables loading old contract classes.
#[derive(Deserialize)]
pub struct ContractClassIgnoreAbi {
    pub sierra_program: Vec<BigUintAsHex>,
    pub sierra_program_debug_info: Option<cairo_lang_sierra::debug_info::DebugInfo>,
    pub contract_class_version: String,
    pub entry_points_by_type: ContractEntryPoints,
    pub _abi: Option<serde_json::Value>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let list_selector =
        ListSelector::new(args.allowed_libfuncs_list_name, args.allowed_libfuncs_list_file)
            .expect("Both allowed libfunc list name and file were supplied.");
    let ContractClassIgnoreAbi {
        sierra_program,
        sierra_program_debug_info,
        contract_class_version,
        entry_points_by_type,
        _abi,
    } = serde_json::from_str(
        &fs::read_to_string(&args.file)
            .with_context(|| format!("Failed to read {}.", &args.file))?,
    )
    .with_context(|| "deserialization Failed.")?;
    let contract_class = ContractClass {
        sierra_program,
        sierra_program_debug_info,
        contract_class_version,
        entry_points_by_type,
        abi: None,
    };
    contract_class.validate_version_compatible(list_selector)?;
    let casm_contract = CasmContractClass::from_contract_class(
        contract_class,
        args.add_pythonic_hints,
        args.max_bytecode_size,
    )
    .with_context(|| "Compilation failed.")?;

    let res = serde_json::to_string_pretty(&casm_contract)
        .with_context(|| "Casm contract Serialization failed.")?;

    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write casm contract.")?,
        None => println!("{res}"),
    }
    Ok(())
}
