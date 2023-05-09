use std::fs;
use std::path::PathBuf;

use anyhow::Context;
use cairo_lang_compiler::CompilerConfig;
use cairo_lang_starknet::allowed_libfuncs::{validate_compatible_sierra_version, ListSelector};
use cairo_lang_starknet::contract_class::compile_path;
use clap::Parser;

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The crate to compile.
    path: PathBuf,
    // The contract fully qualified path.
    #[arg(short, long)]
    contract_path: Option<String>,
    /// The output file name (default: stdout).
    output: Option<String>,
    /// Replaces sierra ids with human-readable ones.
    #[arg(short, long, default_value_t = false)]
    replace_ids: bool,
    /// The allowed libfuncs list to use (default: most recent audited list).
    #[arg(long)]
    allowed_libfuncs_list_name: Option<String>,
    /// A file of the allowed libfuncs list to use.
    #[arg(long)]
    allowed_libfuncs_list_file: Option<String>,
}

/// Compile Starknet crate (or specific contract in the crate).
pub fn starknet_compile(
    crate_path: PathBuf,
    contract_path: Option<String>,
    config: Option<CompilerConfig>,
    allowed_libfuncs_list_name: Option<String>,
    allowed_libfuncs_list_file: Option<String>,
) -> anyhow::Result<String> {
    let list_selector = ListSelector::new(allowed_libfuncs_list_name, allowed_libfuncs_list_file)
        .expect("Both allowed libfunc list name and file were supplied.");
    let contract = compile_path(
        &crate_path,
        contract_path.as_deref(),
        if let Some(config) = config { config } else { CompilerConfig::default() },
    )?;
    validate_compatible_sierra_version(&contract, list_selector)?;
    Ok(serde_json::to_string_pretty(&contract).with_context(|| "Serialization failed.")?)
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let res = starknet_compile(
        args.path,
        args.contract_path,
        Some(CompilerConfig { replace_ids: args.replace_ids, ..CompilerConfig::default() }),
        args.allowed_libfuncs_list_name,
        args.allowed_libfuncs_list_file,
    )?;
    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write output.")?,
        None => println!("{res}"),
    }

    Ok(())
}
