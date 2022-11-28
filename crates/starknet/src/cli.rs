use std::fs;
use std::path::PathBuf;

use anyhow::Context;
use clap::Parser;
use compiler::diagnostics::eprint_diagnostic;
use compiler::{compile, CompileArgs, CompileProjectConfig, CompileResult};
use starknet::abi;
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
    /// Replaces sierra ids with human-readable ones.
    #[arg(short, long, default_value_t = false)]
    replace_ids: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let CompileResult { sierra_program, .. } = compile(CompileArgs {
        compile: CompileProjectConfig::LoadFromPath { path: PathBuf::from(&args.path) },
        on_diagnostic: Some(&mut eprint_diagnostic),
        replace_ids: args.replace_ids,
    })?;

    // TODO(ilya): Get abi and entry points from the code.
    let contract = ContractClass {
        sierra_program: (*sierra_program).clone(),
        entry_points_by_type: ContractEntryPoints::default(),
        abi: abi::Contract::default(),
    };

    let res = serde_json::to_string_pretty(&contract).with_context(|| "Serialization failed.")?;

    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write output.")?,
        None => println!("{}", res),
    }

    Ok(())
}
