use std::fs;
use std::path::PathBuf;

use anyhow::Context;
use clap::Parser;
use starknet::contract_class::compile_path;

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile
    path: String,
    /// The output file name (default: stdout).
    output: Option<String>,
    /// Replaces sierra ids with human readable ones.
    #[arg(short, long, default_value_t = false)]
    replace_ids: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let contract = compile_path(&PathBuf::from(args.path), args.replace_ids)?;
    let res = serde_json::to_string_pretty(&contract).with_context(|| "Serialization failed.")?;
    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write output.")?,
        None => println!("{}", res),
    }

    Ok(())
}
