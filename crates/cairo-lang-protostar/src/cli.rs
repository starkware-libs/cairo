//! Compiles and runs a Cairo program.
use std::fs;

use cairo_lang_protostar::build_protostar_casm_from_sierra;
use clap::Parser;

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
    let sierra_code = fs::read_to_string(args.file).expect("Could not read file!");
    if let Some(output_contents) = build_protostar_casm_from_sierra(None, sierra_code, args.output)?
    {
        println!("{}", output_contents);
    }

    Ok(())
}
