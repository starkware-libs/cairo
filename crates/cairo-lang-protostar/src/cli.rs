//! Compiles and runs a Cairo program.
use std::fs;

use anyhow::Context;
use cairo_lang_protostar::build_protostar_casm_from_sierra;
use cairo_lang_protostar::test_collector::collect_tests;
use clap::Parser;

#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile
    file: String,
    /// The output file name (default: stdout).
    output_sierra: Option<String>,
    output_casm: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let (sierra_code, collected) = collect_tests(
        &args.file,
        None,
        None,
        None
    )?;

    if let Some(out_path) = args.output_sierra {
        fs::write(out_path, format!("{}", sierra_code.as_ref().unwrap())).context("Failed to write output.")?;
    }


    
    if let Some(output_contents) = build_protostar_casm_from_sierra(&collected, sierra_code.unwrap(), args.output_casm)?
    {
        println!("{}", output_contents);
    }

    Ok(())
}
