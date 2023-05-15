//! Compiles and runs a Cairo program.
use std::fs;

use anyhow::{anyhow, Context};
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

    let (sierra_code_opt, collected) =
        collect_tests(
            &args.file,
            None,
            Some(vec![
                (&String::from("/Users/piotrmagiera/SWM/Dev/cairo/scarb_integration/libraries/libraries/external_lib_bar/src"), &String::from("external_lib_bar")),
                (&String::from("/Users/piotrmagiera/SWM/Dev/cairo/scarb_integration/libraries/libraries/external_lib_foo/src"), &String::from("external_lib_foo")),
                (&String::from("/Users/piotrmagiera/SWM/Dev/cairo/scarb_integration/libraries/src"), &String::from("libraries_project"))
            ]),
            None)?;
    let sierra_code = sierra_code_opt.ok_or(anyhow!("Expected sierra code"))?;

    if let Some(out_path) = args.output_sierra {
        fs::write(out_path, format!("{}", sierra_code)).context("Failed to write output.")?;
    }

    if let Some(output_contents) =
        build_protostar_casm_from_sierra(&collected, sierra_code, args.output_casm)?
    {
        println!("{}", output_contents);
    }

    Ok(())
}
