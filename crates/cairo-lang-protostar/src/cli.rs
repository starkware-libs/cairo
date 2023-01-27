//! Compiles and runs a Cairo program.
use clap::Parser;

use cairo_lang_protostar::build_protostar_casm_from_file;

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
    if let Some(output_contents) = build_protostar_casm_from_file(None, args.file, args.output)? {
        println!("{}", output_contents);
    }

    Ok(())
}