//! Compiles and runs a Cairo program.

use anyhow::{Context};
use cairo_lang_sierra::ProgramParser;
use cairo_lang_sierra::program::Program;
use casm_generator::SierraCasmGenerator;
use std::fs;

use clap::Parser;

// use casm_generator::SierraCasmGenerator;
mod casm_generator;

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
    let program: Program = ProgramParser::new().parse(&sierra_code).unwrap();
    let casm_generator = match SierraCasmGenerator::new(program, false) {
        Ok(casm_generator) => casm_generator,
        Err(e) => panic!("{}", e)
    };
    let protostar_casm = match casm_generator.build_casm() {
       Ok(pc) => pc, 
       Err(e) => panic!("{}", e)
    };
    let res =
        serde_json::to_string_pretty(&protostar_casm).with_context(|| "Serialization failed.")?;

    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write output.")?,
        None => println!("{}", res),
    }
    Ok(())
}