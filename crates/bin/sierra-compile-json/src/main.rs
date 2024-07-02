use std::fs;

use anyhow::Context;
use cairo_lang_sierra::ProgramParser;
use cairo_lang_sierra_to_casm::{
    compiler::{compile, CasmCairoProgram, SierraToCasmConfig},
    metadata::calc_metadata,
};
use clap::Parser;

/// Compiles a Sierra file (Cairo Program) into serialized CASM.
/// Exits with 0/1 if the compilation succeeds/fails.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The path of the file to compile.
    file: String,
    /// The output file name (default: stdout).
    output: Option<String>,
    /// Add pythonic hints.
    #[arg(long, default_value_t = false)]
    add_pythonic_hints: bool,
    /// Add gas usage check
    #[arg(long, default_value_t = false)]
    gas_usage_check: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let sierra_code = fs::read_to_string(args.file).with_context(|| "Could not read file!")?;
    let Ok(program) = ProgramParser::new().parse(&sierra_code) else {
        anyhow::bail!("Failed to parse Sierra program.")
    };

    let sierra_to_casm_config =
        SierraToCasmConfig { gas_usage_check: args.gas_usage_check, max_bytecode_size: usize::MAX };

    let cairo_program = compile(
        &program,
        &calc_metadata(&program, Default::default())
            .with_context(|| "Failed calculating Sierra variables.")?,
        sierra_to_casm_config,
    )
    .with_context(|| "Compilation failed.")?;

    let main_func = program.find_function("::main")?;

    let casm_cairo_program = CasmCairoProgram::new(&cairo_program, main_func)
        .with_context(|| "Sierra to Casm compilation failed.")?;

    let res = serde_json::to_string_pretty(&casm_cairo_program)
        .with_context(|| "Casm contract Serialization failed.")?;

    match args.output {
        Some(path) => fs::write(path, res).with_context(|| "Failed to write casm contract.")?,
        None => println!("{res}"),
    }
    Ok(())
}
