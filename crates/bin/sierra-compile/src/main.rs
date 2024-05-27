use std::fs;

use anyhow::Context;
use cairo_lang_sierra::ProgramParser;
use cairo_lang_sierra_to_casm::compiler::CairoProgramWithSierraContext;
use cairo_lang_sierra_to_casm::compiler::SierraToCasmConfig;
use cairo_lang_sierra_to_casm::metadata::calc_metadata;
use cairo_lang_utils::logging::init_logging;
use clap::Parser;
use indoc::indoc;

/// Compiles a Sierra file to CASM.
/// Exits with 0/1 if the compilation succeeds/fails.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The path of the file to compile.
    file: String,
    output: String,
}

fn main() -> anyhow::Result<()> {
    init_logging(log::LevelFilter::Off);
    log::info!("Starting Sierra compilation.");

    let args = Args::parse();

    let sierra_code = fs::read_to_string(args.file).with_context(|| "Could not read file!")?;
    let Ok(program) = ProgramParser::new().parse(&sierra_code) else {
        anyhow::bail!(indoc! {"
            Failed to parse sierra program.
            Note: StarkNet contracts should be compiled with `starknet-sierra-compile`."
        })
    };

    let cairo_program = cairo_lang_sierra_to_casm::compiler::compile(
        &program,
        &calc_metadata(&program, Default::default())
            .with_context(|| "Failed calculating Sierra variables.")?,
        SierraToCasmConfig { gas_usage_check: true, max_bytecode_size: usize::MAX },
    )
    .with_context(|| "Compilation failed.")?;

    let cairo_program_with_sierra_context =
        CairoProgramWithSierraContext::new(&cairo_program, &program);
    let serialized_casm = serde_json::to_string_pretty(&cairo_program_with_sierra_context);
    match serialized_casm {
        Ok(casm_json) => {
            let casm_json_path = args.output.clone() + ".json";
            fs::write(&casm_json_path, &casm_json)
                .with_context(|| format!("Failed to write output to {}", casm_json_path))?;
        }
        Err(e) => {
            anyhow::bail!("Failed to serialize CairoProgram: {}", e);
        }
    }

    fs::write(args.output, format!("{cairo_program}")).with_context(|| "Failed to write output.")
}
