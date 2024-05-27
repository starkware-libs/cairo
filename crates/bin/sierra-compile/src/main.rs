use std::fs;
use serde::Serialize;
use serde::ser::Error;

use anyhow::Context;
use cairo_lang_sierra::ProgramParser;
use cairo_lang_sierra::program::Program;
use cairo_lang_sierra_to_casm::compiler::CairoProgram;
use cairo_lang_sierra_to_casm::compiler::SierraToCasmConfig;
use cairo_lang_sierra_to_casm::metadata::calc_metadata;
use cairo_lang_utils::logging::init_logging;
use clap::{Parser, ArgAction};
use indoc::indoc;

pub struct CairoProgramWithSierraContext<'a> {
    pub cairo_program: &'a CairoProgram,
    pub sierra_program: &'a Program,
}

impl<'a> CairoProgramWithSierraContext<'a> {
    pub fn new(
        cairo_program: &'a CairoProgram,
        sierra_program: &'a Program,
    ) -> CairoProgramWithSierraContext<'a> {
        CairoProgramWithSierraContext { cairo_program, sierra_program }
    }
}

impl<'a> Serialize for CairoProgramWithSierraContext<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;
        let mut map = serializer.serialize_map(None)?;
        let main_func = self
            .sierra_program
            .find_function("::main")
            .ok_or_else(|| S::Error::custom("Main function not found"))?;
        let entry_point = main_func.entry_point.0;
        let builtins = vec![
            "pedersen_builtin",
            "range_check_builtin",
            "bitwise_builtin",
            "ec_op_builtin",
            "poseidon_builtin",
        ];
        let assembled_cairo_program = self.cairo_program.assemble();
        map.serialize_entry("bytecode", &assembled_cairo_program.bytecode)?;
        map.serialize_entry("hints", &assembled_cairo_program.hints)?;
        map.serialize_entry("entry_point", &entry_point)?;
        map.serialize_entry("builtins", &builtins)?;
        let debug_info = self.cairo_program.debug_info
            .sierra_statement_info
            .iter()
            .map(|statement_debug_info| {
                (
                    statement_debug_info.start_offset,
                    statement_debug_info.instruction_idx,
                )
            })
            .collect::<Vec<(usize, usize)>>();
        map.serialize_entry("debug_info", &debug_info)?;
        map.serialize_entry("consts_info", &self.cairo_program.consts_info)?;
        map.end()
    }
}

/// Compiles a Sierra file to CASM.
/// Exits with 0/1 if the compilation succeeds/fails.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The path of the file to compile.
    file: String,
    output: String,
    // Export the compiled program as a JSON file.
    #[clap(long, action = ArgAction::SetTrue)]
    export_json: bool,
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

    if args.export_json {
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
    }

    fs::write(args.output, format!("{cairo_program}")).with_context(|| "Failed to write output.")
}
