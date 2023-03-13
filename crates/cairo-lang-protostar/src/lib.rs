use std::fs;

use anyhow::Context;
use cairo_lang_sierra::program::Program;
use cairo_lang_sierra::ProgramParser;
use casm_generator::SierraCasmGenerator;

pub mod casm_generator;

pub fn build_protostar_casm(
    named_tests: Option<Vec<String>>,
    contents: &str,
) -> anyhow::Result<String> {
    let program: Program = ProgramParser::new().parse(&contents).unwrap();
    let casm_generator = match SierraCasmGenerator::new(program, false) {
        Ok(casm_generator) => casm_generator,
        Err(e) => panic!("{}", e),
    };
    let protostar_casm = casm_generator.build_casm(named_tests)?;
    let res = serde_json::to_string_pretty(&protostar_casm).context("Serialization failed.")?;
    Ok(res)
}

pub fn build_protostar_casm_from_sierra(
    named_tests: Option<Vec<String>>,
    sierra_code: String,
    maybe_output_path: Option<String>,
) -> anyhow::Result<Option<String>> {
    let casm_contents = build_protostar_casm(named_tests, &sierra_code[..])?;

    if let Some(output_path) = maybe_output_path {
        fs::write(output_path, casm_contents).with_context(|| "Failed to write output.")?;
        return Ok(None);
    }
    Ok(Some(casm_contents))
}
