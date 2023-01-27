use anyhow::Context;
use cairo_lang_sierra::ProgramParser;
use cairo_lang_sierra::program::Program;
use casm_generator::SierraCasmGenerator;
use std::fs;

mod casm_generator;

pub fn build_protostar_casm(named_tests: Option<Vec<String>>, contents: &str) -> anyhow::Result<String> {
  let program: Program = ProgramParser::new().parse(&contents).unwrap();
  let casm_generator = match SierraCasmGenerator::new(program, false) {
      Ok(casm_generator) => casm_generator,
      Err(e) => panic!("{}", e)
  };
  let protostar_casm = casm_generator.build_casm(named_tests).context("Failed to build CASM")?;
  let res =
      serde_json::to_string_pretty(&protostar_casm).context("Serialization failed.")?;
  Ok(res)
}

pub fn build_protostar_casm_from_file(named_tests: Option<Vec<String>>, input_path: String, maybe_output_path: Option<String>) -> anyhow::Result<Option<String>> {
  let sierra_code = fs::read_to_string(input_path).expect("Could not read file!");
  let casm_contents = build_protostar_casm(named_tests, &sierra_code[..])?;

  if let Some(output_path) = maybe_output_path {
    fs::write(output_path, casm_contents).with_context(|| "Failed to write output.")?;
    return Ok(None);
  }
  Ok(Some(casm_contents))
}