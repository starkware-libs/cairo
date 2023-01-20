use std::path::Path;
use std::fs;

use pyo3::prelude::*;
use pyo3::wrap_pyfunction;
use pyo3::exceptions::RuntimeError;

use cairo_lang_compiler::{
    compile_cairo_project_at_path as compile_cairo_to_sierra_at_path,
    CompilerConfig,
};
use cairo_lang_sierra_to_casm::compiler::{
    compile_at_path as compile_sierra_to_casm_at_path,
    compile_contents as compile_sierra_to_casm_for_contents
};

#[pyfunction]
fn call_cairo_to_sierra_compiler(input_path: &str, output_path: Option<&str>) -> PyResult<Option<String>> {
    let sierra_program = compile_cairo_to_sierra_at_path(Path::new(input_path), CompilerConfig::default())
        .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{}", e)))?;
    let sierra_program_contents = format!("{}", sierra_program);
    if let Some(path) = output_path {
        fs::write(path, sierra_program_contents).map_err(|_| PyErr::new::<RuntimeError, _>("Failed to write output."))?;
        return Ok(None);
    }
    Ok(Some(sierra_program_contents))
}

#[pyfunction]
fn call_sierra_to_casm_compiler(input_path: &str, output_path: Option<&str>) -> PyResult<Option<String>> {
    let casm_program = compile_sierra_to_casm_at_path(input_path)
        .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{}", e)))?;
    let casm_program_contents = format!("{}", casm_program);
    if let Some(path) = output_path {
        fs::write(path, casm_program_contents).map_err(|_| PyErr::new::<RuntimeError, _>("Failed to write output."))?;
        return Ok(None);
    }
    Ok(Some(casm_program_contents))
}

#[pyfunction]
fn call_cairo_to_casm_compiler(input_path: &str, output_path: Option<&str>) -> PyResult<Option<String>> {
    let sierra_program = call_cairo_to_sierra_compiler(input_path, None)?.unwrap();
    let casm_program = compile_sierra_to_casm_for_contents(&sierra_program.to_string())
        .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{}", e)))?;

    let casm_program_contents = format!("{}", casm_program);
    if let Some(path) = output_path {
        fs::write(path, casm_program_contents).map_err(|_| PyErr::new::<RuntimeError, _>("Failed to write output."))?;
        return Ok(None);
    }
    Ok(Some(casm_program_contents))
}

#[pymodule]
fn cairo_python_bindings(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_wrapped(wrap_pyfunction!(call_cairo_to_sierra_compiler))?;
    m.add_wrapped(wrap_pyfunction!(call_sierra_to_casm_compiler))?;
    m.add_wrapped(wrap_pyfunction!(call_cairo_to_casm_compiler))?;
    Ok(())
}