use std::path::Path;
use std::fs;

use pyo3::prelude::*;
use pyo3::wrap_pyfunction;
use pyo3::exceptions::RuntimeError;

use compiler::{
    compile_cairo_project_at_path as compile_cairo_to_sierra_at_path,
    CompilerConfig,
};
use sierra_to_casm::compiler::compile_at_path as compile_sierra_to_casm_at_path;

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
    let cairo_program = compile_sierra_to_casm_at_path(input_path)
        .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{}", e)))?;
    let cairo_program_contents = format!("{}", cairo_program);
    if let Some(path) = output_path {
        fs::write(path, cairo_program_contents).map_err(|_| PyErr::new::<RuntimeError, _>("Failed to write output."))?;
        return Ok(None);
    }
    Ok(Some(cairo_program_contents))
}

#[pymodule]
fn cairo_python_bindings(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_wrapped(wrap_pyfunction!(call_cairo_to_sierra_compiler))?;
    m.add_wrapped(wrap_pyfunction!(call_sierra_to_casm_compiler))?;
    Ok(())
}
