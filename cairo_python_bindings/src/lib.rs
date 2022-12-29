use std::path::Path;
use std::fs;

use pyo3::prelude::*;
use pyo3::wrap_pyfunction;
use pyo3::exceptions::RuntimeError;

use compiler::{
    compile_cairo_project_at_path as compile_cairo_to_sierra_at_path,
    CompilerConfig,
};
use sierra_to_casm::compiler::{
    compile_at_path as compile_sierra_to_casm_at_path,
    Args
};

#[pyfunction]
fn multiply(a: isize, b: isize) -> PyResult<isize> {
    println!("RUST IS multiplying! WAT?!");
    Ok(a * b)
}

#[pyfunction]
fn call_cairo_to_sierra_compiler(input_path: &str, output_path: &str) -> PyResult<()> {
    println!("RUST IS calling compiler cairo => sierra for path: {input_path}/{output_path}");
    // match compile_cairo_project_at_path(Path::new(path), CompilerConfig::default()) {
    //     Ok(_) => (),
    //     Err(e) => return Err(PyErr::new::<RuntimeError, _>(format!("{e}"))),
    // }
    let sierra_program = compile_cairo_to_sierra_at_path(Path::new(input_path), CompilerConfig::default())
        .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{}", e)))?;
    fs::write(output_path, format!("{}", sierra_program)).expect("Failed to write output.");
    // TODO write to output
    Ok(())
}

#[pyfunction]
fn call_sierra_to_casm_compiler(input_path: &str, output_path: &str) -> PyResult<()> {
    println!("RUST IS calling compiler sierra => casm for path: {input_path}/{output_path}");
    compile_sierra_to_casm_at_path(Args{file: input_path.to_owned(), output: output_path.to_owned()})
        .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{}", e)))?;
    Ok(())
}

#[pymodule]
fn cairo_python_bindings(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_wrapped(wrap_pyfunction!(multiply))?;
    m.add_wrapped(wrap_pyfunction!(call_cairo_to_sierra_compiler))?;
    m.add_wrapped(wrap_pyfunction!(call_sierra_to_casm_compiler))?;
    Ok(())
}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
