use std::path::Path;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use std::collections::HashSet;

use pyo3::prelude::*;
use pyo3::wrap_pyfunction;
use pyo3::exceptions::RuntimeError;
use anyhow::Context;

use cairo_lang_compiler::{
    db::RootDatabase,
    compile_cairo_project_at_path as compile_cairo_to_sierra_at_path,
    CompilerConfig,
};
use cairo_lang_sierra_to_casm::compiler::{
    compile_at_path as compile_sierra_to_casm_at_path,
    compile_contents as compile_sierra_to_casm_for_contents
};
use cairo_lang_starknet::contract_class::{
    ContractClass,
    compile_path as compile_starknet
};
use cairo_lang_starknet::casm_contract_class::CasmContractClass;
use cairo_lang_semantic::plugin::SemanticPlugin;
use cairo_lang_plugins::derive::DerivePlugin;
use cairo_lang_plugins::panicable::PanicablePlugin;
use cairo_lang_plugins::config::ConfigPlugin;
use cairo_lang_compiler::diagnostics::check_and_eprint_diagnostics;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_diagnostics::ToOption;

mod find_tests;

use find_tests::find_all_tests;
use cairo_lang_protostar::build_protostar_casm_from_file;
use cairo_lang_semantic::{ConcreteFunction, FunctionLongId};
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_debug::debug::DebugWithDb;
use itertools::Itertools;

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

#[pyfunction]
fn call_starknet_contract_compiler(input_path: &str, output_path: Option<&str>) -> PyResult<Option<String>> {
    let casm = starknet_cairo_to_casm(input_path)
        .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{}", e)))?;

    if let Some(path) = output_path {
        fs::write(path, casm).map_err(|_| PyErr::new::<RuntimeError, _>("Failed to write output."))?;
        return Ok(None);
    }
    Ok(Some(casm))
}

fn starknet_cairo_to_casm(input_path: &str) -> Result<String, anyhow::Error> {
    let contract = compile_starknet(&PathBuf::from(input_path), true)?;
    let sierra = serde_json::to_string_pretty(&contract).with_context(|| "Serialization failed.")?;

    let contract_class: ContractClass = serde_json::from_str(&sierra[..])
        .with_context(|| "deserialization Failed.")?;

    let casm_contract = CasmContractClass::from_contract_class(contract_class)
        .with_context(|| "Compilation failed.")?;

    let casm = serde_json::to_string_pretty(&casm_contract)
            .with_context(|| "Serialization failed.")?;

    Ok(casm)
}

#[pyfunction]
fn call_test_collector(path: &str, output_path: Option<&str>) -> PyResult<(Option<String>, Vec<String>)> {
    let plugins: Vec<Arc<dyn SemanticPlugin>> = vec![
        Arc::new(DerivePlugin {}),
        Arc::new(PanicablePlugin {}),
        Arc::new(ConfigPlugin { configs: HashSet::from(["test".to_string()]) }),
    ];
    let mut db_val = RootDatabase::new(plugins);
    let db = &mut db_val;

    let main_crate_ids = setup_project(db, Path::new(&path)).map_err(|_| PyErr::new::<RuntimeError, _>("Failed to write output."))?;

    if check_and_eprint_diagnostics(db) {
        return Err(PyErr::new::<RuntimeError, _>(format!("failed to compile: {}", path)));
    }
    let all_tests = find_all_tests(db, main_crate_ids);
    let sierra_program = db
        .get_sierra_program_for_functions(
            all_tests.iter().map(|t| FunctionWithBodyId::Free(t.func_id)).collect(),
        )
        .to_option()
        .with_context(|| "Compilation failed without any diagnostics.").map_err(|_| PyErr::new::<RuntimeError, _>("Failed to write output."))?;
    let sierra_program = replace_sierra_ids_in_program(db, &sierra_program);

    let named_tests: Vec<String> = all_tests
        .into_iter()
        .map(|test| {
            (
                format!(
                    "{:?}",
                    FunctionLongId {
                        function: ConcreteFunction {
                            generic_function: GenericFunctionId::Free(test.func_id),
                            generic_args: vec![]
                        }
                    }
                    .debug(db)
                ),
                test,
            )
        })
        .collect_vec()
        .into_iter()
        .map(|(test_name, _test_config)| test_name)
        .collect();

    let mut result_contents = None;
    if let Some(path) = output_path {
        fs::write(path, &sierra_program.to_string()).map_err(|_| PyErr::new::<RuntimeError, _>("Failed to write output."))?;
    } else {
        result_contents = Some(sierra_program.to_string());
    }
    Ok((result_contents, named_tests))
}

#[pyfunction]
fn call_protostar_sierra_to_casm(named_tests: Vec<String>, input_path: &str, output_path: Option<&str>) -> PyResult<Option<String>> {
    let casm = build_protostar_casm_from_file(Some(named_tests), input_path.to_string(), output_path.map(|s| s.to_string()))
        .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{}", e)))?;

    Ok(casm)
}

#[pymodule]
fn cairo_python_bindings(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_wrapped(wrap_pyfunction!(call_cairo_to_sierra_compiler))?;
    m.add_wrapped(wrap_pyfunction!(call_sierra_to_casm_compiler))?;
    m.add_wrapped(wrap_pyfunction!(call_cairo_to_casm_compiler))?;
    m.add_wrapped(wrap_pyfunction!(call_starknet_contract_compiler))?;
    m.add_wrapped(wrap_pyfunction!(call_test_collector))?;
    m.add_wrapped(wrap_pyfunction!(call_protostar_sierra_to_casm))?;
    Ok(())
}