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
use cairo_lang_starknet::plugin::StarkNetPlugin;
use cairo_lang_semantic::plugin::SemanticPlugin;
use cairo_lang_plugins::derive::DerivePlugin;
use cairo_lang_plugins::panicable::PanicablePlugin;
use cairo_lang_plugins::config::ConfigPlugin;
use cairo_lang_compiler::diagnostics::check_and_eprint_diagnostics;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_defs::ids::{FreeFunctionId, FunctionWithBodyId, ModuleItemId};
use cairo_lang_diagnostics::ToOption;
use cairo_lang_syntax::node::ast::Expr;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_syntax::node::Token;

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
fn call_test_collector(path: &str) -> PyResult<String> {
    let plugins: Vec<Arc<dyn SemanticPlugin>> = vec![
        Arc::new(DerivePlugin {}),
        Arc::new(PanicablePlugin {}),
        Arc::new(ConfigPlugin { configs: HashSet::from(["test".to_string()]) }),
        Arc::new(StarkNetPlugin {}), // should we use it? it is used conditionally depending on args.starknet
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
    Ok(sierra_program.to_string())
}


/// Expectation for a result of a test.
enum TestExpectation {
    /// Running the test should not panic.
    Success,
    /// Running the test should result in a panic.
    Panics,
}

/// The configuration for running a single test.
#[allow(dead_code)]
struct TestConfig {
    /// The function id of the test function.
    func_id: FreeFunctionId,
    /// The amount of gas the test requested.
    available_gas: Option<usize>,
    /// The expected result of the run.
    expectation: TestExpectation,
    /// Should the test be ignored.
    ignored: bool,
}

/// Finds the tests in the requested crates.
fn find_all_tests(db: &dyn SemanticGroup, main_crates: Vec<CrateId>) -> Vec<TestConfig> {
    let mut tests = vec![];
    for crate_id in main_crates {
        let modules = db.crate_modules(crate_id);
        for module_id in modules.iter() {
            let Ok(module_items) = db.module_items(*module_id) else {
                continue;
            };

            for item in module_items.iter() {
                if let ModuleItemId::FreeFunction(func_id) = item {
                    if let Ok(attrs) =
                        db.function_with_body_attributes(FunctionWithBodyId::Free(*func_id))
                    {
                        let mut is_test = false;
                        let mut available_gas = None;
                        let mut ignored = false;
                        let mut should_panic = false;
                        for attr in attrs {
                            match attr.id.as_str() {
                                "test" => {
                                    is_test = true;
                                }
                                "available_gas" => {
                                    // TODO(orizi): Provide diagnostics when this does not match.
                                    if let [Expr::Literal(literal)] = &attr.args[..] {
                                        available_gas = literal
                                            .token(db.upcast())
                                            .text(db.upcast())
                                            .parse::<usize>()
                                            .ok();
                                    }
                                }
                                "should_panic" => {
                                    should_panic = true;
                                }
                                "ignore" => {
                                    ignored = true;
                                }
                                _ => {}
                            }
                        }
                        if is_test {
                            tests.push(TestConfig {
                                func_id: *func_id,
                                available_gas,
                                expectation: if should_panic {
                                    TestExpectation::Panics
                                } else {
                                    TestExpectation::Success
                                },
                                ignored,
                            })
                        }
                    }
                }
            }
        }
    }
    tests
}



#[pymodule]
fn cairo_python_bindings(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_wrapped(wrap_pyfunction!(call_cairo_to_sierra_compiler))?;
    m.add_wrapped(wrap_pyfunction!(call_sierra_to_casm_compiler))?;
    m.add_wrapped(wrap_pyfunction!(call_cairo_to_casm_compiler))?;
    m.add_wrapped(wrap_pyfunction!(call_starknet_contract_compiler))?;
    m.add_wrapped(wrap_pyfunction!(call_test_collector))?;
    Ok(())
}