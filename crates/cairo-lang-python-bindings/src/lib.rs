use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::Context;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_compiler::CompilerConfig;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_plugins::config::ConfigPlugin;
use cairo_lang_plugins::derive::DerivePlugin;
use cairo_lang_plugins::panicable::PanicablePlugin;
use cairo_lang_protostar::build_protostar_casm_from_sierra;
use cairo_lang_semantic::plugin::SemanticPlugin;
use cairo_lang_sierra::extensions::enm::EnumType;
use cairo_lang_sierra::extensions::NamedType;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_starknet::casm_contract_class::CasmContractClass;
use cairo_lang_starknet::contract_class::{compile_path as compile_starknet, ContractClass};
use pyo3::exceptions::RuntimeError;
use pyo3::prelude::*;
use pyo3::wrap_pyfunction;

mod find_tests;

use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_protostar::casm_generator::SierraCasmGenerator;
use cairo_lang_semantic::items::functions::{ConcreteFunctionWithBodyId, GenericFunctionId};
use cairo_lang_semantic::{ConcreteFunction, FunctionLongId};
use cairo_lang_sierra::program::{GenericArg, Program};
use find_tests::find_all_tests;
use itertools::Itertools;

#[pyfunction]
fn compile_starknet_contract_from_path(
    input_path: &str,
    output_path: Option<&str>,
    maybe_cairo_paths: Option<Vec<&str>>,
) -> PyResult<Option<String>> {
    let casm = starknet_cairo_to_casm(input_path, maybe_cairo_paths)
        .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{}", e)))?;

    if let Some(path) = output_path {
        fs::write(path, casm).map_err(|e| {
            PyErr::new::<RuntimeError, _>(format!("Failed to write output: {}", e.to_string()))
        })?;
        return Ok(None);
    }
    Ok(Some(casm))
}

fn starknet_cairo_to_casm(
    input_path: &str,
    maybe_cairo_paths: Option<Vec<&str>>,
) -> Result<String, anyhow::Error> {
    let contract = compile_starknet(
        &PathBuf::from(input_path),
        CompilerConfig { replace_ids: true, ..CompilerConfig::default() },
        maybe_cairo_paths,
    )?;
    let sierra =
        serde_json::to_string_pretty(&contract).with_context(|| "Serialization failed.")?;

    let contract_class: ContractClass =
        serde_json::from_str(&sierra[..]).with_context(|| "deserialization Failed.")?;

    let casm_contract = CasmContractClass::from_contract_class(contract_class)
        .with_context(|| "Compilation failed.")?;

    let casm =
        serde_json::to_string_pretty(&casm_contract).with_context(|| "Serialization failed.")?;

    Ok(casm)
}

// returns tuple[sierra if no output_path, list[test_names]]
#[pyfunction]
fn collect_tests(
    input_path: &str,
    output_path: Option<&str>,
    maybe_cairo_paths: Option<Vec<&str>>,
) -> PyResult<(Option<String>, Vec<String>)> {
    // code taken from crates/cairo-lang-test-runner/src/cli.rs
    let plugins: Vec<Arc<dyn SemanticPlugin>> = vec![
        Arc::new(DerivePlugin {}),
        Arc::new(PanicablePlugin {}),
        Arc::new(ConfigPlugin { configs: HashSet::from(["test".to_string()]) }),
    ];
    let db = &mut RootDatabase::builder().with_plugins(plugins).detect_corelib().build().map_err(
        |e| PyErr::new::<RuntimeError, _>(format!("Failed to build database: {}", e.to_string())),
    )?;

    let main_crate_ids = setup_project(db, Path::new(&input_path)).map_err(|e| {
        PyErr::new::<RuntimeError, _>(format!(
            "Failed to setup project for path({}): {}",
            input_path,
            e.to_string()
        ))
    })?;
    if let Some(cairo_paths) = maybe_cairo_paths {
        for cairo_path in cairo_paths {
            setup_project(db, Path::new(cairo_path)).map_err(|e| {
                PyErr::new::<RuntimeError, _>(format!(
                    "Failed to add cairo path({}): {}",
                    cairo_path,
                    e.to_string()
                ))
            })?;
        }
    }

    if DiagnosticsReporter::stderr().check(db) {
        return Err(PyErr::new::<RuntimeError, _>(
            "Failed to add cairo path, for a detailed information, please go through the logs \
             above",
        ));
    }
    let all_tests = find_all_tests(db, main_crate_ids);

    let sierra_program = db
        .get_sierra_program_for_functions(
            all_tests
                .iter()
                .flat_map(|t| ConcreteFunctionWithBodyId::from_no_generics_free(db, t.func_id))
                .collect(),
        )
        .to_option()
        .with_context(|| "Compilation failed without any diagnostics")
        .map_err(|e| {
            PyErr::new::<RuntimeError, _>(format!(
                "Failed to get sierra program: {}",
                e.to_string()
            ))
        })?;

    let named_tests = all_tests
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

    let sierra_program = replace_sierra_ids_in_program(db, &sierra_program);

    validate_tests(sierra_program.clone(), &named_tests).map_err(|e| {
        PyErr::new::<RuntimeError, _>(format!("Test validation failed: {}", e.to_string()))
    })?;

    let mut result_contents = None;
    if let Some(path) = output_path {
        fs::write(path, &sierra_program.to_string()).map_err(|e| {
            PyErr::new::<RuntimeError, _>(format!("Failed to write output: {}", e.to_string()))
        })?;
    } else {
        result_contents = Some(sierra_program.to_string());
    }
    Ok((result_contents, named_tests))
}

fn validate_tests(sierra_program: Program, test_names: &Vec<String>) -> Result<(), anyhow::Error> {
    let casm_generator = match SierraCasmGenerator::new(sierra_program, false) {
        Ok(casm_generator) => casm_generator,
        Err(e) => panic!("{}", e),
    };
    for test in test_names {
        let func = casm_generator.find_function(test)?;
        if func.params.len() > 0 {
            anyhow::bail!(format!(
                "Invalid number of parameters for test {}: expected 0, got {}",
                test,
                func.params.len()
            ));
        }
        let signature = &func.signature;
        let tp = &signature.ret_types[0];
        let info = casm_generator.get_info(&tp);
        let mut maybe_return_type_name = None;
        if info.long_id.generic_id == EnumType::ID {
            if let GenericArg::UserType(ut) = &info.long_id.generic_args[0] {
                maybe_return_type_name = Some(ut.debug_name.as_ref().unwrap().as_str());
            }
        }
        if let Some(return_type_name) = maybe_return_type_name {
            if !return_type_name.starts_with("core::PanicResult::") {
                anyhow::bail!("Test function {} must be panicable but it's not", test);
            }
            if return_type_name != "core::PanicResult::<((),)>" {
                anyhow::bail!(
                    "Test function {} returns a value {}, it is required that test functions do \
                     not return values",
                    test,
                    return_type_name
                );
            }
        } else {
            anyhow::bail!(
                "Couldn't read result type for test function {} possible cause: Test function {} \
                 must be panicable but it's not",
                test,
                test
            );
        }
    }

    Ok(())
}

#[pyfunction]
fn compile_protostar_sierra_to_casm(
    named_tests: Vec<String>,
    input_data: String,
    output_path: Option<&str>,
) -> PyResult<Option<String>> {
    let casm = build_protostar_casm_from_sierra(
        Some(named_tests),
        input_data,
        output_path.map(|s| s.to_string()),
    )
    .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{}", e)))?;

    Ok(casm)
}

#[pyfunction]
fn compile_protostar_sierra_to_casm_from_path(
    named_tests: Vec<String>,
    input_path: &str,
    output_path: Option<&str>,
) -> PyResult<Option<String>> {
    let input_data = fs::read_to_string(input_path).expect("Could not read file!");
    let casm = build_protostar_casm_from_sierra(
        Some(named_tests),
        input_data,
        output_path.map(|s| s.to_string()),
    )
    .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{}", e)))?;

    Ok(casm)
}

#[pymodule]
fn cairo_python_bindings(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_wrapped(wrap_pyfunction!(compile_starknet_contract_from_path))?;
    m.add_wrapped(wrap_pyfunction!(collect_tests))?;
    m.add_wrapped(wrap_pyfunction!(compile_protostar_sierra_to_casm))?;
    m.add_wrapped(wrap_pyfunction!(compile_protostar_sierra_to_casm_from_path))?;
    Ok(())
}
