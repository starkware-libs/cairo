use std::fs;
use std::path::PathBuf;

use anyhow::Context;
use cairo_lang_compiler::CompilerConfig;
use cairo_lang_protostar::build_protostar_casm_from_sierra;
use cairo_lang_protostar::casm_generator::TestConfig;
use cairo_lang_protostar::test_collector::collect_tests as internal_collect_tests;
use cairo_lang_starknet::casm_contract_class::CasmContractClass;
use cairo_lang_starknet::contract_class::{compile_path as compile_starknet, ContractClass};
use pyo3::exceptions::RuntimeError;
use pyo3::prelude::*;
use pyo3::wrap_pyfunction;

type CollectedTest = (String, Option<usize>);

#[pyfunction]
fn compile_starknet_contract_to_sierra_from_path(
    input_path: &str,
    output_path: Option<&str>,
    maybe_cairo_paths: Option<Vec<&str>>,
) -> PyResult<Option<String>> {
    let sierra = starknet_cairo_to_sierra(input_path, maybe_cairo_paths)
        .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{:?}", e)))?;

    if let Some(path) = output_path {
        fs::write(path, sierra).map_err(|e| {
            PyErr::new::<RuntimeError, _>(format!("Failed to write output: {:?}", e))
        })?;
        return Ok(None);
    }
    Ok(Some(sierra))
}

fn starknet_cairo_to_sierra(
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

    Ok(sierra)
}

#[pyfunction]
fn compile_starknet_contract_to_casm_from_path(
    input_path: &str,
    output_path: Option<&str>,
    maybe_cairo_paths: Option<Vec<&str>>,
) -> PyResult<Option<String>> {
    let casm = starknet_cairo_to_casm(input_path, maybe_cairo_paths)
        .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{:?}", e)))?;

    if let Some(path) = output_path {
        fs::write(path, casm).map_err(|e| {
            PyErr::new::<RuntimeError, _>(format!("Failed to write output: {:?}", e))
        })?;
        return Ok(None);
    }
    Ok(Some(casm))
}

fn starknet_sierra_to_casm(sierra: &str) -> Result<String, anyhow::Error> {
    let contract_class: ContractClass =
        serde_json::from_str(&sierra[..]).with_context(|| "deserialization Failed.")?;

    let casm_contract = CasmContractClass::from_contract_class(contract_class, true)
        .with_context(|| "Compilation failed.")?;

    let casm =
        serde_json::to_string_pretty(&casm_contract).with_context(|| "Serialization failed.")?;

    Ok(casm)
}

fn starknet_cairo_to_casm(
    input_path: &str,
    maybe_cairo_paths: Option<Vec<&str>>,
) -> Result<String, anyhow::Error> {
    let sierra = starknet_cairo_to_sierra(input_path, maybe_cairo_paths)?;
    starknet_sierra_to_casm(&sierra)
}

#[pyfunction]
fn compile_starknet_contract_sierra_to_casm_from_path(
    input_path: &str,
    output_path: Option<&str>,
) -> PyResult<Option<String>> {
    let sierra = fs::read_to_string(input_path).expect("Could not read file!");
    let casm = starknet_sierra_to_casm(&sierra)
        .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{:?}", e)))?;

    if let Some(path) = output_path {
        fs::write(path, casm).map_err(|e| {
            PyErr::new::<RuntimeError, _>(format!("Failed to write output: {:?}", e))
        })?;
        return Ok(None);
    }
    Ok(Some(casm))
}

// returns tuple[sierra if no output_path, list[test_name, test_config]]
#[pyfunction]
fn collect_tests(
    input_path: String,
    output_path: Option<String>,
    maybe_cairo_paths: Option<Vec<String>>,
    maybe_builtins: Option<Vec<String>>,
) -> PyResult<(Option<String>, Vec<CollectedTest>)> {
    let (sierra_code, collected) = internal_collect_tests(
        &input_path,
        output_path.as_ref(),
        maybe_cairo_paths.as_ref().map(|a| a.iter().map(|b| b).collect::<Vec<&String>>()),
        maybe_builtins.as_ref().map(|a| a.iter().map(|b| b).collect::<Vec<&String>>()),
    )
    .map_err(|e| {
        PyErr::new::<RuntimeError, _>(format!(
            "Failed to setup project for path({}): {:?}",
            input_path, e
        ))
    })?;
    let external_collected = collected.iter().map(|c| (c.name.clone(), c.available_gas)).collect();

    Ok((sierra_code, external_collected))
}

#[pyfunction]
fn compile_protostar_sierra_to_casm(
    collected_tests: Vec<CollectedTest>,
    input_data: String,
    output_path: Option<&str>,
) -> PyResult<Option<String>> {
    let internal_collected = collected_tests
        .iter()
        .map(|c| TestConfig { name: c.0.clone(), available_gas: c.1.clone() })
        .collect();
    let casm = build_protostar_casm_from_sierra(
        &internal_collected,
        input_data,
        output_path.map(|s| s.to_string()),
    )
    .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{:?}", e)))?;
    Ok(casm)
}

#[pyfunction]
fn compile_protostar_sierra_to_casm_from_path(
    collected_tests: Vec<CollectedTest>,
    input_path: &str,
    output_path: Option<&str>,
) -> PyResult<Option<String>> {
    let input_data = fs::read_to_string(input_path).expect("Could not read file!");
    let internal_collected = collected_tests
        .iter()
        .map(|c| TestConfig { name: c.0.clone(), available_gas: c.1.clone() })
        .collect();
    let casm = build_protostar_casm_from_sierra(
        &internal_collected,
        input_data,
        output_path.map(|s| s.to_string()),
    )
    .map_err(|e| PyErr::new::<RuntimeError, _>(format!("{:?}", e)))?;

    Ok(casm)
}

#[pymodule]
fn cairo_python_bindings(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_wrapped(wrap_pyfunction!(compile_starknet_contract_to_casm_from_path))?;
    m.add_wrapped(wrap_pyfunction!(compile_starknet_contract_to_sierra_from_path))?;
    m.add_wrapped(wrap_pyfunction!(compile_starknet_contract_sierra_to_casm_from_path))?;
    m.add_wrapped(wrap_pyfunction!(collect_tests))?;
    m.add_wrapped(wrap_pyfunction!(compile_protostar_sierra_to_casm))?;
    m.add_wrapped(wrap_pyfunction!(compile_protostar_sierra_to_casm_from_path))?;
    Ok(())
}
