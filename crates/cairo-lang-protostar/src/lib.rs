use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::{Context, Result};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::project::{
    get_main_crate_ids_from_project, setup_single_file_project,
    update_crate_roots_from_project_config, ProjectError,
};
use cairo_lang_compiler::CompilerConfig;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_project::{DeserializationError, ProjectConfig, ProjectConfigContent};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_sierra::program::Program;
use cairo_lang_sierra::ProgramParser;
use cairo_lang_starknet::contract_class::{compile_contract_in_prepared_db, ContractClass};
use cairo_lang_starknet::plugin::StarkNetPlugin;
use casm_generator::{SierraCasmGenerator, TestConfig};
use smol_str::SmolStr;

pub mod casm_generator;
pub mod test_collector;

pub fn build_project_config(
    source_root: &Path,
    crate_name: &str,
) -> Result<ProjectConfig, DeserializationError> {
    let base_path: PathBuf = source_root.to_str().ok_or(DeserializationError::PathError)?.into();
    let crate_roots = HashMap::from([(SmolStr::from(crate_name), base_path.clone())]);
    Ok(ProjectConfig { base_path, content: ProjectConfigContent { crate_roots }, corelib: None })
}

pub fn build_protostar_casm(
    collected_tests: &Vec<TestConfig>,
    sierra_contents: &str,
) -> anyhow::Result<String> {
    let program: Program = ProgramParser::new().parse(&sierra_contents).unwrap();

    let casm_generator = match SierraCasmGenerator::new(program) {
        Ok(casm_generator) => casm_generator,
        Err(e) => panic!("{}", e),
    };
    let protostar_casm = casm_generator.build_casm(collected_tests)?;
    let res = serde_json::to_string_pretty(&protostar_casm).context("Serialization failed.")?;
    Ok(res)
}

pub fn build_protostar_casm_from_sierra(
    collected_tests: &Vec<TestConfig>,
    sierra_code: String,
    maybe_output_path: Option<String>,
) -> anyhow::Result<Option<String>> {
    let casm_contents = build_protostar_casm(collected_tests, &sierra_code[..])?;

    if let Some(output_path) = maybe_output_path {
        fs::write(output_path, casm_contents).with_context(|| "Failed to write output.")?;
        return Ok(None);
    }
    Ok(Some(casm_contents))
}

pub fn setup_project_without_cairo_project_toml(
    db: &mut dyn SemanticGroup,
    path: &Path,
    crate_name: &str,
) -> Result<Vec<CrateId>, ProjectError> {
    if path.is_dir() {
        match build_project_config(path, crate_name) {
            Ok(config) => {
                let main_crate_ids = get_main_crate_ids_from_project(db, &config);
                update_crate_roots_from_project_config(db, config);
                Ok(main_crate_ids)
            }
            _ => Err(ProjectError::LoadProjectError),
        }
    } else {
        Ok(vec![setup_single_file_project(db, path)?])
    }
}

pub fn compile_from_resolved_dependencies(
    input_path: &str,
    contract_path: Option<&str>,
    compiler_config: CompilerConfig<'_>,
    maybe_cairo_paths: Option<Vec<(&str, &str)>>,
) -> Result<ContractClass> {
    let mut db = RootDatabase::builder()
        .detect_corelib()
        .with_semantic_plugin(Arc::new(StarkNetPlugin::default()))
        .build()?;

    let cairo_paths = match maybe_cairo_paths {
        Some(paths) => paths,
        None => vec![],
    };
    let main_crate_name = match cairo_paths.iter().find(|(path, _crate_name)| **path == *input_path)
    {
        Some((_crate_path, crate_name)) => crate_name,
        None => "",
    };

    let main_crate_ids =
        setup_project_without_cairo_project_toml(&mut db, Path::new(&input_path), main_crate_name)?;
    for (cairo_path, crate_name) in cairo_paths {
        setup_project_without_cairo_project_toml(&mut db, Path::new(cairo_path), crate_name)?;
    }

    compile_contract_in_prepared_db(&mut db, contract_path, main_crate_ids, compiler_config)
}
