use std::path::{Path, PathBuf};

use cairo_lang_compiler::CompilerConfig;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::ProjectConfig;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::Directory;
use cairo_lang_lowering::utils::InliningStrategy;
use cairo_lang_semantic::db::PluginSuiteInput;
use cairo_lang_semantic::inline_macros::get_default_plugin_suite;
use cairo_lang_starknet_classes::allowed_libfuncs::BUILTIN_ALL_LIBFUNCS_LIST;
use cairo_lang_starknet_classes::contract_class::ContractClass;
use itertools::Itertools;

use crate::compile::compile_contract_in_prepared_db;
use crate::starknet_plugin_suite;

/// Returns a path to example contract that matches `name`.
pub fn get_example_file_path(file_name: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.extend(["test_data", file_name]);
    path
}

pub const CONTRACTS_CRATE_DIR: &str = "cairo_level_tests";

/// Returns the compiled test contract from the contracts crate, with replaced ids.
pub fn get_test_contract(example_file_name: &str) -> ContractClass {
    let mut db = RootDatabase::builder()
        .detect_corelib()
        .with_project_config(ProjectConfig::from_directory(Path::new(CONTRACTS_CRATE_DIR)).unwrap())
        .build()
        .unwrap();

    let crate_configs = db.crate_configs();
    let contracts_crate = crate_configs
        .iter()
        .filter(|(_, config)| match &config.root {
            Directory::Real(path) => path.starts_with(CONTRACTS_CRATE_DIR),
            Directory::Virtual { .. } => false,
        })
        .collect_vec();
    let [(contracts_crate_id, _)] = contracts_crate.as_slice() else {
        panic!(
            "Expected exactly one crate with name starting with {}, found: {:?}",
            CONTRACTS_CRATE_DIR, contracts_crate
        );
    };

    db.set_crate_plugins_from_suite(
        **contracts_crate_id,
        get_default_plugin_suite() + starknet_plugin_suite(),
    );

    let main_crate_ids = vec![**contracts_crate_id];
    let diagnostics_reporter =
        DiagnosticsReporter::default().with_crates(&main_crate_ids).allow_warnings();

    compile_contract_in_prepared_db(&db, Some(example_file_name), main_crate_ids, CompilerConfig {
        replace_ids: true,
        allowed_libfuncs_list_name: Some(BUILTIN_ALL_LIBFUNCS_LIST.to_string()),
        diagnostics_reporter,
        add_statements_functions: false,
        add_statements_code_locations: false,
        inlining_strategy: InliningStrategy::Default,
    })
    .expect("compile_path failed")
}
