use std::path::{Path, PathBuf};
use std::sync::{LazyLock, Mutex};

use cairo_lang_compiler::CompilerConfig;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::ProjectConfig;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::Directory;
use cairo_lang_lowering::utils::InliningStrategy;
use cairo_lang_starknet_classes::allowed_libfuncs::BUILTIN_ALL_LIBFUNCS_LIST;
use cairo_lang_starknet_classes::contract_class::ContractClass;
use cairo_lang_test_utils::test_lock;
use itertools::Itertools;

use crate::compile::compile_contract_in_prepared_db;
use crate::starknet_plugin_suite;

/// Returns a path to example contract that matches `name`.
pub fn get_example_file_path(file_name: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.extend(["test_data", file_name]);
    path
}

/// Salsa database configured to find the corelib, when reused by different tests should be able to
/// use the cached queries that rely on the corelib's code, which vastly reduces the tests runtime.
pub static SHARED_DB: LazyLock<Mutex<RootDatabase>> = LazyLock::new(|| {
    Mutex::new(
        RootDatabase::builder()
            .detect_corelib()
            .with_default_plugin_suite(starknet_plugin_suite())
            .build()
            .unwrap(),
    )
});

const CONTRACTS_CRATE_DIR: &str = "cairo_level_tests";

/// Salsa database configured to find the corelib, and the contracts crate. When reused by different
/// tests should be able to use the cached queries that rely on the corelib's or the contracts
/// crates code, which vastly reduces the tests runtime.
pub static SHARED_DB_WITH_CONTRACTS: LazyLock<Mutex<RootDatabase>> = LazyLock::new(|| {
    Mutex::new(
        RootDatabase::builder()
            .detect_corelib()
            .with_project_config(
                ProjectConfig::from_directory(Path::new(CONTRACTS_CRATE_DIR)).unwrap(),
            )
            .with_default_plugin_suite(starknet_plugin_suite())
            .build()
            .unwrap(),
    )
});

/// Returns the compiled test contract from the contracts crate, with replaced ids.
pub fn get_test_contract(example_file_name: &str) -> ContractClass {
    let locked_db = test_lock(&SHARED_DB_WITH_CONTRACTS);
    let db = locked_db.snapshot();
    drop(locked_db);
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
    let main_crate_ids = vec![**contracts_crate_id];
    let diagnostics_reporter =
        DiagnosticsReporter::default().with_crates(&main_crate_ids).allow_warnings();
    compile_contract_in_prepared_db(
        &db,
        Some(example_file_name),
        main_crate_ids,
        CompilerConfig {
            replace_ids: true,
            allowed_libfuncs_list_name: Some(BUILTIN_ALL_LIBFUNCS_LIST.to_string()),
            diagnostics_reporter,
            add_statements_functions: false,
            add_statements_code_locations: false,
            inlining_strategy: InliningStrategy::Default,
        },
    )
    .expect("compile_path failed")
}
