use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::ProjectConfig;
use cairo_lang_compiler::CompilerConfig;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::Directory;
use cairo_lang_test_utils::test_lock;
use itertools::Itertools;
use once_cell::sync::Lazy;

use crate::allowed_libfuncs::BUILTIN_ALL_LIBFUNCS_LIST;
use crate::contract_class::compile_contract_in_prepared_db;
use crate::inline_macros::get_starknet_inline_macro_plugins;
use crate::plugin::StarkNetPlugin;

/// Returns a path to example contract that matches `name`.
pub fn get_example_file_path(file_name: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.extend(["test_data", file_name]);
    path
}

/// Salsa database configured to find the corelib, when reused by different tests should be able to
/// use the cached queries that rely on the corelib's code, which vastly reduces the tests runtime.
pub static SHARED_DB: Lazy<Mutex<RootDatabase>> = Lazy::new(|| {
    Mutex::new(
        RootDatabase::builder()
            .detect_corelib()
            .with_macro_plugin(Arc::new(StarkNetPlugin::default()))
            .with_inline_macro_plugins(get_starknet_inline_macro_plugins())
            .build()
            .unwrap(),
    )
});

const CONTRACTS_CRATE_DIR: &str = "cairo_level_tests";

/// Salsa database configured to find the corelib, and the contracts crate. When reused by different
/// tests should be able to use the cached queries that rely on the corelib's or the contracts
/// crates code, which vastly reduces the tests runtime.
pub static SHARED_DB_WITH_CONTRACTS: Lazy<Mutex<RootDatabase>> = Lazy::new(|| {
    Mutex::new(
        RootDatabase::builder()
            .detect_corelib()
            .with_project_config(
                ProjectConfig::from_directory(Path::new(CONTRACTS_CRATE_DIR)).unwrap(),
            )
            .with_macro_plugin(Arc::new(StarkNetPlugin::default()))
            .with_inline_macro_plugins(get_starknet_inline_macro_plugins())
            .build()
            .unwrap(),
    )
});

/// Returns the compiled test contract from the contracts crate, with replaced ids.
pub fn get_test_contract(example_file_name: &str) -> crate::contract_class::ContractClass {
    let locked_db = test_lock(&SHARED_DB_WITH_CONTRACTS);
    let db = locked_db.snapshot();
    drop(locked_db);
    let crate_roots = db.crate_roots();
    let contracts_crate = crate_roots
        .iter()
        .filter(|(_, dir)| match dir {
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
    let diagnostics_reporter = DiagnosticsReporter::default().with_extra_crates(&main_crate_ids);
    compile_contract_in_prepared_db(
        &db,
        Some(example_file_name),
        main_crate_ids,
        CompilerConfig {
            replace_ids: true,
            allowed_libfuncs_list_name: Some(BUILTIN_ALL_LIBFUNCS_LIST.to_string()),
            diagnostics_reporter,
        },
    )
    .expect("compile_path failed")
}

/// Converts a Cairo path of a contract (e.g. `a::b::c`) to a file name with underscores (a_b_c).
pub fn get_contract_file_name_from_path(path: &str) -> String {
    path.replace("::", "__")
}
