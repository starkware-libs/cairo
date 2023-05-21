use std::ops::DerefMut;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_compiler::CompilerConfig;
use cairo_lang_test_utils::test_lock;
use once_cell::sync::Lazy;

use crate::allowed_libfuncs::DEFAULT_EXPERIMENTAL_LIBFUNCS_LIST;
use crate::contract_class::compile_contract_in_prepared_db;
use crate::plugin::StarkNetPlugin;

/// Returns a path to example contract that matches `name`.
pub fn get_example_file_path(file_name: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.extend(["test_data", file_name].into_iter());
    path
}

/// Salsa database configured to find the corelib, when reused by different tests should be able to
/// use the cached queries that rely on the corelib's code, which vastly reduces the tests runtime.
static SHARED_DB: Lazy<Mutex<RootDatabase>> = Lazy::new(|| {
    Mutex::new(
        RootDatabase::builder()
            .detect_corelib()
            .with_semantic_plugin(Arc::new(StarkNetPlugin::default()))
            .build()
            .unwrap(),
    )
});

/// Returns the compiled test contract, with replaced ids.
pub fn get_test_contract(example_file_name: &str) -> crate::contract_class::ContractClass {
    let path = get_example_file_path(example_file_name);
    let mut locked_db = test_lock(&SHARED_DB);
    // Setting up the contract path.
    let main_crate_ids =
        setup_project(locked_db.deref_mut(), Path::new(&path)).expect("failed to setup project");
    let db = locked_db.snapshot();
    compile_contract_in_prepared_db(
        &db,
        None,
        main_crate_ids,
        CompilerConfig {
            replace_ids: true,
            allowed_libfuncs_list_name: Some(DEFAULT_EXPERIMENTAL_LIBFUNCS_LIST.to_string()),
            ..CompilerConfig::default()
        },
    )
    .expect("compile_path failed")
}
