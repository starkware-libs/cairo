use std::path::PathBuf;

use crate::contract_class::compile_path;

/// Returns a path to example contract that matches `name`.
pub fn get_example_file_path(file_name: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.extend(["test_data", file_name].into_iter());
    path
}

/// Returns the compiled test contract, with replaced ids.
pub fn get_test_contract(example_file_name: &str) -> crate::contract_class::ContractClass {
    let path = get_example_file_path(example_file_name);
    let replace_ids = true;
    compile_path(&path, replace_ids).expect("compile_path failed")
}
