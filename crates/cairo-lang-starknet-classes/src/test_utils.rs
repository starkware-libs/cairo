use std::path::PathBuf;

/// Returns a path to example contract that matches `name`.
pub fn get_example_file_path(file_name: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.pop(); // Remove `cairo-lang-starknet-classes`
    path.extend(["cairo-lang-starknet", "test_data", file_name]);
    path
}
