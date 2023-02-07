use std::path::PathBuf;

pub fn detect_corelib() -> Option<PathBuf> {
    const CORELIB_DIR_NAME: &str = "corelib";

    if let Ok(cargo_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        // This is the directory of Cargo.toml of the current crate.
        // This is used for development of the compiler.
        let mut dir = PathBuf::from(cargo_dir);
        dir.pop();
        dir.push(CORELIB_DIR_NAME);
        if dir.exists() {
            return Some(dir);
        }
        dir.pop();
        dir.pop();
        dir.push(CORELIB_DIR_NAME);
        if dir.exists() {
            return Some(dir);
        }
    }

    if let Ok(mut dir) = std::env::current_exe() {
        dir.pop();
        dir.pop();
        dir.push(CORELIB_DIR_NAME);
        if dir.exists() {
            return Some(dir);
        }
        dir.pop();
        dir.pop();
        dir.push(CORELIB_DIR_NAME);
        if dir.exists() {
            return Some(dir);
        }
    }

    None
}
