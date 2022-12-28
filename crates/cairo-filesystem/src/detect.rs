use std::path::PathBuf;

pub fn detect_corelib() -> PathBuf {
    if let Ok(cargo_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        // This is the directory of Cargo.toml of the current crate.
        // This is used for developement of the compiler.
        let mut dir = PathBuf::from(cargo_dir);
        dir.pop();
        dir.push("corelib");
        if dir.exists() {
            return dir;
        }
        dir.pop();
        dir.pop();
        dir.push("corelib");
        if dir.exists() {
            return dir;
        }
    }
    if let Ok(mut dir) = std::env::current_exe() {
        dir.pop();
        dir.pop();
        dir.push("corelib");
        if dir.exists() {
            return dir;
        }
        dir.pop();
        dir.pop();
        dir.push("corelib");
        if dir.exists() {
            return dir;
        }
    }
    panic!("Corelib not found.")
}
