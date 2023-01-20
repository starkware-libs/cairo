use std::path::PathBuf;

pub fn detect_corelib() -> PathBuf {
    let maybe_env_var: Option<String>;
    if let Ok(cargo_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        maybe_env_var = Some(cargo_dir);
    } else {
        maybe_env_var = option_env!("CARGO_MANIFEST_DIR").map(|s| s.to_string());
    }
    if let Some(cargo_dir) = maybe_env_var {
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
