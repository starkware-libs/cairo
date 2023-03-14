use std::path::{Path, PathBuf};

pub fn detect_corelib() -> Option<PathBuf> {
    let maybe_env_var: Option<String>;
    macro_rules! try_path {
        ($base:expr, $up:expr) => {{
            let mut path = $base.to_path_buf();
            for _ in 0..$up {
                path.pop();
            }
            path.push("corelib");
            path.push("src");
            if path.exists() {
                return Some(path);
            }
        }};
    }

    if let Ok(cargo_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        maybe_env_var = Some(cargo_dir);
    } else {
        maybe_env_var = option_env!("CARGO_MANIFEST_DIR").map(|s| s.to_string());
    }
    if let Some(cargo_dir) = maybe_env_var {
        // This is the directory of Cargo.toml of the current crate.
        // This is used for development of the compiler.
        let dir = Path::new(&cargo_dir);
        try_path!(dir, 1);
        try_path!(dir, 2);
    }

    if let Ok(dir) = std::env::current_exe() {
        try_path!(&dir, 2);
        try_path!(&dir, 3);
    }

    None
}
