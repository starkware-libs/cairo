use std::path::{Path, PathBuf};

#[allow(clippy::reversed_empty_ranges)]
pub fn detect_corelib() -> Option<PathBuf> {
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
        // This is the directory of Cargo.toml of the current crate.
        // This is used for development of the compiler.
        let dir = Path::new(&cargo_dir);
        try_path!(dir, 1);
        try_path!(dir, 2);
    }

    if let Ok(dir) = std::env::current_exe() {
        try_path!(&dir, 2);
        try_path!(&dir, 3);
        try_path!(&dir, 4);
    }

    if let Ok(dir) = std::env::current_dir() {
        try_path!(&dir, 0);
    }

    None
}
