use std::fs;
use std::path::{Path, PathBuf};

pub fn detect_corelib() -> Option<PathBuf> {
    fn try_path(base: &Path, relative: &str) -> Option<PathBuf> {
        let path = base.join(relative);
        let path = fs::canonicalize(&path).unwrap_or(path);
        path.exists().then_some(path)
    }

    macro_rules! try_path {
        ($base:expr, $rel:expr) => {{
            let ret = try_path($base, &format!($rel));
            if ret.is_some() {
                return ret;
            }
        }};
    }

    const CORELIB_DIR_NAME: &str = "corelib/src";

    if let Ok(cargo_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        // This is the directory of Cargo.toml of the current crate.
        // This is used for development of the compiler.
        let dir = Path::new(&cargo_dir);
        try_path!(dir, "../{CORELIB_DIR_NAME}");
        try_path!(dir, "../../{CORELIB_DIR_NAME}");
    }

    if let Ok(dir) = std::env::current_exe() {
        try_path!(&dir, "../../{CORELIB_DIR_NAME}");
        try_path!(&dir, "../../../{CORELIB_DIR_NAME}");
    }

    None
}
