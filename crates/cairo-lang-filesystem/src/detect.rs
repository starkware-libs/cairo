use std::path::PathBuf;

pub fn detect_corelib() -> Option<PathBuf> {
    for (base, up_options) in [
        // This is the directory of Cargo.toml of the current crate.
        // This is used for development of the compiler.
        (std::env::var("CARGO_MANIFEST_DIR").ok().map(PathBuf::from), 1..=2),
        // This is the directory of the executable.
        (std::env::current_exe().ok(), 2..=4),
        // This is the current directory.
        (std::env::current_dir().ok(), 0..=0),
    ] {
        let Some(base) = base else { continue };
        for up in up_options {
            let mut path = base.clone();
            for _ in 0..up {
                path.pop();
            }
            path.push("corelib");
            path.push("src");
            if path.exists() {
                return Some(path);
            }
        }
    }
    None
}
