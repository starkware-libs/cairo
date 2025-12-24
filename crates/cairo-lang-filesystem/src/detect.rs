use std::path::PathBuf;

/// Number of directory levels to search upward from CARGO_MANIFEST_DIR.
/// For a crate in `crates/cairo-lang-filesystem/`, we need to go up 1-2 levels
/// to reach the repository root where `corelib/` is located.
const CARGO_MANIFEST_DIR_SEARCH_LEVELS: std::ops::RangeInclusive<u32> = 1..=2;

/// Number of directory levels to search upward from the executable path.
/// The executable can be in `target/debug/`, `target/release/`, or installed
/// in a system directory, so we check 2-4 levels up to find the repository root.
const EXECUTABLE_SEARCH_LEVELS: std::ops::RangeInclusive<u32> = 2..=4;

/// Number of directory levels to search upward from the current working directory.
/// We only check the current directory itself (0 levels up).
const CURRENT_DIR_SEARCH_LEVELS: std::ops::RangeInclusive<u32> = 0..=0;

pub fn detect_corelib() -> Option<PathBuf> {
    for (base, up_options) in [
        // This is the directory of Cargo.toml of the current crate.
        // This is used for development of the compiler.
        (
            std::env::var("CARGO_MANIFEST_DIR").ok().map(PathBuf::from),
            CARGO_MANIFEST_DIR_SEARCH_LEVELS,
        ),
        // This is the directory of the executable.
        (std::env::current_exe().ok(), EXECUTABLE_SEARCH_LEVELS),
        // This is the current directory.
        (std::env::current_dir().ok(), CURRENT_DIR_SEARCH_LEVELS),
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
