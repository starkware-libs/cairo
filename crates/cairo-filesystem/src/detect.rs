use std::path::PathBuf;

macro_rules! check_parents_for_corelib {
    ($dir:ident) => {
        $dir.pop();
        $dir.push("corelib");
        if $dir.exists() {
            return $dir;
        }
        $dir.pop();
        $dir.pop();
        $dir.push("corelib");
        if $dir.exists() {
            return $dir;
        }
    };
}

pub fn detect_corelib() -> PathBuf {
    if let Ok(cargo_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        // This is the directory of Cargo.toml of the current crate.
        // This is used for developement of the compiler.
        let mut dir = PathBuf::from(cargo_dir);
        check_parents_for_corelib!(dir);
    }
    if let Ok(mut dir) = std::env::current_exe() {
        dir.pop();
        check_parents_for_corelib!(dir);
    }
    let this_file = file!();
    // This is the directory of this file.
    let mut dir = PathBuf::from(this_file);
    dir.pop(); // Path to src
    dir.pop(); // Path to this crate
    check_parents_for_corelib!(dir);

    panic!("Corelib not found.")
}
