use std::fs;
use std::path::PathBuf;

use itertools::Itertools;

/// Reads an example Sierra program that matches `name`.
pub fn read_sierra_example_file(name: &str) -> String {
    // Pop the "/sierra_to_casm" suffix.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_owned();
    path.extend(["cairo-lang-sierra", "examples", &format!("{name}.sierra")]);
    fs::read_to_string(path).unwrap()
}

/// Removes all comments and empty lines from the given program.
pub fn strip_comments_and_linebreaks(program: &str) -> String {
    return program
        .split('\n')
        .filter(|line| !(line.is_empty() || line.starts_with("//")))
        .join("\n")
        + "\n";
}
