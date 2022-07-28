use std::fs;

use crate::generator::{get_expected_codes, project_root};

#[test]
fn sourcegen_ast() {
    for (suffix, code) in get_expected_codes() {
        let filename = project_root().join(suffix);
        if fs::read_to_string(filename).unwrap() != code {
            panic!("Some files are not up to date. Please run `cargo run --bin generate_syntax`");
        }
    }
}
