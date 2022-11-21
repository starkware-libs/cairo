use std::fs;

use test_log::test;

use crate::generator::{get_codes, project_root};

#[test]
fn sourcegen_ast() {
    for (suffix, code) in get_codes() {
        let filename = project_root().join(suffix);
        if fs::read_to_string(filename).unwrap() != code {
            panic!("Some files are not up to date. Please run `cargo run --bin generate_syntax`");
        }
    }
}
