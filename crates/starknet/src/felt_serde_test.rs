use std::fs::read_to_string;

use sierra::ProgramParser;

use super::{sierra_from_felts, sierra_to_felts};
use crate::test_utils::get_example_file_path;

#[test]
fn test_felt_serde() {
    let sierra = ProgramParser::new()
        .parse(&read_to_string(get_example_file_path("test_contract.sierra")).unwrap())
        .unwrap();
    pretty_assertions::assert_eq!(
        sierra_from_felts(&sierra_to_felts(&sierra).expect("Serialization failed."))
            .expect("Deserialization failed."),
        sierra
    );
}
