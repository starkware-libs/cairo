use std::fs::read_to_string;

use cairo_lang_sierra::ProgramParser;
use test_case::test_case;

use super::{sierra_from_felts, sierra_to_felts};
use crate::test_utils::get_example_file_path;

#[test_case("test_contract")]
#[test_case("hello_starknet")]
fn test_felt_serde(example_file_name: &str) {
    let sierra = ProgramParser::new()
        .parse(
            &read_to_string(get_example_file_path(
                format!("{}.sierra", example_file_name).as_str(),
            ))
            .unwrap(),
        )
        .unwrap();
    pretty_assertions::assert_eq!(
        sierra_from_felts(&sierra_to_felts(&sierra).expect("Serialization failed."))
            .expect("Deserialization failed."),
        sierra
    );
}
