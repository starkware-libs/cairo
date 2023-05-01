use std::fs::read_to_string;

use cairo_lang_sierra::ProgramParser;
use cairo_lang_sierra_generator::canonical_id_replacer::CanonicalReplacer;
use cairo_lang_sierra_generator::replace_ids::SierraIdReplacer;
use test_case::test_case;

use super::{sierra_from_felt252s, sierra_to_felt252s};
use crate::compiler_version;
use crate::test_utils::get_example_file_path;

#[test_case("test_contract")]
#[test_case("hello_starknet")]
fn test_felt252_serde(example_file_name: &str) {
    let sierra = ProgramParser::new()
        .parse(
            &read_to_string(get_example_file_path(format!("{example_file_name}.sierra").as_str()))
                .unwrap(),
        )
        .unwrap();
    let replacer = CanonicalReplacer::from_program(&sierra);
    let sierra = replacer.apply(&sierra);
    pretty_assertions::assert_eq!(
        sierra_from_felt252s(
            &sierra_to_felt252s(
                compiler_version::current_sierra_version_id(),
                compiler_version::current_compiler_version_id(),
                &sierra
            )
            .expect("Serialization failed.")
        )
        .expect("Deserialization failed."),
        (
            compiler_version::current_sierra_version_id(),
            compiler_version::current_compiler_version_id(),
            sierra
        )
    );
}
