use std::fs::read_to_string;

use cairo_lang_sierra::extensions::core::CoreLibfunc;
use cairo_lang_sierra::extensions::GenericLibfunc;
use cairo_lang_sierra::ProgramParser;
use cairo_lang_sierra_generator::canonical_id_replacer::CanonicalReplacer;
use cairo_lang_sierra_generator::replace_ids::SierraIdReplacer;
use test_case::test_case;

use super::{sierra_from_felt252s, sierra_to_felt252s};
use crate::compiler_version;
use crate::felt252_serde::Felt252Serde;
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
    let dummy_sierra_version_id = compiler_version::VersionId { major: 1, minor: 0, patch: 0 };
    let dummy_compiler_version_id = compiler_version::VersionId { major: 2, minor: 0, patch: 0 };
    pretty_assertions::assert_eq!(
        sierra_from_felt252s(
            &sierra_to_felt252s(
                dummy_sierra_version_id.clone(),
                dummy_compiler_version_id.clone(),
                &sierra
            )
            .expect("Serialization failed.")
        )
        .expect("Deserialization failed."),
        (dummy_sierra_version_id, dummy_compiler_version_id, sierra)
    );
}

#[test]

fn test_libfunc_serde() {
    let mut output = vec![];
    for libfunc_id in CoreLibfunc::supported_ids() {
        libfunc_id.serialize(&mut output).expect("Serialization failed.");
        output.clear()
    }
}
