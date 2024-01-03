use cairo_lang_sierra::ProgramParser;
use cairo_lang_test_utils::parse_test_file::{TestFileRunner, TestRunnerResult};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::{compute_bytecode_segment_lengths, find_functions_segments, get_segment_lengths};

/// Tests `find_functions_segments` returns the correct result.
#[derive(Default)]
struct FindFunctionsSegmentsTest;
impl TestFileRunner for FindFunctionsSegmentsTest {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        _args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        let sierra_program = ProgramParser::new()
            .parse(inputs.get("sierra").expect("Missing argument 'sierra' in test scenario."))
            .unwrap();

        let result_str = format!("{:?}", find_functions_segments(&sierra_program));
        TestRunnerResult::success([("segments".to_string(), result_str)].into_iter().collect())
    }
}

cairo_lang_test_utils::test_file_test_with_runner!(
    test_find_functions_segments,
    "src/contract_segmentation_test_data",
    { find_functions_segments: "find_functions_segments" },
    FindFunctionsSegmentsTest
);

#[test]
fn test_get_segment_lengths() {
    // Tests get_segment_lengths returns the correct result.
    let segment_starts_offsets = vec![0, 10, 14, 14, 16];
    let bytecode_len = 20;
    let expected_lengths = vec![10, 4, 2, 4];
    assert_eq!(get_segment_lengths(&segment_starts_offsets, bytecode_len), expected_lengths);

    let bytecode_len2 = 16;
    let expected_lengths2 = vec![10, 4, 2];
    assert_eq!(get_segment_lengths(&segment_starts_offsets, bytecode_len2), expected_lengths2);
}

/// Tests `bytecode_segment_length` returns the correct result.
#[derive(Default)]
struct BytecodeSegmentLengthTest;
impl TestFileRunner for BytecodeSegmentLengthTest {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        _args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        let sierra_program = ProgramParser::new()
            .parse(inputs.get("sierra").expect("Missing argument 'sierra' in test scenario."))
            .unwrap();
        let metadata =
            cairo_lang_sierra_to_casm::metadata::calc_metadata(&sierra_program, Default::default())
                .expect("Metadata calculation failed.");

        let gas_usage_check = false;
        let cairo_program = cairo_lang_sierra_to_casm::compiler::compile(
            &sierra_program,
            &metadata,
            gas_usage_check,
        )
        .expect("Compilation failed.");
        let bytecode_len = cairo_program.assemble().bytecode.len();
        let cairo_program_str = format!("{}", cairo_program);
        let result_str = format!(
            "{:?}",
            compute_bytecode_segment_lengths(&sierra_program, &cairo_program, bytecode_len)
        );
        TestRunnerResult::success(
            [
                ("cairo_program".to_string(), cairo_program_str),
                ("bytecode_lengths".to_string(), result_str),
            ]
            .into_iter()
            .collect(),
        )
    }
}

cairo_lang_test_utils::test_file_test_with_runner!(
    test_bytecode_segment_lengths,
    "src/contract_segmentation_test_data",
    { find_functions_segments: "bytecode_segment_lengths" },
    BytecodeSegmentLengthTest
);
