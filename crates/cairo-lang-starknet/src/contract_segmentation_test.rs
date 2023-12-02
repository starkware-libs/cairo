use cairo_lang_sierra::ProgramParser;
use cairo_lang_test_utils::parse_test_file::{TestFileRunner, TestRunnerResult};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::find_segments;

/// Tests `find_segments` returns the correct result.
#[derive(Default)]
struct FindSegmentTest;
impl TestFileRunner for FindSegmentTest {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        _args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        let sierra_program = ProgramParser::new()
            .parse(inputs.get("sierra").expect("Missing argument 'sierra' in test scenario."))
            .unwrap();

        let result_str = format!("{:?}", find_segments(&sierra_program));
        TestRunnerResult::success([("segments".to_string(), result_str)].into_iter().collect())
    }
}

cairo_lang_test_utils::test_file_test_with_runner!(
    test_find_segment,
    "src/contract_segmentation_test_data",
    { find_segment: "find_segment" },
    FindSegmentTest
);
