use cairo_lang_sierra::ProgramParser;
use cairo_lang_test_utils::get_direct_or_file_content;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::ProfilingInfoProcessor;
use crate::SierraCasmRunner;

cairo_lang_test_utils::test_file_test!(
    profiling,
    "src/profiling_test_data/",
    {
        profiling: "profiling",
    },
    test_profiling
);

pub fn test_profiling(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let (_path, sierra_code) = get_direct_or_file_content(&inputs["sierra_code"]);
    let entry_point_name = &inputs["entry_point_name"];

    let sierra_program = ProgramParser::new().parse(&sierra_code).unwrap();

    let runner =
        SierraCasmRunner::new(sierra_program.clone(), None, OrderedHashMap::default(), true)
            .unwrap();
    let func = runner.find_function(entry_point_name).unwrap();
    let result =
        runner.run_function_with_starknet_context(func, &[], None, Default::default()).unwrap();

    let profiling_processor = ProfilingInfoProcessor::new(sierra_program);
    let processed_profiling_info = profiling_processor.process(&result.profiling_info.unwrap());

    TestRunnerResult {
        outputs: OrderedHashMap::from([(
            "expected_profiling_info".into(),
            processed_profiling_info.to_string(),
        )]),
        error: None,
    }
}
