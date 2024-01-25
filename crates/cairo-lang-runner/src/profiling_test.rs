use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_starknet::starknet_plugin_suite;
use cairo_lang_test_utils::get_direct_or_file_content;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::ProfilingInfoProcessor;
use crate::SierraCasmRunner;

cairo_lang_test_utils::test_file_test!(
    profiling,
    "src/profiling_test_data/",
    {
        major_test_cases: "major_test_cases",
        profiling: "profiling",
    },
    test_profiling
);

pub fn test_profiling(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let prev_max_stack_trace_depth_env_var = std::env::var("MAX_STACK_TRACE_DEPTH");
    if let Some(max_stack_trace_depth) = inputs.get("max_stack_trace_depth") {
        std::env::set_var("MAX_STACK_TRACE_DEPTH", max_stack_trace_depth);
    }

    let db = RootDatabase::builder()
        .with_plugin_suite(starknet_plugin_suite())
        .detect_corelib()
        .build()
        .unwrap();
    let (_path, cairo_code) = get_direct_or_file_content(&inputs["cairo_code"]);
    let test_module = setup_test_module(&db, &cairo_code).unwrap();
    DiagnosticsReporter::stderr().with_crates(&[test_module.crate_id]).ensure(&db).unwrap();

    // Compile to Sierra.
    let (sierra_program, statements_locations) =
        db.get_sierra_program(vec![test_module.crate_id]).unwrap();
    let sierra_program = replace_sierra_ids_in_program(&db, &sierra_program);
    let statements_functions = statements_locations.get_statements_functions_map(&db);
    let runner = SierraCasmRunner::new(
        sierra_program.clone(),
        Some(Default::default()),
        OrderedHashMap::default(),
        true,
    )
    .unwrap();
    let func = runner.find_function(&inputs["function_name"]).unwrap();
    let result = runner
        .run_function_with_starknet_context(func, &[], Some(u32::MAX as usize), Default::default())
        .unwrap();
    let profiling_processor = ProfilingInfoProcessor::new(sierra_program, statements_functions);
    let processed_profiling_info = profiling_processor.process(&result.profiling_info.unwrap());

    if inputs.contains_key("max_stack_trace_depth") {
        match prev_max_stack_trace_depth_env_var {
            Ok(val) => std::env::set_var("MAX_STACK_TRACE_DEPTH", val),
            Err(_) => std::env::remove_var("MAX_STACK_TRACE_DEPTH"),
        }
    }

    TestRunnerResult {
        outputs: OrderedHashMap::from([(
            "expected_profiling_info".into(),
            processed_profiling_info.to_string(),
        )]),
        error: None,
    }
}
