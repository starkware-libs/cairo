use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_defs::ids::{ModuleId, NamedLanguageElementId};
use cairo_lang_semantic::test_utils::{
    TARGET_FUNCTION_ATTR, resolve_target_functions, setup_test_module, with_target_function_plugin,
};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::program_generator::SierraProgramWithDebug;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_starknet::starknet_plugin_suite;
use cairo_lang_test_utils::get_direct_or_file_content;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::{ProfilingInfoProcessor, ProfilingInfoProcessorParams};
use crate::{ProfilingInfoCollectionConfig, SierraCasmRunner};

cairo_lang_test_utils::test_file_test!(
    profiling,
    "src/profiling_test_data/",
    {
        major_test_cases: "major_test_cases",
        profiling: "profiling",
        circuit: "circuit",
        scoped_statements: "scoped_statements"
    },
    test_profiling
);

/// Resolves the plain function-name string that `SierraCasmRunner::find_function` should be
/// given, either from the legacy `function_name` tag, or (when absent) from the single
/// `#[target_function]`-marked function in the parsed `cairo_code` module.
///
/// This is a bridge, not a redesign: `find_function` itself only ever takes a name-suffix string
/// (it works post-Sierra-compilation and has no notion of the semantic model), so the marker is
/// resolved here, against the pre-compilation semantic module, and only its plain name crosses
/// into the Sierra-level lookup.
fn resolve_function_name(
    db: &RootDatabase,
    module_id: ModuleId<'_>,
    inputs: &OrderedHashMap<String, String>,
) -> String {
    if let Some(function_name) = inputs.get("function_name") {
        return function_name.clone();
    }
    match resolve_target_functions(db, module_id).as_slice() {
        [free_function_id] => free_function_id.name(db).long(db).to_string(),
        [] => panic!(
            "No function marked with `#[{TARGET_FUNCTION_ATTR}]` was found. Mark the tested \
             function, or add a `function_name` tag."
        ),
        target_functions => panic!(
            "Expected a single function marked with `#[{TARGET_FUNCTION_ATTR}]`, found {}.",
            target_functions.len()
        ),
    }
}

pub fn test_profiling(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let mut profiling_info_collection_config = ProfilingInfoCollectionConfig::default();
    if let Some(max_stack_trace_depth) = inputs.get("max_stack_trace_depth") {
        profiling_info_collection_config.set_max_stack_trace_depth(
            max_stack_trace_depth.parse().expect("max_stack_trace_depth must be a number."),
        );
    }

    if inputs.get("collect_scoped_sierra_statement_weights").is_some() {
        profiling_info_collection_config.collect_scoped_sierra_statement_weights = true;
    }

    let db = RootDatabase::builder()
        .with_default_plugin_suite(with_target_function_plugin(starknet_plugin_suite()))
        .detect_corelib()
        .build()
        .unwrap();
    let (_path, cairo_code) = get_direct_or_file_content(&inputs["cairo_code"]);
    let test_module = setup_test_module(&db, &cairo_code).unwrap();
    let function_name = resolve_function_name(&db, test_module.module_id, inputs);
    let crate_input = test_module.crate_id.long(&db).clone().into_crate_input(&db);
    DiagnosticsReporter::stderr().with_crates(&[crate_input]).allow_warnings().ensure(&db).unwrap();

    // Compile to Sierra.
    let SierraProgramWithDebug { program: sierra_program, debug_info } = db
        .get_sierra_program(vec![test_module.crate_id])
        .expect("`get_sierra_program` failed. run with RUST_LOG=warn (or less) to see diagnostics");
    let sierra_program = replace_sierra_ids_in_program(&db, sierra_program);
    let statements_functions =
        debug_info.statements_locations.get_statements_functions_map_for_tests(&db);
    let runner = SierraCasmRunner::new(
        sierra_program.clone(),
        Some(Default::default()),
        OrderedHashMap::default(),
        Some(profiling_info_collection_config),
    )
    .unwrap();
    let func = runner.find_function(&function_name).unwrap();
    let result = runner
        .run_function_with_starknet_context(
            func,
            vec![],
            Some(u32::MAX as usize),
            Default::default(),
        )
        .unwrap();
    let profiling_processor =
        ProfilingInfoProcessor::new(Some(&db), &sierra_program, statements_functions);

    let profiling_params = if inputs.contains_key("scoped_mode") {
        ProfilingInfoProcessorParams {
            min_weight: 1,
            process_by_statement: false,
            process_by_concrete_libfunc: false,
            process_by_generic_libfunc: false,
            process_by_user_function: false,
            process_by_original_user_function: false,
            process_by_cairo_function: false,
            process_by_stack_trace: false,
            process_by_cairo_stack_trace: false,
            process_by_scoped_statement: true,
        }
    } else {
        Default::default()
    };
    let processed_profiling_info =
        profiling_processor.process(&result.profiling_info.unwrap(), &profiling_params);

    TestRunnerResult {
        outputs: OrderedHashMap::from([(
            "expected_profiling_info".into(),
            processed_profiling_info.to_string(),
        )]),
        error: None,
    }
}
