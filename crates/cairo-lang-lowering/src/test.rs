use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    lowering,
    "src/test_data",
    {
        assignment :"assignment",
        borrow_check :"borrow_check",
        call :"call",
        enums :"enums",
        error_propagate :"error_propagate",
        extern_ :"extern",
        arm_pattern_destructure :"arm_pattern_destructure",
        if_ :"if",
        match_ :"match",
        panic :"panic",
        rebindings :"rebindings",
        struct_ :"struct",
        tests :"tests",
        tuple :"tuple",
    },
    test_function_lowering
);

fn test_function_lowering(
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
    let db = &mut LoweringDatabaseForTesting::default();
    db.set_semantic_plugins(get_default_plugins());
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();
    let structured_lowered =
        db.free_function_lowered_structured(test_function.function_id).unwrap();
    let flat_lowered = db.free_function_lowered_flat(test_function.function_id).unwrap();
    let mut diagnostics = DiagnosticsBuilder::default();
    diagnostics.extend(structured_lowered.diagnostics.clone());
    diagnostics.extend(flat_lowered.diagnostics.clone());
    let diagnostics = diagnostics.build();

    let lowered_formatter = LoweredFormatter { db, variables: &flat_lowered.variables };
    OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering_diagnostics".into(), diagnostics.format(db)),
        (
            "lowering_structured".into(),
            format!("{:?}", structured_lowered.debug(&lowered_formatter)),
        ),
        ("lowering_flat".into(), format!("{:?}", flat_lowered.debug(&lowered_formatter))),
    ])
}
