use std::ops::Deref;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::implicits::lower_implicits;
use crate::inline::apply_inlining;
use crate::optimizations::match_optimizer::optimize_matches;
use crate::optimizations::remappings::optimize_remappings;
use crate::panic::lower_panics;
use crate::reorganize_blocks::reorganize_blocks;
use crate::test_utils::LoweringDatabaseForTesting;
use crate::FlatLowered;

cairo_lang_test_utils::test_file_test!(
    lowering,
    "src/test_data",
    {
        assignment :"assignment",
        borrow_check :"borrow_check",
        call :"call",
        constant :"constant",
        enums :"enums",
        error_propagate :"error_propagate",
        generics :"generics",
        extern_ :"extern",
        arm_pattern_destructure :"arm_pattern_destructure",
        if_ :"if",
        match_ :"match",
        members :"members",
        panic :"panic",
        rebindings :"rebindings",
        snapshot :"snapshot",
        struct_ :"struct",
        tests :"tests",
        tuple :"tuple",
    },
    test_function_lowering
);

cairo_lang_test_utils::test_file_test!(
    lowering_phases,
    "src/test_data",
    {
        tests :"lowering_phases",
    },
    test_function_lowering_phases
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
    let lowered =
        db.concrete_function_with_body_lowered(test_function.concrete_function_id).unwrap();
    assert!(
        lowered.blocks.iter().all(|(_, b)| b.is_set()),
        "There should not be any unset flat blocks"
    );
    let diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();

    let lowered_formatter = LoweredFormatter { db, variables: &lowered.variables };
    OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering_diagnostics".into(), diagnostics.format(db)),
        ("lowering_flat".into(), format!("{:?}", lowered.debug(&lowered_formatter))),
    ])
}

/// Tests all the lowering phases of a function (tracking logic in
/// `concrete_function_with_body_lowered`).
/// Can be used to debug cases where the transition of a specific lowering phase fails.
fn test_function_lowering_phases(
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
    let concrete_function = test_function.concrete_function_id;

    let before_all = db.priv_concrete_function_with_body_lowered_flat(concrete_function).unwrap();
    assert!(
        before_all.blocks.iter().all(|(_, b)| b.is_set()),
        "There should not be any unset blocks"
    );

    let mut after_inlining = before_all.deref().clone();
    apply_inlining(db, test_function.function_id, &mut after_inlining).unwrap();

    let after_lower_panics = lower_panics(db, concrete_function, &after_inlining).unwrap();

    let mut after_lower_implicits = after_lower_panics.clone();
    lower_implicits(db, concrete_function, &mut after_lower_implicits);

    let mut after_optimize_matches = after_lower_implicits.clone();
    optimize_matches(&mut after_optimize_matches);

    let mut after_optimize_remappings = after_optimize_matches.clone();
    optimize_remappings(&mut after_optimize_remappings);

    let mut after_reorganize_blocks = after_optimize_remappings.clone();
    reorganize_blocks(&mut after_reorganize_blocks);

    let after_all = db.concrete_function_with_body_lowered(concrete_function).unwrap();

    // This asserts that we indeed follow the logic of `concrete_function_with_body_lowered`.
    // If something is changed there, it should be changed here too.
    assert_eq!(*after_all, after_reorganize_blocks);

    let diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();

    OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering_diagnostics".into(), diagnostics.format(db)),
        ("before_all".into(), formatted_lowered(db, &before_all)),
        ("after_inlining".into(), formatted_lowered(db, &after_inlining)),
        ("after_lower_panics".into(), formatted_lowered(db, &after_lower_panics)),
        ("after_lower_implicits".into(), formatted_lowered(db, &after_lower_implicits)),
        ("after_optimize_matches".into(), formatted_lowered(db, &after_optimize_matches)),
        ("after_optimize_remappings".into(), formatted_lowered(db, &after_reorganize_blocks)),
        (
            "after_reorganize_blocks (final)".into(),
            formatted_lowered(db, &after_optimize_remappings),
        ),
    ])
}

fn formatted_lowered(db: &dyn LoweringGroup, lowered: &FlatLowered) -> String {
    let lowered_formatter = LoweredFormatter { db, variables: &lowered.variables };
    format!("{:?}", lowered.debug(&lowered_formatter))
}
