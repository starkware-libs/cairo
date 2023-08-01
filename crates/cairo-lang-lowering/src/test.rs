use std::ops::Deref;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::test_utils::{setup_test_expr, setup_test_function};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::chain;

use crate::add_withdraw_gas::add_withdraw_gas;
use crate::db::LoweringGroup;
use crate::destructs::add_destructs;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnosticKind};
use crate::fmt::LoweredFormatter;
use crate::ids::{ConcreteFunctionWithBodyId, LocationId};
use crate::implicits::lower_implicits;
use crate::inline::apply_inlining;
use crate::optimizations::branch_inversion;
use crate::optimizations::match_optimizer::optimize_matches;
use crate::optimizations::remappings::optimize_remappings;
use crate::optimizations::reorder_statements::reorder_statements;
use crate::panic::lower_panics;
use crate::reorganize_blocks::reorganize_blocks;
use crate::test::branch_inversion::branch_inversion;
use crate::test_utils::LoweringDatabaseForTesting;
use crate::FlatLowered;
cairo_lang_test_utils::test_file_test!(
    lowering,
    "src/test_data",
    {
        assignment :"assignment",
        call :"call",
        constant :"constant",
        literal :"literal",
        destruct :"destruct",
        enums :"enums",
        error_propagate :"error_propagate",
        generics :"generics",
        extern_ :"extern",
        arm_pattern_destructure :"arm_pattern_destructure",
        if_ :"if",
        implicits :"implicits",
        logical_operator :"logical_operator",
        loop_ :"loop",
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
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    let lowered = db.concrete_function_with_body_lowered(function_id);
    if let Ok(lowered) = &lowered {
        assert!(
            lowered.blocks.iter().all(|(_, b)| b.is_set()),
            "There should not be any unset flat blocks"
        );
    }
    let diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap_or_default();
    let lowering_format =
        lowered.map(|lowered| formatted_lowered(db, &lowered)).unwrap_or_default();

    OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering_diagnostics".into(), diagnostics.format(db)),
        ("lowering_flat".into(), lowering_format),
    ])
}

/// Tests all the lowering phases of a function (tracking logic in
/// `concrete_function_with_body_lowered`).
/// Can be used to debug cases where the transition of a specific lowering phase fails.
fn test_function_lowering_phases(
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
    let mut db = LoweringDatabaseForTesting::default();
    db.set_semantic_plugins(get_default_plugins());

    let (test_function, semantic_diagnostics) = setup_test_function(
        &mut db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();
    if !semantic_diagnostics.is_empty() {
        panic!("Got semantic diagnostics: {semantic_diagnostics}");
    }
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(&db, test_function.concrete_function_id);

    let before_all = db.priv_concrete_function_with_body_lowered_flat(function_id).unwrap();
    assert!(
        before_all.blocks.iter().all(|(_, b)| b.is_set()),
        "There should not be any unset blocks"
    );
    let mut stage_states = OrderedHashMap::<String, String>::default();
    let mut add_stage_state = |name: &str, lowered: &FlatLowered| {
        stage_states.insert(name.into(), formatted_lowered(&db, lowered));
    };
    add_stage_state("before_all", &before_all);
    let mut curr_state = before_all.deref().clone();
    let mut apply_stage = |name: &str, stage: &dyn Fn(&mut FlatLowered)| {
        (*stage)(&mut curr_state);
        add_stage_state(name, &curr_state);
    };
    apply_stage("after_inlining", &|lowered| apply_inlining(&db, function_id, lowered).unwrap());
    apply_stage("after_add_withdraw_gas", &|lowered| {
        add_withdraw_gas(&db, function_id, lowered).unwrap()
    });
    apply_stage("after_lower_panics", &|lowered| {
        *lowered = lower_panics(&db, function_id, lowered).unwrap();
    });
    apply_stage("after_add_destructs", &|lowered| add_destructs(&db, function_id, lowered));
    apply_stage("after_optimize_remappings1", &optimize_remappings);
    apply_stage("after_reorder_statements1", &|lowered| reorder_statements(&db, lowered));
    apply_stage("after_branch_inversion", &|lowered| branch_inversion(&db, lowered));
    apply_stage("after_reorder_statements2", &|lowered| reorder_statements(&db, lowered));
    apply_stage("after_optimize_matches", &optimize_matches);
    apply_stage("after_lower_implicits", &|lowered| lower_implicits(&db, function_id, lowered));
    apply_stage("after_optimize_remappings2", &optimize_remappings);
    apply_stage("after_reorder_statements3", &|lowered| reorder_statements(&db, lowered));
    apply_stage("after_reorganize_blocks (final)", &reorganize_blocks);

    let after_all = db.concrete_function_with_body_lowered(function_id).unwrap();

    // This asserts that we indeed follow the logic of `concrete_function_with_body_lowered`.
    // If something is changed there, it should be changed here too.
    assert_eq!(*after_all, curr_state);

    let diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();

    OrderedHashMap::from_iter(chain!(
        [
            ("semantic_diagnostics".into(), semantic_diagnostics),
            ("lowering_diagnostics".into(), diagnostics.format(&db)),
        ]
        .into_iter(),
        stage_states.into_iter()
    ))
}

fn formatted_lowered(db: &dyn LoweringGroup, lowered: &FlatLowered) -> String {
    let lowered_formatter = LoweredFormatter::new(db, &lowered.variables);
    format!("{:?}", lowered.debug(&lowered_formatter))
}

#[test]
fn test_diagnostics() {
    let db = &mut LoweringDatabaseForTesting::default();
    db.set_semantic_plugins(get_default_plugins());
    let test_expr = setup_test_expr(db, "a = a * 3", "", "let mut a = 5;").unwrap();
    let location = LocationId::from_stable_location(db, test_expr.function_id.stable_location(db))
        .with_auto_generation_note(db, "withdraw_gas")
        .with_auto_generation_note(db, "destructor")
        .get(db);

    let mut builder = DiagnosticsBuilder::new();

    builder.add(LoweringDiagnostic {
        location,
        kind: LoweringDiagnosticKind::CannotInlineFunctionThatMightCallItself,
    });

    // TODO(ilya): Consider moving the notes to the end of the error message.
    assert_eq!(
        builder.build().format(db),
        indoc::indoc! {"
error: Cannot inline a function that might call itself.
 --> lib.cairo:1:1
fn test_func() { let mut a = 5; {
^*******************************^
note: this error originates in auto-generated withdraw_gas logic.
note: this error originates in auto-generated destructor logic.

"}
    );
}
