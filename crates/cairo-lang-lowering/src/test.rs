use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::{DiagnosticNote, DiagnosticsBuilder};
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::test_utils::{setup_test_expr, setup_test_function};
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use pretty_assertions::assert_eq;

use crate::db::LoweringGroup;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnosticKind};
use crate::fmt::LoweredFormatter;
use crate::ids::{ConcreteFunctionWithBodyId, LocationId};
use crate::test_utils::LoweringDatabaseForTesting;
use crate::FlatLowered;

cairo_lang_test_utils::test_file_test!(
    lowering,
    "src/test_data",
    {
        assignment :"assignment",
        call :"call",
        constant :"constant",
        cycles :"cycles",
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
        strings :"strings",
        while_ :"while",
    },
    test_function_lowering
);

fn test_function_lowering(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    let lowered = db.final_concrete_function_with_body_lowered(function_id);
    if let Ok(lowered) = &lowered {
        assert!(
            lowered.blocks.iter().all(|(_, b)| b.is_set()),
            "There should not be any unset flat blocks"
        );
    }
    let diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap_or_default();
    let lowering_format =
        lowered.map(|lowered| formatted_lowered(db, &lowered)).unwrap_or_default();

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering_diagnostics".into(), diagnostics.format(db)),
        ("lowering_flat".into(), lowering_format),
    ]))
}

fn formatted_lowered(db: &dyn LoweringGroup, lowered: &FlatLowered) -> String {
    let lowered_formatter = LoweredFormatter::new(db, &lowered.variables);
    format!("{:?}", lowered.debug(&lowered_formatter))
}

#[test]
fn test_location_and_diagnostics() {
    let db = &mut LoweringDatabaseForTesting::default();

    let test_expr = setup_test_expr(db, "a = a * 3", "", "let mut a = 5;").unwrap();

    let function_body = db.function_body(test_expr.function_id).unwrap();

    let expr_location = StableLocation::new(
        extract_matches!(&function_body.exprs[test_expr.expr_id], semantic::Expr::Assignment)
            .stable_ptr
            .untyped(),
    )
    .diagnostic_location(db);

    let location = LocationId::from_stable_location(db, test_expr.function_id.stable_location(db))
        .with_auto_generation_note(db, "withdraw_gas")
        .with_note(
            db,
            DiagnosticNote::with_location("Adding destructor for".to_string(), expr_location),
        )
        .get(db);

    assert_eq!(
        format!("{:?}", location.debug(db)),
        indoc::indoc! {"
lib.cairo:1:1
fn test_func() { let mut a = 5; {
^*******************************^
note: this error originates in auto-generated withdraw_gas logic.
note: Adding destructor for:
  --> lib.cairo:2:1
a = a * 3
^*******^"}
    );

    let mut builder = DiagnosticsBuilder::default();

    builder.add(LoweringDiagnostic {
        location,
        kind: LoweringDiagnosticKind::CannotInlineFunctionThatMightCallItself,
    });

    assert_eq!(
        builder.build().format(db),
        indoc::indoc! {"
error: Cannot inline a function that might call itself.
 --> lib.cairo:1:1
fn test_func() { let mut a = 5; {
^*******************************^
note: this error originates in auto-generated withdraw_gas logic.
note: Adding destructor for:
  --> lib.cairo:2:1
a = a * 3
^*******^

"}
    );
}
