use std::collections::HashMap;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::{DiagnosticNote, DiagnosticsBuilder};
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::test_utils::{setup_test_expr, setup_test_function, setup_test_module};
use cairo_lang_syntax::node::{Terminal, TypedStablePtr};
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{LookupIntern, Upcast, extract_matches};
use itertools::Itertools;
use pretty_assertions::assert_eq;

use crate::FlatLowered;
use crate::db::LoweringGroup;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnosticKind};
use crate::fmt::LoweredFormatter;
use crate::ids::{ConcreteFunctionWithBodyId, LocationId};
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    lowering,
    "src/test_data",
    {
        assignment :"assignment",
        call :"call",
        constant :"constant",
        coupon :"coupon",
        closure :"closure",
        cycles :"cycles",
        literal :"literal",
        destruct :"destruct",
        enums :"enums",
        error_propagate :"error_propagate",
        generics :"generics",
        extern_ :"extern",
        fixed_size_array :"fixed_size_array",
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
        for_ :"for",
    },
    test_function_lowering
);

fn test_function_lowering(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
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
    let formatted_lowering_diagnostics = diagnostics.format(db);
    let combined_diagnostics =
        format!("{}\n{}", semantic_diagnostics, formatted_lowering_diagnostics);
    let error = verify_diagnostics_expectation(args, &combined_diagnostics);
    let lowering_format = lowered.map(|lowered| formatted_lowered(db, &lowered)).unwrap_or(
        "<Failed lowering function - run with RUST_LOG=warn (or less) to see diagnostics>"
            .to_string(),
    );
    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("semantic_diagnostics".into(), semantic_diagnostics),
            ("lowering_diagnostics".into(), formatted_lowering_diagnostics),
            ("lowering_flat".into(), lowering_format),
        ]),
        error,
    }
}

fn formatted_lowered(db: &dyn LoweringGroup, lowered: &FlatLowered) -> String {
    let lowered_formatter = LoweredFormatter::new(db, &lowered.variables);
    format!("{:?}", lowered.debug(&lowered_formatter))
}

#[test]
fn test_location_and_diagnostics() {
    let db = &mut LoweringDatabaseForTesting::default();

    let test_expr = setup_test_expr(db, "a = a * 3", "", "let mut a = 5;", None).unwrap();

    let function_body = db.function_body(test_expr.function_id).unwrap();

    let expr_location = StableLocation::new(
        extract_matches!(
            &function_body.arenas.exprs[test_expr.expr_id],
            semantic::Expr::Assignment
        )
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
        .lookup_intern(db);

    assert_eq!(
        format!("{:?}", location.debug(db)),
        indoc::indoc! {"
lib.cairo:1:1-3:4
  fn test_func() { let mut a = 5; {
 _^
| a = a * 3
| }; }
|____^
note: this error originates in auto-generated withdraw_gas logic.
note: Adding destructor for:
  --> lib.cairo:2:1
a = a * 3
^^^^^^^^^"}
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
 --> lib.cairo:1:1-3:4
  fn test_func() { let mut a = 5; {
 _^
| a = a * 3
| }; }
|____^
note: this error originates in auto-generated withdraw_gas logic.
note: Adding destructor for:
  --> lib.cairo:2:1
a = a * 3
^^^^^^^^^

"}
    );
}

#[test]
fn test_sizes() {
    let db = &mut LoweringDatabaseForTesting::default();
    let type_to_size = [
        ("u8", 1),
        ("u256", 2),
        ("felt252", 1),
        ("()", 0),
        ("(u8, u16)", 2),
        ("(u8, u256, u32)", 4),
        ("Array<u8>", 2),
        ("Array<u256>", 2),
        ("Array<felt252>", 2),
        ("Result<(), ()>", 1),
        ("Result<(), u16>", 2),
        ("Result<(), u256>", 3),
        ("Result<u8, ()>", 2),
        ("Result<u8, u16>", 2),
        ("Result<u8, u256>", 3),
        ("Result<u256, ()>", 3),
        ("Result<u256, u16>", 3),
        ("Result<u256, u256>", 3),
        ("[u256; 10]", 20),
        ("[felt252; 7]", 7),
        ("@[felt252; 7]", 7),
        ("core::cmp::min::<u8>::Coupon", 0),
    ];

    let test_module = setup_test_module(
        db,
        &type_to_size
            .iter()
            .enumerate()
            .map(|(i, (ty_str, _))| format!("type T{i} = {ty_str};\n"))
            .join(""),
    )
    .unwrap();
    let db: &LoweringDatabaseForTesting = db;
    let type_aliases = db.module_type_aliases(test_module.module_id).unwrap();
    assert_eq!(type_aliases.len(), type_to_size.len());
    let alias_expected_size = HashMap::<_, _>::from_iter(
        type_to_size.iter().enumerate().map(|(i, (_, size))| (format!("T{i}"), *size)),
    );
    for (alias_id, alias) in type_aliases.iter() {
        let ty = db.module_type_alias_resolved_type(*alias_id).unwrap();
        let size = db.type_size(ty);
        let alias_name = alias.name(db.upcast()).text(db.upcast());
        let expected_size = alias_expected_size[alias_name.as_str()];
        assert_eq!(size, expected_size, "Wrong size for type alias `{}`", ty.format(db.upcast()));
    }
}
