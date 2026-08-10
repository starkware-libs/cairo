use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId};
use cairo_lang_diagnostics::{DiagnosticNote, DiagnosticsBuilder};
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::items::function_with_body::FunctionWithBodySemantic;
use cairo_lang_semantic::items::module_type_alias::ModuleTypeAliasSemantic;
use cairo_lang_semantic::path::ContextualizePath;
use cairo_lang_semantic::test_utils::{setup_test_expr, setup_test_function, setup_test_module};
use cairo_lang_syntax::node::{Terminal, TypedStablePtr};
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::{Itertools, chain};
use pretty_assertions::assert_eq;

use crate::db::LoweringGroup;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnosticKind};
use crate::ids::{ConcreteFunctionWithBodyId, LocationId, Signature};
use crate::test_utils::{LoweringDatabaseForTesting, formatted_lowered};
use crate::{BlockEnd, Lowered, LoweringStage};

cairo_lang_test_utils::test_file_test!(
    lowering,
    "src/test_data",
    {
        assignment: "assignment",
        call: "call",
        constant: "constant",
        coupon: "coupon",
        closure: "closure",
        cycles: "cycles",
        literal: "literal",
        destruct: "destruct",
        enums: "enums",
        error_propagate: "error_propagate",
        generics: "generics",
        extern_: "extern",
        fixed_size_array: "fixed_size_array",
        arm_pattern_destructure: "arm_pattern_destructure",
        if_: "if",
        inline_macros: "inline_macros",
        implicits: "implicits",
        let_else: "let_else",
        logical_operator: "logical_operator",
        loop_: "loop",
        match_: "match",
        members: "members",
        panic: "panic",
        rebindings: "rebindings",
        refutable_pattern: "refutable_pattern",
        repr_ptr: "repr_ptr",
        snapshot: "snapshot",
        struct_: "struct",
        tests: "tests",
        tuple: "tuple",
        strings: "strings",
        while_: "while",
        for_: "for",
    },
    test_function_lowering,
    ["expect_diagnostics", "no_gas"]
);

fn test_function_lowering(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut if args.get("no_gas").map(|s| s.trim()) == Some("true") {
        LoweringDatabaseForTesting::with_no_gas()
    } else {
        LoweringDatabaseForTesting::default()
    };
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    let lowered = db.lowered_body(function_id, LoweringStage::Final);
    if let Ok(lowered) = &lowered {
        assert!(
            lowered.blocks.iter().all(|(_, b)| b.is_set()),
            "There should not be any unset flat blocks"
        );
    }
    let diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap_or_default();
    let formatted_lowering_diagnostics = diagnostics.format(db);
    let combined_diagnostics = format!("{semantic_diagnostics}\n{formatted_lowering_diagnostics}");
    let error = verify_diagnostics_expectation(args, &combined_diagnostics);
    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("semantic_diagnostics".into(), semantic_diagnostics),
            ("lowering_diagnostics".into(), formatted_lowering_diagnostics),
            ("lowering_flat".into(), formatted_lowered(db, lowered.ok())),
        ]),
        error,
    }
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
    .span_in_file(db);

    let location = LocationId::from_stable_location(db, test_expr.function_id.stable_location(db))
        .with_auto_generation_note(db, "withdraw_gas")
        .with_note(
            db,
            DiagnosticNote::with_location("Adding destructor for".to_string(), expr_location),
        )
        .long(db);

    assert_eq!(
        format!("{:?}", location.debug(db)),
        indoc::indoc! {"
lib.cairo:1:1-3:4
  #[target_function] fn test_func() { let mut a = 5; {
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
        location: location.clone(),
        kind: LoweringDiagnosticKind::CannotInlineFunctionThatMightCallItself,
    });

    assert_eq!(
        builder.build().format(db),
        indoc::indoc! {"
error[E3005]: Cannot inline a function that might call itself.
 --> lib.cairo:1:1-3:4
  #[target_function] fn test_func() { let mut a = 5; {
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
    let type_aliases = test_module.module_id.module_data(db).unwrap().type_aliases(db);
    assert_eq!(type_aliases.len(), type_to_size.len());
    let alias_expected_size = UnorderedHashMap::<_, _>::from_iter(
        type_to_size.iter().enumerate().map(|(i, (_, size))| (format!("T{i}"), *size)),
    );
    for (alias_id, alias) in type_aliases.iter() {
        let ty = db.module_type_alias_resolved_type(*alias_id).unwrap();
        let size = db.type_size(ty);
        let alias_name = alias.name(db).text(db).long(db).as_str();
        let expected_size = alias_expected_size[alias_name];
        assert_eq!(size, expected_size, "Wrong size for type alias `{}`", ty.format(db));
    }
}

/// Formats `signature` as a single line:
/// `(ty1, ty2) -> return_type refs(ty1, ty2)`
///
/// The `refs` clause is omitted when there are no extra returns.
fn format_signature(
    db: &dyn salsa::Database,
    signature: &Signature<'_>,
    module_id: ModuleId<'_>,
) -> String {
    let ty_of = |ty: &semantic::TypeId<'_>| ty.contextualized_path(db, module_id);
    let params = signature.params.iter().map(ty_of).join(", ");
    let mut res = format!("({params}) -> {}", ty_of(&signature.return_type));
    if !signature.extra_rets.is_empty() {
        res += &format!(" refs({})", signature.extra_rets.iter().map(ty_of).join(", "));
    }
    res
}

/// Asserts that `signature` describes the parameters and the returned values of the blocks of
/// `lowered`, at the given stage.
fn assert_signature_matches_lowered(lowered: &Lowered<'_>, stage: LoweringStage) {
    let signature = &lowered.signature;
    let ty_of = |var_id| lowered.variables[var_id].ty;
    assert_eq!(
        signature.params,
        lowered.parameters.iter().map(|param| ty_of(*param)).collect_vec(),
        "Signature params do not match the lowering's parameters at {stage:?}."
    );
    let mut all_rets = lowered.blocks.iter().filter_map(|(_, block)| match &block.end {
        BlockEnd::Return(vars, _) => Some(vars.iter().map(|var| ty_of(var.var_id)).collect_vec()),
        _ => None,
    });
    let rets = all_rets.next().expect("Expected at least one returning block.");
    assert!(
        all_rets.all(|other| other == rets),
        "Returning blocks disagree on the returned types."
    );
    assert_eq!(
        chain!(signature.extra_rets.iter().copied(), [signature.return_type]).collect_vec(),
        rets,
        "Signature returns do not match the lowering's returned values at {stage:?}."
    );
}

cairo_lang_test_utils::test_file_test!(
    per_stage_signature,
    "src/test_data",
    {
        signature: "signature",
    },
    test_per_stage_signature,
    []
);

/// Prints the stored signature at each [LoweringStage], and cross-checks it against the
/// parameters and returned values of the lowering's blocks at that stage.
fn test_per_stage_signature(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
    let (test_function, semantic_diagnostics) = setup_test_function(db, inputs).split();
    assert_eq!(semantic_diagnostics, "");
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    let mut outputs = OrderedHashMap::default();
    for (tag, stage) in [
        ("monomorphized", LoweringStage::Monomorphized),
        ("pre_optimizations", LoweringStage::PreOptimizations),
        ("post_baseline", LoweringStage::PostBaseline),
        ("final", LoweringStage::Final),
    ] {
        let lowered = db.lowered_body(function_id, stage).unwrap();
        assert_signature_matches_lowered(lowered, stage);
        outputs
            .insert(tag.into(), format_signature(db, &lowered.signature, test_function.module_id));
    }
    TestRunnerResult::success(outputs)
}
