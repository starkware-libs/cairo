use cairo_lang_semantic::GenericArgumentId;
use cairo_lang_semantic::corelib::get_core_ty_by_name;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_sierra::extensions::OutputVarReferenceInfo;
use cairo_lang_sierra::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange,
};
use cairo_lang_sierra::ids::ConcreteLibfuncId;
use cairo_lang_utils::LookupIntern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;
use pretty_assertions::assert_eq;

use super::{LibfuncInfo, LocalVariables};
use crate::db::SierraGenGroup;
use crate::pre_sierra;
use crate::replace_ids::replace_sierra_ids;
use crate::store_variables::add_store_statements;
use crate::test_utils::{
    SierraGenDatabaseForTesting, as_var_id_vec, dummy_jump_statement, dummy_label,
    dummy_push_values, dummy_push_values_ex, dummy_return_statement, dummy_simple_branch,
    dummy_simple_statement,
};

/// Returns the [OutputVarReferenceInfo] information for a given libfunc.
/// All libfuncs inputs and outputs are felt252s, since [dummy_push_values] is currently with
/// felt252s.
fn get_lib_func_signature(db: &dyn SierraGenGroup, libfunc: ConcreteLibfuncId) -> LibfuncSignature {
    let libfunc_long_id = libfunc.lookup_intern(db);
    let felt252_ty =
        db.get_concrete_type_id(db.core_info().felt252).expect("Can't find core::felt252.");
    let array_ty = db
        .get_concrete_type_id(get_core_ty_by_name(
            db.upcast(),
            "Array".into(),
            vec![GenericArgumentId::Type(db.core_info().felt252)],
        ))
        .expect("Can't find core::Array<core::felt252>.");
    let name = libfunc_long_id.generic_id.0;
    match name.as_str() {
        "felt252_add" => {
            let vars = vec![OutputVarInfo {
                ty: felt252_ty.clone(),
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }];
            LibfuncSignature {
                param_signatures: vec![
                    ParamSignature::new(felt252_ty.clone()),
                    ParamSignature::new(felt252_ty).with_allow_const(),
                ],
                branch_signatures: vec![BranchSignature {
                    vars,
                    ap_change: SierraApChange::Known { new_vars_only: true },
                }],
                fallthrough: Some(0),
            }
        }
        "felt252_add3" => LibfuncSignature {
            param_signatures: vec![ParamSignature::new(felt252_ty.clone()).with_allow_add_const()],
            branch_signatures: vec![BranchSignature {
                vars: vec![OutputVarInfo {
                    ty: felt252_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                }],
                ap_change: SierraApChange::Known { new_vars_only: true },
            }],
            fallthrough: Some(0),
        },
        "felt252_const" => LibfuncSignature {
            param_signatures: vec![],
            branch_signatures: vec![BranchSignature {
                vars: vec![OutputVarInfo {
                    ty: felt252_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Const),
                }],
                ap_change: SierraApChange::Known { new_vars_only: true },
            }],
            fallthrough: Some(0),
        },
        "array_append" => LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(array_ty.clone()).with_allow_add_const(),
                ParamSignature::new(felt252_ty),
            ],
            branch_signatures: vec![BranchSignature {
                vars: vec![OutputVarInfo {
                    ty: array_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                }],
                ap_change: SierraApChange::Known { new_vars_only: true },
            }],
            fallthrough: Some(0),
        },
        "nope" => LibfuncSignature {
            param_signatures: vec![],
            branch_signatures: vec![BranchSignature {
                vars: vec![],
                ap_change: SierraApChange::Known { new_vars_only: true },
            }],
            fallthrough: Some(0),
        },
        "revoke_ap" => LibfuncSignature {
            param_signatures: vec![],
            branch_signatures: vec![BranchSignature {
                vars: vec![],
                ap_change: SierraApChange::Unknown,
            }],
            fallthrough: Some(0),
        },
        "function_call4" => {
            let vars: Vec<_> = (0..4)
                .map(|idx| OutputVarInfo {
                    ty: felt252_ty.clone(),
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx },
                })
                .collect();
            LibfuncSignature {
                param_signatures: vec![],
                branch_signatures: vec![BranchSignature {
                    vars,
                    ap_change: SierraApChange::Known { new_vars_only: false },
                }],
                fallthrough: Some(0),
            }
        }
        "jump" => LibfuncSignature {
            param_signatures: vec![],
            branch_signatures: vec![BranchSignature {
                vars: vec![],
                ap_change: SierraApChange::Known { new_vars_only: true },
            }],
            fallthrough: None,
        },
        "branch" => LibfuncSignature {
            param_signatures: vec![],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
            ],
            fallthrough: Some(1),
        },
        "branch_with_param" => LibfuncSignature {
            param_signatures: vec![ParamSignature::new(felt252_ty)],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
            ],
            fallthrough: Some(1),
        },
        "store_temp<felt252>" => LibfuncSignature {
            param_signatures: vec![ParamSignature {
                ty: felt252_ty.clone(),
                allow_deferred: true,
                allow_add_const: true,
                allow_const: true,
            }],
            branch_signatures: vec![BranchSignature {
                vars: vec![OutputVarInfo {
                    ty: felt252_ty,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                }],
                ap_change: SierraApChange::Known { new_vars_only: true },
            }],
            fallthrough: Some(0),
        },
        "temp_not_on_top" => LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: felt252_ty,
                // Simulate the case where the returned value is not on the top of the stack.
                ref_info: OutputVarReferenceInfo::SimpleDerefs,
            }],
            SierraApChange::Known { new_vars_only: false },
        ),
        "dup" => LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature {
                ty: felt252_ty.clone(),
                allow_deferred: true,
                allow_add_const: true,
                allow_const: true,
            }],
            vec![
                OutputVarInfo {
                    ty: felt252_ty.clone(),
                    ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                },
                OutputVarInfo {
                    ty: felt252_ty,
                    ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                },
            ],
            SierraApChange::Known { new_vars_only: true },
        ),
        "make_local" => LibfuncSignature::new_non_branch(
            vec![felt252_ty.clone()],
            vec![OutputVarInfo { ty: felt252_ty, ref_info: OutputVarReferenceInfo::NewLocalVar }],
            SierraApChange::Known { new_vars_only: true },
        ),
        _ => panic!("get_branch_signatures() is not implemented for '{name}'."),
    }
}

/// Helper function for tests of [add_store_statements].
///
/// Calls [add_store_statements] on the given `statements` and returns the result as a vector of
/// strings.
fn test_add_store_statements(
    db: &SierraGenDatabaseForTesting,
    statements: Vec<pre_sierra::StatementWithLocation>,
    local_variables: LocalVariables,
    params: &[&str],
) -> Vec<String> {
    let felt252_ty =
        db.get_concrete_type_id(db.core_info().felt252).expect("Can't find core::felt252.");
    add_store_statements(
        db,
        statements,
        &(|libfunc| LibfuncInfo { signature: get_lib_func_signature(db, libfunc) }),
        local_variables,
        &as_var_id_vec(params)
            .into_iter()
            .map(|id| cairo_lang_sierra::program::Param { id, ty: felt252_ty.clone() })
            .collect_vec(),
    )
    .iter()
    .map(|statement| replace_sierra_ids(db, statement).statement.to_string(db))
    .collect()
}

#[test]
fn store_temp_simple() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_statement(&db, "felt252_add", &["0", "1"], &["2"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_simple_statement(&db, "felt252_add", &["2", "3"], &["4"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_simple_statement(&db, "felt252_add", &["5", "4"], &["5"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_label(&db, 0),
        dummy_simple_statement(&db, "felt252_add", &["5", "6"], &["6"]),
        dummy_return_statement(&[]),
    ];

    assert_eq!(
        test_add_store_statements(
            &db,
            statements,
            LocalVariables::default(),
            &["0", "1", "3", "5", "6"]
        ),
        vec![
            "felt252_add(0, 1) -> (2)",
            "nope() -> ()",
            "store_temp<felt252>(2) -> (2)",
            "felt252_add(2, 3) -> (4)",
            "nope() -> ()",
            "store_temp<felt252>(4) -> (4)",
            "felt252_add(5, 4) -> (5)",
            "nope() -> ()",
            "label_test::test::0:",
            "store_temp<felt252>(5) -> (5)",
            "felt252_add(5, 6) -> (6)",
            "return()",
        ]
    );
}

#[test]
fn store_temp_for_branch_command() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_statement(&db, "felt252_add", &["0", "1"], &["2"]),
        dummy_simple_branch(&db, "branch_with_param", &["2"], 0),
        dummy_label(&db, 0),
        dummy_return_statement(&[]),
    ];

    assert_eq!(
        test_add_store_statements(&db, statements, LocalVariables::default(), &["0", "1"]),
        vec![
            "felt252_add(0, 1) -> (2)",
            "store_temp<felt252>(2) -> (2)",
            "branch_with_param(2) { label_test::test::0() fallthrough() }",
            "label_test::test::0:",
            "return()",
        ]
    );
}

#[test]
fn store_local_simple() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_statement(&db, "felt252_add", &["0", "1"], &["2"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        // Case I: local added instead of tempvar, when first used.
        dummy_simple_statement(&db, "felt252_add", &["2", "3"], &["4"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        // Case II: deferred computed into local before revoke_ap().
        dummy_simple_statement(&db, "revoke_ap", &[], &[]),
        dummy_simple_statement(&db, "function_call4", &[], &["5", "6", "7", "8"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        // Case III: tempvar copied into local before revoke_ap().
        dummy_simple_statement(&db, "revoke_ap", &[], &[]),
        dummy_simple_statement(&db, "store_temp<felt252>", &["9"], &["9"]),
        // Don't store as local due to a simple jump.
        dummy_jump_statement(&db, 0),
        dummy_label(&db, 0),
        dummy_simple_statement(&db, "nope", &[], &[]),
        // Case IV: tempvar copied into local before branches.
        dummy_simple_branch(&db, "branch", &[], 1),
        dummy_label(&db, 1),
        dummy_return_statement(&[]),
    ];

    assert_eq!(
        test_add_store_statements(
            &db,
            statements,
            OrderedHashMap::from_iter(vec![
                ("2".into(), "102".into()),
                ("4".into(), "104".into()),
                ("7".into(), "107".into()),
                ("9".into(), "109".into())
            ]),
            &["0", "1", "3", "9"]
        ),
        vec![
            "felt252_add(0, 1) -> (2)",
            "nope() -> ()",
            "store_local<felt252>(102, 2) -> (2)",
            "felt252_add(2, 3) -> (4)",
            "nope() -> ()",
            "store_local<felt252>(104, 4) -> (4)",
            "revoke_ap() -> ()",
            "function_call4() -> (5, 6, 7, 8)",
            "nope() -> ()",
            "store_local<felt252>(107, 7) -> (7)",
            "revoke_ap() -> ()",
            "store_temp<felt252>(9) -> (9)",
            "jump() { label_test::test::0() }",
            "label_test::test::0:",
            "nope() -> ()",
            "store_local<felt252>(109, 9) -> (9)",
            "branch() { label_test::test::1() fallthrough() }",
            "label_test::test::1:",
            "return()",
        ],
    );
}

#[test]
fn same_as_param() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_statement(&db, "felt252_add3", &["0"], &["1"]),
        dummy_simple_statement(&db, "dup", &["1"], &["2", "3"]),
        dummy_simple_statement(&db, "felt252_add3", &["2"], &["4"]),
        dummy_simple_statement(&db, "felt252_add", &["3", "4"], &["5"]),
        dummy_return_statement(&[]),
    ];

    assert_eq!(
        test_add_store_statements(&db, statements, LocalVariables::default(), &["0"]),
        vec![
            "felt252_add3(0) -> (1)",
            "dup(1) -> (2, 3)",
            "felt252_add3(2) -> (4)",
            "store_temp<felt252>(3) -> (3)",
            "store_temp<felt252>(4) -> (4)",
            "felt252_add(3, 4) -> (5)",
            "return()",
        ]
    );
}

#[test]
fn same_as_param_push_value_optimization() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_statement(&db, "store_temp<felt252>", &["0"], &["1"]),
        dummy_simple_statement(&db, "dup", &["1"], &["2", "3"]),
        dummy_push_values(&db, &[("2", "102"), ("4", "104")]),
        dummy_return_statement(&[]),
    ];

    assert_eq!(
        test_add_store_statements(&db, statements, LocalVariables::default(), &["0", "4"]),
        vec![
            "store_temp<felt252>(0) -> (1)",
            "dup(1) -> (2, 3)",
            "rename<felt252>(2) -> (102)",
            "store_temp<felt252>(4) -> (104)",
            "return()",
        ]
    );
}

/// Tests that storing the result of an if as a local variable works correctly.
///
/// For example:
///     let y = if cond { 1 } else { 2 }
///     revoke_ap()
///     // Use y.
#[test]
fn store_local_result_of_if() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_branch(&db, "branch", &[], 0),
        // If part.
        dummy_simple_statement(&db, "store_temp<felt252>", &["100"], &["100"]),
        dummy_jump_statement(&db, 1),
        // Else part.
        dummy_label(&db, 0),
        dummy_push_values(&db, &[("0", "100")]),
        // Post-if.
        dummy_label(&db, 1),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_simple_statement(&db, "revoke_ap", &[], &[]),
        dummy_return_statement(&[]),
    ];

    assert_eq!(
        test_add_store_statements(
            &db,
            statements,
            OrderedHashMap::from_iter(vec![("100".into(), "200".into()),],),
            &["0", "100"],
        ),
        vec![
            "branch() { label_test::test::0() fallthrough() }",
            "store_temp<felt252>(100) -> (100)",
            "jump() { label_test::test::1() }",
            "label_test::test::0:",
            "store_temp<felt252>(0) -> (100)",
            "label_test::test::1:",
            "nope() -> ()",
            "store_local<felt252>(200, 100) -> (100)",
            "revoke_ap() -> ()",
            "return()",
        ],
    );
}

/// Tests the behavior of the [PushValues](pre_sierra::Statement::PushValues) statement.
#[test]
fn store_temp_push_values() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_statement(&db, "felt252_add", &["0", "1"], &["2"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_simple_statement(&db, "felt252_add", &["3", "4"], &["5"]),
        dummy_simple_statement(&db, "felt252_add", &["5", "6"], &["7"]),
        dummy_simple_statement(&db, "store_temp<felt252>", &["7"], &["7"]),
        dummy_push_values(&db, &[("8", "100"), ("2", "101"), ("7", "102"), ("9", "103")]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_return_statement(&["10"]),
    ];

    assert_eq!(
        test_add_store_statements(
            &db,
            statements,
            LocalVariables::default(),
            &["0", "1", "3", "4", "6", "8", "9", "10"]
        ),
        vec![
            "felt252_add(0, 1) -> (2)",
            "nope() -> ()",
            "felt252_add(3, 4) -> (5)",
            "store_temp<felt252>(5) -> (5)",
            "felt252_add(5, 6) -> (7)",
            "store_temp<felt252>(7) -> (7)",
            "store_temp<felt252>(8) -> (100)",
            "store_temp<felt252>(2) -> (101)",
            "store_temp<felt252>(7) -> (102)",
            "store_temp<felt252>(9) -> (103)",
            "nope() -> ()",
            "return(10)",
        ]
    );
}

/// Tests the behavior of the [PushValues](pre_sierra::Statement::PushValues) statement with
/// [dup_var](pre_sierra::Statement::PushValues::dup_var).
#[test]
fn store_temp_push_values_with_dup() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_statement(&db, "felt252_add", &["0", "1"], &["2"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_push_values_ex(
            &db,
            &[
                // Deferred with dup.
                ("2", "102", true),
                // Temporary variable with dup.
                ("3", "100", true),
            ],
        ),
        dummy_return_statement(&[]),
    ];

    assert_eq!(
        test_add_store_statements(&db, statements, LocalVariables::default(), &["0", "1", "3"]),
        vec![
            "felt252_add(0, 1) -> (2)",
            "nope() -> ()",
            "store_temp<felt252>(2) -> (102)",
            "dup<felt252>(102) -> (102, 2)",
            "dup<felt252>(3) -> (3, 100)",
            "store_temp<felt252>(100) -> (100)",
            "return()",
        ]
    );
}

/// Tests the [PushValues](pre_sierra::Statement::PushValues) optimization.
#[test]
fn push_values_optimization() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_statement(&db, "function_call4", &[], &["0", "1", "2", "3"]),
        dummy_push_values(&db, &[("2", "102"), ("3", "103"), ("0", "100")]),
        dummy_push_values(&db, &[("102", "202")]),
        dummy_return_statement(&["0"]),
    ];

    assert_eq!(
        test_add_store_statements(&db, statements, LocalVariables::default(), &[]),
        vec![
            "function_call4() -> (0, 1, 2, 3)",
            "rename<felt252>(2) -> (102)",
            "rename<felt252>(3) -> (103)",
            "store_temp<felt252>(0) -> (100)",
            "store_temp<felt252>(102) -> (202)",
            "return(0)",
        ]
    );
}

/// Tests that the known stack is cleared after change to ap.
#[test]
fn push_values_clear_known_stack() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_push_values(&db, &[("0", "100")]),
        // The explicit call to store_temp() will clear the known stack.
        dummy_simple_statement(&db, "store_temp<felt252>", &["1"], &["101"]),
        dummy_push_values(&db, &[("100", "200"), ("101", "201")]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_push_values(&db, &[("200", "300"), ("201", "301")]),
        dummy_return_statement(&["0"]),
    ];

    assert_eq!(
        test_add_store_statements(&db, statements, LocalVariables::default(), &["0", "1"]),
        vec![
            "store_temp<felt252>(0) -> (100)",
            "store_temp<felt252>(1) -> (101)",
            "rename<felt252>(100) -> (200)",
            "rename<felt252>(101) -> (201)",
            "nope() -> ()",
            "rename<felt252>(200) -> (300)",
            "rename<felt252>(201) -> (301)",
            "return(0)",
        ]
    );
}

#[test]
fn push_values_temp_not_on_top() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_statement(&db, "temp_not_on_top", &[], &["0"]),
        dummy_push_values(&db, &[("0", "100")]),
        dummy_return_statement(&["0"]),
    ];

    assert_eq!(
        test_add_store_statements(&db, statements, LocalVariables::default(), &[]),
        vec!["temp_not_on_top() -> (0)", "store_temp<felt252>(0) -> (100)", "return(0)",]
    );
}

/// Tests a few consecutive invocations of [PushValues](pre_sierra::Statement::PushValues).
#[test]
fn consecutive_push_values() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_push_values(&db, &[("0", "100"), ("1", "101")]),
        dummy_push_values_ex(
            &db,
            &[
                ("100", "200", false),
                ("101", "201", true),
                ("2", "202", false),
                ("3", "203", false),
            ],
        ),
        dummy_push_values(&db, &[("101", "301"), ("202", "302"), ("203", "303"), ("4", "304")]),
        dummy_push_values(&db, &[("304", "404")]),
        dummy_return_statement(&["0"]),
    ];

    assert_eq!(
        test_add_store_statements(
            &db,
            statements,
            LocalVariables::default(),
            &["0", "1", "2", "3", "4"]
        ),
        vec![
            // First statement. Push [0] and [1].
            "store_temp<felt252>(0) -> (100)",
            "store_temp<felt252>(1) -> (101)",
            // Second statement. Reuse [100] and [101]. Push [2] and [3].
            "rename<felt252>(100) -> (200)",
            "dup<felt252>(101) -> (101, 201)",
            "store_temp<felt252>(2) -> (202)",
            "store_temp<felt252>(3) -> (203)",
            // Third statement. Reuse [101], [202] and [203]. Push [4].
            "rename<felt252>(101) -> (301)",
            "rename<felt252>(202) -> (302)",
            "rename<felt252>(203) -> (303)",
            "store_temp<felt252>(4) -> (304)",
            // Third statement. Reuse [304].
            "rename<felt252>(304) -> (404)",
            // Return.
            "return(0)",
        ]
    );
}

/// Tests a few consecutive invocations of [PushValues](pre_sierra::Statement::PushValues).
#[test]
fn push_values_after_branch_merge() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_branch(&db, "branch", &[], 0),
        dummy_push_values(&db, &[("0", "100"), ("1", "101"), ("2", "102")]),
        dummy_jump_statement(&db, 1),
        dummy_label(&db, 0),
        dummy_push_values(&db, &[("1", "101"), ("2", "102")]),
        dummy_label(&db, 1),
        dummy_push_values(&db, &[("101", "201"), ("102", "202"), ("3", "203")]),
        dummy_return_statement(&["0"]),
    ];

    assert_eq!(
        test_add_store_statements(
            &db,
            statements,
            LocalVariables::default(),
            &["0", "1", "2", "3"]
        ),
        vec![
            "branch() { label_test::test::0() fallthrough() }",
            // Push [0], [1] and [2].
            "store_temp<felt252>(0) -> (100)",
            "store_temp<felt252>(1) -> (101)",
            "store_temp<felt252>(2) -> (102)",
            "jump() { label_test::test::1() }",
            "label_test::test::0:",
            // Push [1] and [2].
            "store_temp<felt252>(1) -> (101)",
            "store_temp<felt252>(2) -> (102)",
            "label_test::test::1:",
            // Here the two branches merge and the merged stack is [1], [2].
            // Reuse [101] and [102]. Push [3].
            "rename<felt252>(101) -> (201)",
            "rename<felt252>(102) -> (202)",
            "store_temp<felt252>(3) -> (203)",
            // Return.
            "return(0)",
        ]
    );
}

/// Tests a few consecutive invocations of [PushValues](pre_sierra::Statement::PushValues).
#[test]
fn push_values_early_return() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_push_values(&db, &[("0", "100"), ("1", "101")]),
        dummy_simple_branch(&db, "branch", &[], 0),
        dummy_push_values(&db, &[("101", "201"), ("2", "202"), ("3", "203")]),
        dummy_return_statement(&["0"]),
        dummy_label(&db, 0),
        dummy_push_values(&db, &[("101", "201"), ("2", "202")]),
        dummy_return_statement(&["0"]),
    ];

    assert_eq!(
        test_add_store_statements(
            &db,
            statements,
            LocalVariables::default(),
            &["0", "1", "2", "3"]
        ),
        vec![
            // Push [0] and [1].
            "store_temp<felt252>(0) -> (100)",
            "store_temp<felt252>(1) -> (101)",
            "branch() { label_test::test::0() fallthrough() }",
            // Reuse [101]. Push [2] and [3].
            "rename<felt252>(101) -> (201)",
            "store_temp<felt252>(2) -> (202)",
            "store_temp<felt252>(3) -> (203)",
            "return(0)",
            // This is not a merge of branches because of the "return" statement.
            // The stack contains [0] and [1].
            "label_test::test::0:",
            // Reuse [101]. Push [2].
            "rename<felt252>(101) -> (201)",
            "store_temp<felt252>(2) -> (202)",
            "return(0)",
        ]
    );
}

/// Tests a few consecutive invocations of [PushValues](pre_sierra::Statement::PushValues).
#[test]
fn consecutive_const_additions() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_statement(&db, "felt252_add", &["0", "1"], &["2"]),
        dummy_simple_statement(&db, "felt252_add3", &["2"], &["3"]),
        dummy_simple_statement(&db, "felt252_add3", &["3"], &["4"]),
        dummy_simple_statement(&db, "felt252_add", &["5", "4"], &["5"]),
        dummy_simple_statement(&db, "felt252_add", &["6", "5"], &["6"]),
        dummy_simple_statement(&db, "felt252_add3", &["6"], &["7"]),
        dummy_simple_statement(&db, "felt252_add3", &["7"], &["8"]),
        dummy_push_values(&db, &[("8", "9")]),
        dummy_return_statement(&["9"]),
    ];

    assert_eq!(
        test_add_store_statements(
            &db,
            statements,
            LocalVariables::default(),
            &["0", "1", "5", "6"]
        ),
        vec![
            "felt252_add(0, 1) -> (2)",
            "store_temp<felt252>(2) -> (2)",
            "felt252_add3(2) -> (3)",
            // There is no need to add a store_temp() instruction between two `felt252_add3()`.
            "felt252_add3(3) -> (4)",
            "store_temp<felt252>(4) -> (4)",
            "felt252_add(5, 4) -> (5)",
            "store_temp<felt252>(5) -> (5)",
            "felt252_add(6, 5) -> (6)",
            "store_temp<felt252>(6) -> (6)",
            "felt252_add3(6) -> (7)",
            // There is no need to add a store_temp() instruction between two `felt252_add3()`.
            "felt252_add3(7) -> (8)",
            // Return.
            "store_temp<felt252>(8) -> (9)",
            "return(9)",
        ]
    );
}

/// Tests a few consecutive invocations of [PushValues](pre_sierra::Statement::PushValues).
#[test]
fn consecutive_const_additions_with_branch() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_statement(&db, "felt252_add", &["0", "1"], &["2"]),
        dummy_simple_statement(&db, "felt252_add3", &["2"], &["3"]),
        dummy_simple_statement(&db, "felt252_add3", &["3"], &["4"]),
        dummy_simple_branch(&db, "branch", &[], 0),
        dummy_label(&db, 0),
        dummy_push_values(&db, &[("4", "5")]),
        dummy_return_statement(&["5"]),
    ];

    assert_eq!(
        test_add_store_statements(&db, statements, LocalVariables::default(), &["0", "1"]),
        vec![
            "felt252_add(0, 1) -> (2)",
            "store_temp<felt252>(2) -> (2)",
            "felt252_add3(2) -> (3)",
            // There is no need to add a store_temp() instruction between two `felt252_add3()`.
            "felt252_add3(3) -> (4)",
            "store_temp<felt252>(4) -> (4)",
            "branch() { label_test::test::0() fallthrough() }",
            "label_test::test::0:",
            // Return.
            "rename<felt252>(4) -> (5)",
            "return(5)",
        ]
    );
}

/// Tests a few consecutive invocations of [PushValues](pre_sierra::Statement::PushValues).
#[test]
fn consecutive_appends_with_branch() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_simple_statement(&db, "array_append", &["0", "1"], &["2"]),
        dummy_simple_statement(&db, "array_append", &["2", "3"], &["4"]),
        dummy_simple_statement(&db, "array_append", &["4", "5"], &["6"]),
        dummy_simple_branch(&db, "branch", &[], 0),
        dummy_label(&db, 0),
        dummy_push_values(&db, &[("6", "7")]),
        dummy_return_statement(&["7"]),
    ];

    assert_eq!(
        test_add_store_statements(
            &db,
            statements,
            LocalVariables::default(),
            &["0", "1", "3", "5"]
        ),
        vec![
            "array_append(0, 1) -> (2)",
            "array_append(2, 3) -> (4)",
            "array_append(4, 5) -> (6)",
            "store_temp<Array<felt252>>(6) -> (6)",
            "branch() { label_test::test::0() fallthrough() }",
            "label_test::test::0:",
            // Return.
            "rename<felt252>(6) -> (7)",
            "return(7)",
        ]
    );
}

#[test]
fn push_values_with_hole() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        dummy_push_values(&db, &[("0", "100"), ("1", "101"), ("2", "102")]),
        dummy_simple_statement(&db, "make_local", &["102"], &["102"]),
        dummy_push_values(&db, &[("100", "200"), ("101", "201")]),
        dummy_return_statement(&["201"]),
    ];

    assert_eq!(
        test_add_store_statements(&db, statements, LocalVariables::default(), &["0", "1", "2"]),
        vec![
            "store_temp<felt252>(0) -> (100)",
            "store_temp<felt252>(1) -> (101)",
            "store_temp<felt252>(2) -> (102)",
            "make_local(102) -> (102)",
            "store_temp<felt252>(100) -> (200)",
            "store_temp<felt252>(101) -> (201)",
            "return(201)",
        ]
    );
}
