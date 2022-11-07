use pretty_assertions::assert_eq;
use sierra::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange,
};
use sierra::extensions::OutputVarReferenceInfo;
use sierra::ids::ConcreteLibFuncId;
use utils::ordered_hash_set::OrderedHashSet;

use crate::db::SierraGenGroup;
use crate::pre_sierra;
use crate::replace_ids::replace_sierra_ids;
use crate::store_variables::add_store_statements;
use crate::test_utils::{
    dummy_jump_statement, dummy_label, dummy_push_values, dummy_return_statement,
    dummy_simple_branch, dummy_simple_statement, SierraGenDatabaseForTesting,
};

/// Returns the [OutputVarReferenceInfo] information for a given libfunc.
/// All libfuncs inputs and outputs are felts, since [dummy_push_values] is currently with felts.
fn get_lib_func_signature(db: &dyn SierraGenGroup, libfunc: ConcreteLibFuncId) -> LibFuncSignature {
    let libfunc_long_id = db.lookup_intern_concrete_lib_func(libfunc);
    let felt_ty = db.get_concrete_type_id(db.core_felt_ty()).expect("Can't find core::felt.");
    let name = libfunc_long_id.generic_id.debug_name.unwrap();
    match name.as_str() {
        "felt_add" => {
            let vars = vec![OutputVarInfo {
                ty: felt_ty.clone(),
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }];
            LibFuncSignature {
                param_signatures: vec![
                    ParamSignature::new(felt_ty.clone()),
                    ParamSignature::new(felt_ty),
                ],
                branch_signatures: vec![BranchSignature {
                    vars,
                    ap_change: SierraApChange::NotImplemented,
                }],
                fallthrough: Some(0),
            }
        }
        "felt_add3" => LibFuncSignature {
            param_signatures: vec![ParamSignature {
                ty: felt_ty.clone(),
                allow_deferred: false,
                allow_add_const: true,
            }],
            branch_signatures: vec![BranchSignature {
                vars: vec![OutputVarInfo {
                    ty: felt_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                }],
                ap_change: SierraApChange::Known(0),
            }],
            fallthrough: Some(0),
        },
        "nope" => LibFuncSignature {
            param_signatures: vec![],
            branch_signatures: vec![BranchSignature {
                vars: vec![],
                ap_change: SierraApChange::NotImplemented,
            }],
            fallthrough: Some(0),
        },
        "function_call4" => {
            let vars: Vec<_> = (0..4)
                .map(|idx| OutputVarInfo {
                    ty: felt_ty.clone(),
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx },
                })
                .collect();
            LibFuncSignature {
                param_signatures: vec![],
                branch_signatures: vec![BranchSignature {
                    vars,
                    ap_change: SierraApChange::NotImplemented,
                }],
                fallthrough: Some(0),
            }
        }
        "jump" => LibFuncSignature {
            param_signatures: vec![],
            branch_signatures: vec![BranchSignature {
                vars: vec![],
                ap_change: SierraApChange::Known(0),
            }],
            fallthrough: None,
        },
        "branch" => LibFuncSignature {
            param_signatures: vec![],
            branch_signatures: vec![
                BranchSignature { vars: vec![], ap_change: SierraApChange::Known(0) },
                BranchSignature { vars: vec![], ap_change: SierraApChange::Known(0) },
            ],
            fallthrough: Some(1),
        },
        "store_temp" => LibFuncSignature {
            param_signatures: vec![ParamSignature {
                ty: felt_ty.clone(),
                allow_deferred: true,
                allow_add_const: true,
            }],
            branch_signatures: vec![BranchSignature {
                vars: vec![OutputVarInfo {
                    ty: felt_ty,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                }],
                ap_change: SierraApChange::NotImplemented,
            }],
            fallthrough: Some(0),
        },
        _ => panic!("get_branch_signatures() is not implemented for '{}'.", name),
    }
}

#[test]
fn store_temp_simple() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::Statement> = vec![
        dummy_simple_statement(&db, "felt_add", &["0", "1"], &["2"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_simple_statement(&db, "felt_add", &["2", "3"], &["4"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_simple_statement(&db, "felt_add", &["2", "4"], &["5"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_label(0),
        dummy_simple_statement(&db, "felt_add", &["5", "5"], &["6"]),
        dummy_return_statement(&[]),
    ];

    assert_eq!(
        add_store_statements(
            &db,
            statements,
            &(|libfunc| get_lib_func_signature(&db, libfunc)),
            OrderedHashSet::default()
        )
        .iter()
        .map(|statement| replace_sierra_ids(&db, statement).to_string())
        .collect::<Vec<String>>(),
        vec![
            "felt_add(0, 1) -> (2)",
            "nope() -> ()",
            "store_temp<felt>(2) -> (2)",
            "felt_add(2, 3) -> (4)",
            "nope() -> ()",
            "store_temp<felt>(4) -> (4)",
            "felt_add(2, 4) -> (5)",
            "nope() -> ()",
            "store_temp<felt>(5) -> (5)",
            "label0:",
            "felt_add(5, 5) -> (6)",
            "return()",
        ]
    );
}

#[test]
fn store_local_simple() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::Statement> = vec![
        dummy_simple_statement(&db, "felt_add", &["0", "1"], &["2"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_simple_statement(&db, "felt_add", &["2", "3"], &["4"]),
        dummy_return_statement(&[]),
    ];

    assert_eq!(
        add_store_statements(
            &db,
            statements,
            &(|libfunc| get_lib_func_signature(&db, libfunc)),
            OrderedHashSet::from_iter(vec!["2".into()])
        )
        .iter()
        .map(|statement| replace_sierra_ids(&db, statement).to_string())
        .collect::<Vec<String>>(),
        vec![
            "felt_add(0, 1) -> (2)",
            "nope() -> ()",
            "store_local<felt>(2) -> (2)",
            "felt_add(2, 3) -> (4)",
            "return()",
        ]
    );
}

/// Tests the behavior of the [PushValues](pre_sierra::Statement::PushValues) statement.
#[test]
fn store_temp_push_values() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::Statement> = vec![
        dummy_simple_statement(&db, "felt_add", &["0", "1"], &["2"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_simple_statement(&db, "felt_add", &["3", "4"], &["5"]),
        dummy_simple_statement(&db, "felt_add", &["5", "5"], &["6"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_push_values(&db, &[("5", "100"), ("2", "101"), ("6", "102"), ("6", "103")]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_return_statement(&["6"]),
    ];

    assert_eq!(
        add_store_statements(
            &db,
            statements,
            &(|libfunc| get_lib_func_signature(&db, libfunc)),
            OrderedHashSet::default()
        )
        .iter()
        .map(|statement| replace_sierra_ids(&db, statement).to_string())
        .collect::<Vec<String>>(),
        vec![
            "felt_add(0, 1) -> (2)",
            "nope() -> ()",
            "felt_add(3, 4) -> (5)",
            "store_temp<felt>(5) -> (5)",
            "felt_add(5, 5) -> (6)",
            "nope() -> ()",
            "store_temp<felt>(5) -> (100)",
            "store_temp<felt>(2) -> (2)",
            "rename<felt>(2) -> (101)",
            "store_temp<felt>(6) -> (6)",
            "rename<felt>(6) -> (102)",
            "store_temp<felt>(6) -> (103)",
            "nope() -> ()",
            "return(6)",
        ]
    );
}

/// Tests the [PushValues](pre_sierra::Statement::PushValues) optimization.
#[test]
fn push_values_optimization() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::Statement> = vec![
        dummy_simple_statement(&db, "function_call4", &[], &["0", "1", "2", "3"]),
        dummy_push_values(&db, &[("2", "102"), ("3", "103"), ("0", "100")]),
        dummy_push_values(&db, &[("102", "202")]),
        dummy_return_statement(&["0"]),
    ];

    assert_eq!(
        add_store_statements(
            &db,
            statements,
            &(|libfunc| get_lib_func_signature(&db, libfunc)),
            OrderedHashSet::default()
        )
        .iter()
        .map(|statement| replace_sierra_ids(&db, statement).to_string())
        .collect::<Vec<String>>(),
        vec![
            "function_call4() -> (0, 1, 2, 3)",
            "rename<felt>(2) -> (102)",
            "rename<felt>(3) -> (103)",
            "store_temp<felt>(0) -> (100)",
            "store_temp<felt>(102) -> (202)",
            "return(0)",
        ]
    );
}

/// Tests a few consecutive invocations of [PushValues](pre_sierra::Statement::PushValues).
#[test]
fn consecutive_push_values() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::Statement> = vec![
        dummy_push_values(&db, &[("0", "100"), ("1", "101")]),
        dummy_push_values(&db, &[("100", "200"), ("101", "201"), ("2", "202"), ("3", "203")]),
        dummy_push_values(&db, &[("101", "301"), ("202", "302"), ("203", "303"), ("4", "304")]),
        dummy_push_values(&db, &[("304", "404")]),
        dummy_return_statement(&["0"]),
    ];

    assert_eq!(
        add_store_statements(
            &db,
            statements,
            &(|libfunc| get_lib_func_signature(&db, libfunc)),
            OrderedHashSet::default()
        )
        .iter()
        .map(|statement| replace_sierra_ids(&db, statement).to_string())
        .collect::<Vec<String>>(),
        vec![
            // First statement. Push [0] and [1].
            "store_temp<felt>(0) -> (100)",
            "store_temp<felt>(1) -> (101)",
            // Second statement. Reuse [100] and [101]. Push [2] and [3].
            "rename<felt>(100) -> (200)",
            "rename<felt>(101) -> (201)",
            "store_temp<felt>(2) -> (202)",
            "store_temp<felt>(3) -> (203)",
            // Third statement. Reuse [101], [202] and [203]. Push [4].
            "rename<felt>(101) -> (301)",
            "rename<felt>(202) -> (302)",
            "rename<felt>(203) -> (303)",
            "store_temp<felt>(4) -> (304)",
            // Third statement. Reuse [304].
            "rename<felt>(304) -> (404)",
            // Return.
            "return(0)",
        ]
    );
}

/// Tests a few consecutive invocations of [PushValues](pre_sierra::Statement::PushValues).
#[test]
fn push_values_after_branch_merge() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::Statement> = vec![
        dummy_simple_branch(&db, "branch", &[], 0),
        dummy_push_values(&db, &[("0", "100"), ("1", "101"), ("2", "102")]),
        dummy_jump_statement(&db, 1),
        dummy_label(0),
        dummy_push_values(&db, &[("1", "101"), ("2", "102")]),
        dummy_label(1),
        dummy_push_values(&db, &[("101", "201"), ("102", "202"), ("3", "203")]),
        dummy_return_statement(&["0"]),
    ];

    assert_eq!(
        add_store_statements(
            &db,
            statements,
            &(|libfunc| get_lib_func_signature(&db, libfunc)),
            OrderedHashSet::default()
        )
        .iter()
        .map(|statement| replace_sierra_ids(&db, statement).to_string())
        .collect::<Vec<String>>(),
        vec![
            "branch() { label0() fallthrough() }",
            // Push [0], [1] and [2].
            "store_temp<felt>(0) -> (100)",
            "store_temp<felt>(1) -> (101)",
            "store_temp<felt>(2) -> (102)",
            "jump() { label1() }",
            "label0:",
            // Push [1] and [2].
            "store_temp<felt>(1) -> (101)",
            "store_temp<felt>(2) -> (102)",
            "label1:",
            // Here the two branches merge and the merged stack is [1], [2].
            // Reuse [101] and [102]. Push [3].
            "rename<felt>(101) -> (201)",
            "rename<felt>(102) -> (202)",
            "store_temp<felt>(3) -> (203)",
            // Return.
            "return(0)",
        ]
    );
}

/// Tests a few consecutive invocations of [PushValues](pre_sierra::Statement::PushValues).
#[test]
fn push_values_early_return() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::Statement> = vec![
        dummy_push_values(&db, &[("0", "100"), ("1", "101")]),
        dummy_simple_branch(&db, "branch", &[], 0),
        dummy_push_values(&db, &[("101", "201"), ("2", "202"), ("3", "203")]),
        dummy_return_statement(&["0"]),
        dummy_label(0),
        dummy_push_values(&db, &[("101", "201"), ("2", "202")]),
        dummy_return_statement(&["0"]),
    ];

    assert_eq!(
        add_store_statements(
            &db,
            statements,
            &(|libfunc| get_lib_func_signature(&db, libfunc)),
            OrderedHashSet::default()
        )
        .iter()
        .map(|statement| replace_sierra_ids(&db, statement).to_string())
        .collect::<Vec<String>>(),
        vec![
            // Push [0] and [1].
            "store_temp<felt>(0) -> (100)",
            "store_temp<felt>(1) -> (101)",
            "branch() { label0() fallthrough() }",
            // Reuse [101]. Push [2] and [3].
            "rename<felt>(101) -> (201)",
            "store_temp<felt>(2) -> (202)",
            "store_temp<felt>(3) -> (203)",
            "return(0)",
            // This is not a merge of branches because of the "return" statement.
            // The stack contains [0] and [1].
            "label0:",
            // Reuse [101]. Push [2].
            "rename<felt>(101) -> (201)",
            "store_temp<felt>(2) -> (202)",
            "return(0)",
        ]
    );
}

/// Tests a few consecutive invocations of [PushValues](pre_sierra::Statement::PushValues).
#[test]
fn store_temp_gets_deferred() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::Statement> = vec![
        dummy_simple_statement(&db, "felt_add", &["0", "1"], &["2"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_simple_statement(&db, "store_temp", &["2"], &["3"]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_simple_statement(&db, "felt_add", &["2", "2"], &["4"]),
        dummy_simple_statement(&db, "felt_add", &["3", "3"], &["6"]),
        dummy_return_statement(&["0"]),
    ];

    assert_eq!(
        add_store_statements(
            &db,
            statements,
            &(|libfunc| get_lib_func_signature(&db, libfunc)),
            OrderedHashSet::default()
        )
        .iter()
        .map(|statement| replace_sierra_ids(&db, statement).to_string())
        .collect::<Vec<String>>(),
        vec![
            "felt_add(0, 1) -> (2)",
            "nope() -> ()",
            // Explicit call to store_temp() is not preceded by an implicit store_temp().
            "store_temp(2) -> (3)",
            "nope() -> ()",
            // Since var 2 is still deferred an implicit store_temp() is added before felt_add().
            "store_temp<felt>(2) -> (2)",
            "felt_add(2, 2) -> (4)",
            // Var 3 is already on the stack.
            "felt_add(3, 3) -> (6)",
            // Return.
            "return(0)",
        ]
    );
}

/// Tests a few consecutive invocations of [PushValues](pre_sierra::Statement::PushValues).
#[test]
fn consecutive_const_additions() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::Statement> = vec![
        dummy_simple_statement(&db, "felt_add", &["0", "1"], &["2"]),
        dummy_simple_statement(&db, "felt_add3", &["2"], &["3"]),
        dummy_simple_statement(&db, "felt_add3", &["3"], &["4"]),
        dummy_simple_statement(&db, "felt_add", &["0", "4"], &["5"]),
        dummy_simple_statement(&db, "felt_add", &["0", "5"], &["6"]),
        dummy_simple_statement(&db, "felt_add3", &["6"], &["7"]),
        dummy_simple_statement(&db, "felt_add3", &["7"], &["8"]),
        dummy_push_values(&db, &[("8", "9")]),
        dummy_return_statement(&["9"]),
    ];

    assert_eq!(
        add_store_statements(
            &db,
            statements,
            &(|libfunc| get_lib_func_signature(&db, libfunc)),
            OrderedHashSet::default()
        )
        .iter()
        .map(|statement| replace_sierra_ids(&db, statement).to_string())
        .collect::<Vec<String>>(),
        vec![
            "felt_add(0, 1) -> (2)",
            "store_temp<felt>(2) -> (2)",
            "felt_add3(2) -> (3)",
            // There is no need to add a store_temp() instruction between two `felt_add3()`.
            "felt_add3(3) -> (4)",
            "store_temp<felt>(4) -> (4)",
            "felt_add(0, 4) -> (5)",
            "store_temp<felt>(5) -> (5)",
            "felt_add(0, 5) -> (6)",
            "store_temp<felt>(6) -> (6)",
            "felt_add3(6) -> (7)",
            // There is no need to add a store_temp() instruction between two `felt_add3()`.
            "felt_add3(7) -> (8)",
            // Return.
            "store_temp<felt>(8) -> (8)",
            "rename<felt>(8) -> (9)",
            "return(9)",
        ]
    );
}
