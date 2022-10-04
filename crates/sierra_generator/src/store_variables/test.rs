use pretty_assertions::assert_eq;
use sierra::extensions::lib_func::{BranchSignature, OutputVarInfo, SierraApChange};
use sierra::extensions::OutputVarReferenceInfo;
use sierra::ids::{ConcreteLibFuncId, ConcreteTypeId};

use crate::db::SierraGenGroup;
use crate::pre_sierra;
use crate::store_variables::add_store_statements;
use crate::test_utils::{
    dummy_jump_statement, dummy_label, dummy_push_values, dummy_return_statement,
    dummy_simple_branch, dummy_simple_statement, replace_libfunc_ids, SierraGenDatabaseForTesting,
};

/// Returns the [OutputVarReferenceInfo] information for a given libfunc.
fn get_branch_signatures(
    db: &dyn SierraGenGroup,
    libfunc: ConcreteLibFuncId,
) -> Vec<BranchSignature> {
    let libfunc_long_id = db.lookup_intern_concrete_lib_func(libfunc);
    let dummy_type = ConcreteTypeId::from_usize(0);
    let name = libfunc_long_id.generic_id.debug_name.unwrap();
    match name.as_str() {
        "felt_add" => {
            let vars =
                vec![OutputVarInfo { ty: dummy_type, ref_info: OutputVarReferenceInfo::Deferred }];
            vec![BranchSignature { vars, ap_change: SierraApChange::NotImplemented }]
        }
        "nope" => {
            vec![BranchSignature { vars: vec![], ap_change: SierraApChange::NotImplemented }]
        }
        "function_call4" => {
            let vars: Vec<_> = (0..4)
                .map(|idx| OutputVarInfo {
                    ty: dummy_type.clone(),
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx },
                })
                .collect();
            vec![BranchSignature { vars, ap_change: SierraApChange::NotImplemented }]
        }
        "jump" => {
            vec![BranchSignature { vars: vec![], ap_change: SierraApChange::Known }]
        }
        "branch" => {
            vec![
                BranchSignature { vars: vec![], ap_change: SierraApChange::Known },
                BranchSignature { vars: vec![], ap_change: SierraApChange::Known },
            ]
        }
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
        add_store_statements(&db, statements, &(|libfunc| get_branch_signatures(&db, libfunc)))
            .iter()
            .map(|statement| replace_libfunc_ids(&db, statement).to_string())
            .collect::<Vec<String>>(),
        vec![
            "felt_add(0, 1) -> (2)",
            "nope() -> ()",
            "store_temp<[0]>(2) -> (2)",
            "felt_add(2, 3) -> (4)",
            "nope() -> ()",
            "store_temp<[0]>(4) -> (4)",
            "felt_add(2, 4) -> (5)",
            "nope() -> ()",
            "store_temp<[0]>(5) -> (5)",
            "label0:",
            "felt_add(5, 5) -> (6)",
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
        add_store_statements(&db, statements, &(|libfunc| get_branch_signatures(&db, libfunc)))
            .iter()
            .map(|statement| replace_libfunc_ids(&db, statement).to_string())
            .collect::<Vec<String>>(),
        vec![
            "felt_add(0, 1) -> (2)",
            "nope() -> ()",
            "felt_add(3, 4) -> (5)",
            "store_temp<[0]>(5) -> (5)",
            "felt_add(5, 5) -> (6)",
            "nope() -> ()",
            "store_temp<[0]>(5) -> (100)",
            "store_temp<[0]>(2) -> (2)",
            "rename<[0]>(2) -> (101)",
            "store_temp<[0]>(6) -> (6)",
            "rename<[0]>(6) -> (102)",
            "store_temp<[0]>(6) -> (103)",
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
        dummy_return_statement(&["0"]),
    ];

    assert_eq!(
        add_store_statements(&db, statements, &(|libfunc| get_branch_signatures(&db, libfunc)))
            .iter()
            .map(|statement| replace_libfunc_ids(&db, statement).to_string())
            .collect::<Vec<String>>(),
        vec![
            "function_call4() -> (0, 1, 2, 3)",
            "rename<[0]>(2) -> (102)",
            "rename<[0]>(3) -> (103)",
            "store_temp<[0]>(0) -> (100)",
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
        add_store_statements(&db, statements, &(|libfunc| get_branch_signatures(&db, libfunc)))
            .iter()
            .map(|statement| replace_libfunc_ids(&db, statement).to_string())
            .collect::<Vec<String>>(),
        vec![
            // First statement. Push [0] and [1].
            "store_temp<[0]>(0) -> (100)",
            "store_temp<[0]>(1) -> (101)",
            // Second statement. Reuse [100] and [101]. Push [2] and [3].
            "rename<[0]>(100) -> (200)",
            "rename<[0]>(101) -> (201)",
            "store_temp<[0]>(2) -> (202)",
            "store_temp<[0]>(3) -> (203)",
            // Third statement. Reuse [101], [202] and [203]. Push [4].
            "rename<[0]>(101) -> (301)",
            "rename<[0]>(202) -> (302)",
            "rename<[0]>(203) -> (303)",
            "store_temp<[0]>(4) -> (304)",
            // Third statement. Reuse [304].
            "rename<[0]>(304) -> (404)",
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
        add_store_statements(&db, statements, &(|libfunc| get_branch_signatures(&db, libfunc)))
            .iter()
            .map(|statement| replace_libfunc_ids(&db, statement).to_string())
            .collect::<Vec<String>>(),
        vec![
            "branch() { label0() fallthrough() }",
            // Push [0], [1] and [2].
            "store_temp<[0]>(0) -> (100)",
            "store_temp<[0]>(1) -> (101)",
            "store_temp<[0]>(2) -> (102)",
            "jump() { label1() }",
            "label0:",
            // Push [1] and [2].
            "store_temp<[0]>(1) -> (101)",
            "store_temp<[0]>(2) -> (102)",
            "label1:",
            // Here the two branches merge and the merged stack is [1], [2].
            // Reuse [101] and [102]. Push [3].
            "rename<[0]>(101) -> (201)",
            "rename<[0]>(102) -> (202)",
            "store_temp<[0]>(3) -> (203)",
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
        add_store_statements(&db, statements, &(|libfunc| get_branch_signatures(&db, libfunc)))
            .iter()
            .map(|statement| replace_libfunc_ids(&db, statement).to_string())
            .collect::<Vec<String>>(),
        vec![
            // Push [0] and [1].
            "store_temp<[0]>(0) -> (100)",
            "store_temp<[0]>(1) -> (101)",
            "branch() { label0() fallthrough() }",
            // Reuse [101]. Push [2] and [3].
            "rename<[0]>(101) -> (201)",
            "store_temp<[0]>(2) -> (202)",
            "store_temp<[0]>(3) -> (203)",
            "return(0)",
            // This is not a merge of branches because of the "return" statement.
            // The stack contains [0] and [1].
            "label0:",
            // Reuse [101]. Push [2].
            "rename<[0]>(101) -> (201)",
            "store_temp<[0]>(2) -> (202)",
            "return(0)",
        ]
    );
}
