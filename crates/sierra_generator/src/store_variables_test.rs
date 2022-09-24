use pretty_assertions::assert_eq;
use sierra::extensions::lib_func::{OutputBranchInfo, OutputVarInfo};
use sierra::extensions::OutputVarReferenceInfo;
use sierra::ids::{ConcreteLibFuncId, ConcreteTypeId};

use crate::db::SierraGenGroup;
use crate::pre_sierra;
use crate::store_variables::add_store_statements;
use crate::test_utils::{
    dummy_label, dummy_push_values, dummy_return_statement, dummy_simple_statement,
    replace_libfunc_ids, SierraGenDatabaseForTesting,
};

/// Returns the [OutputVarReferenceInfo] information for a given libfunc.
fn get_output_info(db: &dyn SierraGenGroup, libfunc: ConcreteLibFuncId) -> Vec<OutputBranchInfo> {
    let libfunc_long_id = db.lookup_intern_concrete_lib_func(libfunc);
    let dummy_type = ConcreteTypeId::from_usize(0);
    let name = libfunc_long_id.generic_id.debug_name.unwrap();
    let single_branch = match name {
        x if x == "felt_add" => {
            vec![OutputVarInfo { ty: dummy_type, ref_info: OutputVarReferenceInfo::Deferred }]
        }
        x if x == "nope" => vec![],
        _ => panic!("get_signature() is not implemented for '{}'.", name),
    };
    vec![OutputBranchInfo { vars: single_branch }]
}

#[test]
fn store_temp_simple() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::Statement> = vec![
        dummy_simple_statement(&db, "felt_add", &[0, 1], &[2]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_simple_statement(&db, "felt_add", &[2, 3], &[4]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_simple_statement(&db, "felt_add", &[2, 4], &[5]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_label(0),
        dummy_simple_statement(&db, "felt_add", &[5, 5], &[6]),
        dummy_return_statement(&[]),
    ];

    assert_eq!(
        add_store_statements(&db, statements, &(|libfunc| get_output_info(&db, libfunc)))
            .iter()
            .map(|statement| replace_libfunc_ids(&db, statement).to_string())
            .collect::<Vec<String>>(),
        vec![
            "felt_add([0], [1]) -> ([2])",
            "nope() -> ()",
            "felt_add([2], [3]) -> ([4])",
            "nope() -> ()",
            "felt_add([2], [4]) -> ([5])",
            "nope() -> ()",
            "label0:",
            "felt_add([5], [5]) -> ([6])",
            "return()",
        ]
    );
}

/// Tests the behavior of the [PushValues](pre_sierra::Statement::PushValues) statement.
#[test]
fn store_temp_push_values() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::Statement> = vec![
        dummy_simple_statement(&db, "felt_add", &[0, 1], &[2]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_simple_statement(&db, "felt_add", &[3, 4], &[5]),
        dummy_simple_statement(&db, "felt_add", &[5, 5], &[6]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_push_values(&db, &[(5, 100), (2, 101), (6, 102), (6, 103)]),
        dummy_simple_statement(&db, "nope", &[], &[]),
        dummy_return_statement(&[6]),
    ];

    assert_eq!(
        add_store_statements(&db, statements, &(|libfunc| get_output_info(&db, libfunc)))
            .iter()
            .map(|statement| replace_libfunc_ids(&db, statement).to_string())
            .collect::<Vec<String>>(),
        vec![
            "felt_add([0], [1]) -> ([2])",
            "nope() -> ()",
            "felt_add([3], [4]) -> ([5])",
            "felt_add([5], [5]) -> ([6])",
            "nope() -> ()",
            "store_temp<[0]>([5]) -> ([100])",
            "store_temp<[0]>([2]) -> ([101])",
            "store_temp<[0]>([6]) -> ([102])",
            "store_temp<[0]>([6]) -> ([103])",
            "nope() -> ()",
            "return([6])",
        ]
    );
}
