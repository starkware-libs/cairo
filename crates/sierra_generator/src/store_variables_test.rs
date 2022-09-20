use pretty_assertions::assert_eq;
use sierra::extensions::lib_func::BranchReferenceInfo;
use sierra::extensions::OutputVarReferenceInfo;
use sierra::ids::ConcreteLibFuncId;

use crate::pre_sierra;
use crate::store_variables::add_store_statements;
use crate::test_utils::{
    dummy_label, dummy_return_statement, dummy_simple_statement, SierraGenDatabaseForTesting,
};

/// Returns the [OutputVarReferenceInfo] information for a given libfunc.
fn get_output_info(libfunc: ConcreteLibFuncId) -> Vec<BranchReferenceInfo> {
    let single_branch = match libfunc.debug_name.clone().unwrap() {
        x if x == "felt_add" => vec![OutputVarReferenceInfo::Deferred],
        x if x == "nope" => vec![],
        _ => panic!("get_signature() is not implemented for '{}'.", libfunc.debug_name.unwrap()),
    };
    vec![BranchReferenceInfo(single_branch)]
}

#[test]
fn store_temp_simple() {
    let db = SierraGenDatabaseForTesting::default();
    let statements: Vec<pre_sierra::Statement> = vec![
        dummy_simple_statement("felt_add", &[0, 1], &[2]),
        dummy_simple_statement("nope", &[], &[]),
        dummy_simple_statement("felt_add", &[2, 3], &[4]),
        dummy_simple_statement("nope", &[], &[]),
        dummy_simple_statement("felt_add", &[2, 4], &[5]),
        dummy_simple_statement("nope", &[], &[]),
        dummy_label(0),
        dummy_simple_statement("felt_add", &[5, 5], &[6]),
        dummy_return_statement(&[]),
    ];
    assert_eq!(
        add_store_statements(&db, statements, &get_output_info)
            .iter()
            .map(|x| format!("{}", x))
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
