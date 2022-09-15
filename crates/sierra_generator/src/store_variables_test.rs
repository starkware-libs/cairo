use pretty_assertions::assert_eq;
use salsa::{InternId, InternKey};
use sierra::extensions::OutputVarReferenceInfo;
use sierra::ids::ConcreteLibFuncId;

use crate::pre_sierra;
use crate::store_variables::add_store_statements;

fn label(id: usize) -> pre_sierra::Statement {
    pre_sierra::Statement::Label(pre_sierra::Label {
        id: pre_sierra::LabelId::from_intern_id(InternId::from(id)),
    })
}

fn simple_statement(name: &str, inputs: &[usize], outputs: &[usize]) -> pre_sierra::Statement {
    let inputs_vec: Vec<sierra::ids::VarId> =
        inputs.iter().map(|x| sierra::ids::VarId::from_usize(*x)).collect();
    let outputs_vec: Vec<sierra::ids::VarId> =
        outputs.iter().map(|x| sierra::ids::VarId::from_usize(*x)).collect();
    crate::utils::simple_statement(ConcreteLibFuncId::from_string(name), &inputs_vec, &outputs_vec)
}

fn return_statement(args: &[usize]) -> pre_sierra::Statement {
    let args_vec: Vec<sierra::ids::VarId> =
        args.iter().map(|x| sierra::ids::VarId::from_usize(*x)).collect();
    crate::utils::return_statement(args_vec)
}

fn get_signature(libfunc: ConcreteLibFuncId) -> Vec<OutputVarReferenceInfo> {
    match libfunc.debug_name.clone().unwrap() {
        x if x == "felt_add" => vec![OutputVarReferenceInfo::Deferred],
        x if x == "nope" => vec![],
        _ => panic!("get_signature() is not implemented for '{}'.", libfunc.debug_name.unwrap()),
    }
}

#[test]
fn store_temp_simple() {
    let statements: Vec<pre_sierra::Statement> = vec![
        simple_statement("felt_add", &[0, 1], &[2]),
        simple_statement("nope", &[], &[]),
        simple_statement("felt_add", &[2, 3], &[4]),
        simple_statement("nope", &[], &[]),
        simple_statement("felt_add", &[2, 4], &[5]),
        simple_statement("nope", &[], &[]),
        label(0),
        simple_statement("felt_add", &[5, 5], &[6]),
        return_statement(&[]),
    ];
    assert_eq!(
        add_store_statements(statements, &get_signature)
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
