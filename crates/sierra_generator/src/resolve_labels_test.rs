use pretty_assertions::assert_eq;

use super::resolve_labels;
use crate::pre_sierra;
use crate::utils::{jump_statement, simple_statement};

fn label(id: usize) -> pre_sierra::Statement {
    pre_sierra::Statement::Label(pre_sierra::Label { id: pre_sierra::LabelId::new(id) })
}

fn jump(id: usize) -> pre_sierra::Statement {
    jump_statement(pre_sierra::LabelId::new(id))
}

#[test]
fn test_resolve_labels() {
    let statements: Vec<pre_sierra::Statement> = vec![
        label(7),
        label(5),
        simple_statement("Instruction0", &vec![], &vec![]),
        simple_statement("Instruction1", &vec![], &vec![]),
        jump(8),
        jump(7),
        label(0),
        jump(7),
        jump(5),
        simple_statement("Instruction2", &vec![], &vec![]),
        jump(0),
        label(8),
        jump(8),
        jump(9),
        // Note: this label does not point to an actual instruction.
        label(9),
    ];
    assert_eq!(
        resolve_labels(&statements).iter().map(|x| format!("{}", x)).collect::<Vec<String>>(),
        vec![
            // labels 7 and 5 (instruction index 0).
            "Instruction0() -> ()",
            "Instruction1() -> ()",
            "jump() { 8() }",
            "jump() { 0() }",
            // label 0 (instruction index 5).
            "jump() { 0() }",
            "jump() { 0() }",
            "Instruction2() -> ()",
            "jump() { 4() }",
            // label 8 (instruction index 8).
            "jump() { 8() }",
            "jump() { 10() }",
            // label 9 (instruction index 10).
        ]
    );
}
