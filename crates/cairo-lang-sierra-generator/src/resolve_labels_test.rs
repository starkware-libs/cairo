use cairo_lang_sierra::ids::ConcreteLibfuncId;
use pretty_assertions::assert_eq;
use test_log::test;

use super::resolve_labels;
use crate::pre_sierra;
use crate::resolve_labels::LabelReplacer;
use crate::test_utils::{label_id_from_usize, SierraGenDatabaseForTesting};
use crate::utils::{jump_statement, simple_statement};

#[test]
fn test_resolve_labels() {
    let db_val = SierraGenDatabaseForTesting::default();
    let db = &db_val;
    let label =
        |id| pre_sierra::Statement::Label(pre_sierra::Label { id: label_id_from_usize(db, id) });
    let jump =
        |id| jump_statement(ConcreteLibfuncId::from_string("jump"), label_id_from_usize(db, id));

    let statements: Vec<pre_sierra::Statement> = vec![
        label(7),
        label(5),
        simple_statement(ConcreteLibfuncId::from_string("Instruction0"), &[], &[]),
        simple_statement(ConcreteLibfuncId::from_string("Instruction1"), &[], &[]),
        jump(8),
        jump(7),
        label(0),
        jump(7),
        jump(5),
        simple_statement(ConcreteLibfuncId::from_string("Instruction2"), &[], &[]),
        jump(0),
        label(8),
        jump(8),
        jump(9),
        // Note: this label does not point to an actual instruction.
        label(9),
    ];
    let label_replacer = LabelReplacer::from_statements(&statements);
    assert_eq!(
        resolve_labels(statements, &label_replacer)
            .iter()
            .map(|x| format!("{x}"))
            .collect::<Vec<String>>(),
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
