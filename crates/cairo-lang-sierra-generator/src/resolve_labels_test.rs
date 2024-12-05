use cairo_lang_sierra::ids::ConcreteLibfuncId;
use pretty_assertions::assert_eq;
use test_log::test;

use super::resolve_labels_and_extract_locations;
use crate::pre_sierra;
use crate::resolve_labels::LabelReplacer;
use crate::test_utils::{SierraGenDatabaseForTesting, label_id_from_usize};
use crate::utils::{jump_statement, simple_statement};

fn label(db: &mut SierraGenDatabaseForTesting, id: usize) -> pre_sierra::StatementWithLocation {
    pre_sierra::Statement::Label(pre_sierra::Label { id: label_id_from_usize(db, id) })
        .into_statement_without_location()
}

fn jump(db: &mut SierraGenDatabaseForTesting, id: usize) -> pre_sierra::StatementWithLocation {
    jump_statement(ConcreteLibfuncId::from_string("jump"), label_id_from_usize(db, id))
        .into_statement_without_location()
}

#[test]
fn test_resolve_labels() {
    let db = &mut SierraGenDatabaseForTesting::default();

    let statements: Vec<pre_sierra::StatementWithLocation> = vec![
        label(db, 7),
        label(db, 5),
        simple_statement(ConcreteLibfuncId::from_string("Instruction0"), &[], &[]),
        simple_statement(ConcreteLibfuncId::from_string("Instruction1"), &[], &[]),
        jump(db, 8),
        jump(db, 7),
        label(db, 0),
        jump(db, 7),
        jump(db, 5),
        simple_statement(ConcreteLibfuncId::from_string("Instruction2"), &[], &[]),
        jump(db, 0),
        label(db, 8),
        jump(db, 8),
        jump(db, 9),
        // Note: this label does not point to an actual instruction.
        label(db, 9),
    ];
    let label_replacer = LabelReplacer::from_statements(&statements);
    let (statements, _statements_location) =
        resolve_labels_and_extract_locations(statements, &label_replacer);
    assert_eq!(statements.iter().map(|x| format!("{x}")).collect::<Vec<String>>(), vec![
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
    ]);
}
