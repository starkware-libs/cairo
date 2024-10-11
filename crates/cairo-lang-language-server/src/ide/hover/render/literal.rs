use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::ast::{TerminalLiteralNumber, TerminalShortString, TerminalString};
use indoc::formatdoc;
use smol_str::SmolStr;
use tower_lsp::lsp_types::Hover;

use crate::ide::hover::markdown_contents;
use crate::ide::hover::range::HoverRange;
use crate::lang::db::AnalysisDatabase;

const TITLE: &str = "value of literal";
const RULER: &str = "***";

#[derive(Debug, Clone)]
pub enum Literal {
    Number(TerminalLiteralNumber),
    String(TerminalString),
    ShortString(TerminalShortString),
}

impl Literal {
    pub fn render(&self, db: &AnalysisDatabase, file_id: FileId) -> Option<Hover> {
        match self {
            Literal::Number(literal) => number(db, literal, file_id),
            Literal::String(literal) => string(db, literal, file_id),
            Literal::ShortString(literal) => short_string(db, literal, file_id),
        }
    }
}

#[tracing::instrument(level = "trace", skip_all)]
pub fn number(
    db: &AnalysisDatabase,
    literal: &TerminalLiteralNumber,
    file_id: FileId,
) -> Option<Hover> {
    let (value, type_suffix) = literal.numeric_value_and_suffix(db)?;

    let number_type = type_suffix.as_ref().map(SmolStr::as_str).unwrap_or("felt252");
    let type_path = if number_type == "felt252" { "core" } else { "core::integer" };

    let representation = String::from(formatdoc!(
        "
        {TITLE}: `{value} ({value:#x} | {value:#b})`
        {RULER}
        Type: `{type_path}::{number_type}`
        "
    ));

    Some(Hover { contents: markdown_contents(representation), range: literal.range(db, file_id) })
}

#[tracing::instrument(level = "trace", skip_all)]
pub fn string(db: &AnalysisDatabase, literal: &TerminalString, file_id: FileId) -> Option<Hover> {
    let string = literal.string_value(db)?;

    let representation = String::from(formatdoc!(
        r#"
        {TITLE}: `"{string}"`
        {RULER}
        Type: `core::byte_array::ByteArray`
        "#
    ));

    Some(Hover { contents: markdown_contents(representation), range: literal.range(db, file_id) })
}

#[tracing::instrument(level = "trace", skip_all)]
pub fn short_string(
    db: &AnalysisDatabase,
    literal: &TerminalShortString,
    file_id: FileId,
) -> Option<Hover> {
    let representation = match (literal.numeric_value(db), literal.string_value(db)) {
        (None, _) => None,
        (Some(numeric), None) => Some(String::from(formatdoc!(
            "
            {TITLE}: `{numeric:#x}`
            {RULER}
            Type: `core::felt252`
            "
        ))),
        (Some(numeric), Some(string)) => Some(String::from(formatdoc!(
            "
            {TITLE}: `'{string}' ({numeric:#x})`
            {RULER}
            Type: `core::felt252`
            "
        ))),
    }?;

    Some(Hover { contents: markdown_contents(representation), range: literal.range(db, file_id) })
}
