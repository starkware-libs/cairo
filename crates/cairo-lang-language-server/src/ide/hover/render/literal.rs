use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::ast::{TerminalLiteralNumber, TerminalShortString, TerminalString};
use indoc::formatdoc;
use smol_str::SmolStr;
use tower_lsp::lsp_types::Hover;

use crate::ide::hover::markdown_contents;
use crate::ide::hover::range::HoverRange;
use crate::lang::db::AnalysisDatabase;

const RULE: &str = "---";

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
        ```cairo
        Literal: {value} ({value:#x})
        ```
        {RULE}
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
        ```cairo
        Literal: "{string}"
        ```
        {RULE}
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
            ```cairo
            Literal: {numeric:#x}
            ```
            {RULE}
            Type: `core::felt252`
            "
        ))),
        (Some(numeric), Some(string)) => Some(String::from(formatdoc!(
            "
            ```cairo
            Literal: '{string}' ({numeric:#x})
            ```
            {RULE}
            Type: `core::felt252`
            "
        ))),
    }?;

    Some(Hover { contents: markdown_contents(representation), range: literal.range(db, file_id) })
}
