use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::ast::{TerminalLiteralNumber, TerminalShortString, TerminalString};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::Upcast;
use indoc::formatdoc;
use lsp_types::Hover;
use smol_str::SmolStr;

use crate::ide::hover::markdown_contents;
use crate::lang::db::AnalysisDatabase;
use crate::lang::lsp::ToLsp;

const TITLE: &str = "value of literal";
const RULER: &str = "***";

/// Narrow down [`SyntaxNode`] to [`TerminalLiteralNumber`], [`TermialString`] or
/// [`TerminalShortString`] if it represents some literal
/// and render a hover containing its value and type, return None otherwise.
#[tracing::instrument(level = "trace", skip_all)]
pub fn literal(db: &AnalysisDatabase, node: &SyntaxNode, file_id: FileId) -> Option<Hover> {
    let syntax_db = db.upcast();

    match node.kind(syntax_db) {
        SyntaxKind::TokenLiteralNumber => {
            let literal = TerminalLiteralNumber::from_syntax_node(syntax_db, node.parent()?);
            number(db, &literal, file_id)
        }
        SyntaxKind::TokenString => {
            let literal = TerminalString::from_syntax_node(syntax_db, node.parent()?);
            string(db, &literal, file_id)
        }
        SyntaxKind::TerminalShortString => {
            let literal = TerminalShortString::from_syntax_node(syntax_db, node.parent()?);
            short_string(db, &literal, file_id)
        }
        _ => None,
    }
}

/// Format the number literal writing its decimal, hexadecimal and binary value and type.
fn number(
    db: &AnalysisDatabase,
    literal: &TerminalLiteralNumber,
    file_id: FileId,
) -> Option<Hover> {
    let (value, type_suffix) = literal.numeric_value_and_suffix(db)?;

    let number_type = type_suffix.as_ref().map(SmolStr::as_str).unwrap_or("felt252");
    let type_path = if number_type == "felt252" { "core" } else { "core::integer" };

    let representation = formatdoc!(
        "
        {TITLE}: `{value} ({value:#x} | {value:#b})`
        {RULER}
        Type: `{type_path}::{number_type}`
        "
    );

    Some(Hover {
        contents: markdown_contents(representation),
        range: literal
            .as_syntax_node()
            .span_without_trivia(db.upcast())
            .position_in_file(db.upcast(), file_id)
            .map(|position| position.to_lsp()),
    })
}

/// Format the number literal writing it along with the `core::byte_array::ByteArray` type.
fn string(db: &AnalysisDatabase, literal: &TerminalString, file_id: FileId) -> Option<Hover> {
    let string = literal.string_value(db)?;

    let representation = formatdoc!(
        r#"
        {TITLE}: `"{string}"`
        {RULER}
        Type: `core::byte_array::ByteArray`
        "#
    );

    Some(Hover {
        contents: markdown_contents(representation),
        range: literal
            .as_syntax_node()
            .span_without_trivia(db.upcast())
            .position_in_file(db.upcast(), file_id)
            .map(|position| position.to_lsp()),
    })
}

/// Format the short string literal writing its textual and numeric value along with the
/// `core::felt252` type.
fn short_string(
    db: &AnalysisDatabase,
    literal: &TerminalShortString,
    file_id: FileId,
) -> Option<Hover> {
    let representation = match (literal.numeric_value(db), literal.string_value(db)) {
        (None, _) => None,
        (Some(numeric), None) => Some(formatdoc!(
            "
            {TITLE}: `{numeric:#x}`
            {RULER}
            Type: `core::felt252`
            "
        )),
        (Some(numeric), Some(string)) => Some(formatdoc!(
            "
            {TITLE}: `'{string}' ({numeric:#x})`
            {RULER}
            Type: `core::felt252`
            "
        )),
    }?;

    Some(Hover {
        contents: markdown_contents(representation),
        range: literal
            .as_syntax_node()
            .span_without_trivia(db.upcast())
            .position_in_file(db.upcast(), file_id)
            .map(|position| position.to_lsp()),
    })
}
