use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_formatter::{get_formatted_file, FormatterConfig};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_utils::Upcast;
use tower_lsp::lsp_types::{DocumentFormattingParams, Position, Range, TextEdit};
use tracing::error;

use crate::lang::lsp::LsProtoGroup;

/// Format a whole document.
#[tracing::instrument(
    level = "debug",
    skip_all,
    fields(uri = %params.text_document.uri)
)]
pub fn format(params: DocumentFormattingParams, db: &RootDatabase) -> Option<Vec<TextEdit>> {
    let file_uri = params.text_document.uri;
    let file = db.file_for_url(&file_uri);

    let Ok(node) = db.file_syntax(file) else {
        error!("formatting failed: file '{file_uri}' does not exist");
        return None;
    };

    if db.file_syntax_diagnostics(file).check_error_free().is_err() {
        error!("formatting failed: cannot properly parse '{file_uri}' exist");
        return None;
    }

    let new_text = get_formatted_file(db.upcast(), &node, FormatterConfig::default());

    let Some(file_summary) = db.file_summary(file) else {
        error!("formatting failed: cannot get summary for file '{file_uri}'");
        return None;
    };

    let Ok(old_line_count) = file_summary.line_count().try_into() else {
        error!("formatting failed: line count out of bound in file '{file_uri}'");
        return None;
    };

    Some(vec![TextEdit {
        range: Range {
            start: Position { line: 0, character: 0 },
            end: Position { line: old_line_count, character: 0 },
        },
        new_text,
    }])
}
