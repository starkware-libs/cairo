use cairo_lang_diagnostics::{DiagnosticEntry, DiagnosticLocation, Diagnostics, Severity};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_utils::Upcast;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString, Range,
};

use crate::lang::lsp::{LsProtoGroup, ToLsp};

/// Converts internal diagnostics to LSP format.
#[tracing::instrument(level = "trace", skip_all)]
pub fn map_cairo_diagnostics_to_lsp<T: DiagnosticEntry>(
    db: &T::DbType,
    diags: &mut Vec<Diagnostic>,
    diagnostics: &Diagnostics<T>,
) {
    for diagnostic in diagnostics.get_all() {
        let mut message = diagnostic.format(db);
        let mut related_information = vec![];
        for note in diagnostic.notes(db) {
            if let Some(location) = &note.location {
                related_information.push(DiagnosticRelatedInformation {
                    location: Location {
                        uri: db.url_for_file(location.file_id),
                        range: get_range(db.upcast(), location),
                    },
                    message: note.text.clone(),
                });
            } else {
                message += &format!("\nnote: {}", note.text);
            }
        }

        diags.push(Diagnostic {
            range: get_range(db.upcast(), &diagnostic.location(db)),
            message,
            related_information: if related_information.is_empty() {
                None
            } else {
                Some(related_information)
            },
            severity: Some(match diagnostic.severity() {
                Severity::Error => DiagnosticSeverity::ERROR,
                Severity::Warning => DiagnosticSeverity::WARNING,
            }),
            code: diagnostic.error_code().map(|code| NumberOrString::String(code.to_string())),
            ..Diagnostic::default()
        });
    }
}

/// Converts an internal diagnostic location to an LSP range.
fn get_range(db: &dyn FilesGroup, location: &DiagnosticLocation) -> Range {
    let location = location.user_location(db);
    let start = location.span.start.position_in_file(db, location.file_id).unwrap().to_lsp();
    let end = location.span.start.position_in_file(db, location.file_id).unwrap().to_lsp();
    Range { start, end }
}
