use cairo_lang_diagnostics::{DiagnosticEntry, DiagnosticLocation, Diagnostics, Severity};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_utils::Upcast;
use lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString, Range,
};
use tracing::error;

use crate::lang::lsp::{LsProtoGroup, ToLsp};

/// Converts internal diagnostics to LSP format.
#[tracing::instrument(level = "trace", skip_all)]
pub fn map_cairo_diagnostics_to_lsp<T: DiagnosticEntry>(
    db: &T::DbType,
    diags: &mut Vec<Diagnostic>,
    diagnostics: &Diagnostics<T>,
    processed_file_id: &FileId,
    trace_macro_diagnostics: bool,
) {
    for diagnostic in if trace_macro_diagnostics {
        diagnostics.get_all()
    } else {
        diagnostics.get_diagnostics_without_duplicates(db)
    } {
        let mut message = diagnostic.format(db);
        let mut related_information = vec![];
        for note in diagnostic.notes(db) {
            if let Some(location) = &note.location {
                let Some((range, file_id)) = get_mapped_range_and_add_mapping_note(
                    db,
                    location,
                    trace_macro_diagnostics.then_some(&mut related_information),
                    "Next note mapped from here.",
                ) else {
                    continue;
                };
                related_information.push(DiagnosticRelatedInformation {
                    location: Location { uri: db.url_for_file(file_id), range },
                    message: note.text.clone(),
                });
            } else {
                message += &format!("\nnote: {}", note.text);
            }
        }

        let Some((range, mapped_file_id)) = get_mapped_range_and_add_mapping_note(
            db,
            &diagnostic.location(db),
            trace_macro_diagnostics.then_some(&mut related_information),
            "Diagnostic mapped from here.",
        ) else {
            continue;
        };

        if mapped_file_id != *processed_file_id {
            continue;
        }
        diags.push(Diagnostic {
            range,
            message,
            related_information: (!related_information.is_empty()).then_some(related_information),
            severity: Some(match diagnostic.severity() {
                Severity::Error => DiagnosticSeverity::ERROR,
                Severity::Warning => DiagnosticSeverity::WARNING,
            }),
            code: diagnostic.error_code().map(|code| NumberOrString::String(code.to_string())),
            ..Diagnostic::default()
        });
    }
}

/// Returns the mapped range of a location, optionally adds a note about the mapping of the
/// location.
fn get_mapped_range_and_add_mapping_note(
    db: &(impl Upcast<dyn FilesGroup> + ?Sized),
    orig: &DiagnosticLocation,
    related_info: Option<&mut Vec<DiagnosticRelatedInformation>>,
    message: &str,
) -> Option<(Range, FileId)> {
    let mapped = orig.user_location(db.upcast());
    let mapped_range = get_lsp_range(db.upcast(), &mapped)?;
    if let Some(related_info) = related_info {
        if *orig != mapped {
            if let Some(range) = get_lsp_range(db.upcast(), orig) {
                related_info.push(DiagnosticRelatedInformation {
                    location: Location { uri: db.url_for_file(orig.file_id), range },
                    message: message.to_string(),
                });
            }
        }
    }
    Some((mapped_range, mapped.file_id))
}

/// Converts an internal diagnostic location to an LSP range.
fn get_lsp_range(db: &dyn FilesGroup, location: &DiagnosticLocation) -> Option<Range> {
    let Some(span) = location.span.position_in_file(db, location.file_id) else {
        error!("failed to get range for diagnostic");
        return None;
    };
    Some(span.to_lsp())
}
