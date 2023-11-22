use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileLongId, VirtualFile};
use cairo_lang_utils::Upcast;

use crate::{DiagnosticEntry, DiagnosticLocation, Diagnostics, DiagnosticsBuilder};

/// Transforms diagnostics that originates from plugin generated files. Uses the plugin's diagnostic
/// mapper.
pub fn map_diagnostics<TEntry: DiagnosticEntry>(
    db: &TEntry::DbType,
    original_diagnostics: Diagnostics<TEntry>,
) -> (bool, Diagnostics<TEntry>) {
    let mut diagnostics = DiagnosticsBuilder::default();
    let mut has_change: bool = false;

    for tree in &original_diagnostics.0.subtrees {
        let (changed, new_diags) = map_diagnostics(db, tree.clone());
        diagnostics.extend(new_diags);
        has_change |= changed;
    }

    for diag in &original_diagnostics.0.leaves {
        diagnostics.add(
            if let Some(user_location) = map_location(db.upcast(), diag.location(db)) {
                has_change = true;
                diag.map_plugin_diagnostic(db, user_location)
            } else {
                diag.clone()
            },
        );
    }

    if !has_change {
        return (false, original_diagnostics);
    }

    (has_change, diagnostics.build())
}

/// Transforms a diagnostic that originates from plugin generated files. Uses the plugin's
/// diagnostic mapper.
pub fn map_location(
    db: &dyn FilesGroup,
    mut diag_location: DiagnosticLocation,
) -> Option<DiagnosticLocation> {
    let mut diag_mapped = false;
    while let FileLongId::Virtual(VirtualFile { parent: Some(parent), code_mappings, .. }) =
        db.lookup_intern_file(diag_location.file_id)
    {
        if let Some(span) =
            code_mappings.iter().find_map(|mapping| mapping.translate(diag_location.span))
        {
            diag_location.span = span;
            diag_location.file_id = parent;
            diag_mapped = true;
        } else {
            break;
        }
    }
    if diag_mapped { Some(diag_location) } else { None }
}
