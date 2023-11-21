use cairo_lang_filesystem::ids::{FileLongId, VirtualFile};
use cairo_lang_utils::Upcast;

use crate::{DiagnosticEntry, DiagnosticLocation, Diagnostics, DiagnosticsBuilder};

/// Transforms diagnostics that originate from plugin generated files. Uses the plugin's diagnostic
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
        let mut diag_mapped = false;
        let diag_location = diag.location(db);
        let mut mapped_span = diag_location.span;
        let mut orig_file = diag_location.file_id;
        while let FileLongId::Virtual(VirtualFile { parent: Some(parent), code_mappings, .. }) =
            db.upcast().lookup_intern_file(orig_file)
        {
            if let Some(span) =
                code_mappings.iter().find_map(|mapping| mapping.translate(mapped_span))
            {
                mapped_span = span;
                diag_mapped = true;
                orig_file = parent;
            } else {
                break;
            }
        }
        if !diag_mapped {
            diagnostics.add(diag.clone());
            continue;
        }
        diagnostics.add(diag.map_plugin_diagnostic(
            db,
            DiagnosticLocation { file_id: orig_file, span: mapped_span },
        ));
        has_change = true;
    }

    if !has_change {
        return (false, original_diagnostics);
    }

    (has_change, diagnostics.build())
}
