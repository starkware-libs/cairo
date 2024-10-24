use std::collections::HashMap;

use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_utils::Upcast;
use lsp_types::{CodeAction, Diagnostic, TextEdit, Url, WorkspaceEdit};

use crate::lang::db::AnalysisDatabase;

/// Create a code action that prefixes an unused variable with an `_`.
pub fn rename_unused_variable(
    db: &AnalysisDatabase,
    node: &SyntaxNode,
    diagnostic: Diagnostic,
    uri: Url,
) -> CodeAction {
    CodeAction {
        title: format!("Rename to `_{}`", node.get_text(db.upcast())),
        edit: Some(WorkspaceEdit {
            changes: Some(HashMap::from_iter([(
                uri,
                // The diagnostic range is just the first char of the variable name, so we can just
                // pass an underscore as the new text it won't replace the current variable name,
                // and it will prefix it with `_`
                vec![TextEdit { range: diagnostic.range, new_text: "_".to_owned() }],
            )])),
            document_changes: None,
            change_annotations: None,
        }),
        diagnostics: Some(vec![diagnostic]),
        ..Default::default()
    }
}
