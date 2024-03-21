use std::collections::HashMap;

use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_syntax::node::SyntaxNode;
use tower_lsp::lsp_types::{CodeAction, Diagnostic, TextEdit, Url, WorkspaceEdit};

/// Create a code action that prefixes an unused variable with an `_`.
#[tracing::instrument(level = "trace", skip_all)]
pub fn rename_unused_variable(
    db: &dyn SemanticGroup,
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
