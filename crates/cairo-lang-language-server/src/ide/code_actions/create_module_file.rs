use cairo_lang_syntax::node::ast::{ItemModule, MaybeModuleBody};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, TypedSyntaxNode};
use lsp_types::{CodeAction, CreateFile, DocumentChangeOperation, ResourceOp, Url, WorkspaceEdit};

use crate::lang::db::{AnalysisDatabase, LsSyntaxGroup};

/// Code actions for missing module file.
pub fn create_module_file(
    db: &AnalysisDatabase,
    node: SyntaxNode,
    mut url: Url,
) -> Option<CodeAction> {
    if let Some(item_module) = db.first_ancestor_of_kind(node.clone(), SyntaxKind::ItemModule) {
        let item_module = ItemModule::from_syntax_node(db, item_module);

        if let MaybeModuleBody::None(_) = item_module.body(db) {
            let module_name = item_module.name(db).text(db);
            let title = format!("Create module file `{module_name}`");

            if let Ok(mut path) = url.path_segments_mut() {
                path.pop();
                path.push(&format!("{module_name}.cairo"));
            }

            return Some(CodeAction {
                title: title.clone(),
                edit: Some(WorkspaceEdit {
                    document_changes: Some(lsp_types::DocumentChanges::Operations(vec![
                        DocumentChangeOperation::Op(ResourceOp::Create(CreateFile {
                            uri: url,
                            annotation_id: None,
                            options: None,
                        })),
                    ])),
                    ..Default::default()
                }),
                ..Default::default()
            });
        }
    }

    None
}
