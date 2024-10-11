use std::collections::HashMap;

use cairo_lang_defs::ids::{LookupItemId, ModuleId, NamedLanguageElementId};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::TextOffset;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::inference::InferenceId;
use cairo_lang_semantic::items::function_with_body::SemanticExprLookup;
use cairo_lang_semantic::lookup_item::{HasResolverData, LookupItemEx};
use cairo_lang_semantic::resolve::Resolver;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::Upcast;
use lsp_types::{CodeAction, CodeActionKind, Range, TextEdit, Url, WorkspaceEdit};
use tracing::debug;

use crate::ide::utils::find_methods_for_type;
use crate::lang::db::{AnalysisDatabase, LsSemanticGroup};
use crate::lang::lsp::{LsProtoGroup, ToLsp};

/// Create a Quick Fix code action to add a missing trait given a `CannotCallMethod` diagnostic.
#[tracing::instrument(level = "trace", skip_all)]
pub fn add_missing_trait(db: &AnalysisDatabase, node: &SyntaxNode, uri: Url) -> Vec<CodeAction> {
    let file_id = db.file_for_url(&uri).unwrap();
    let lookup_items = db.collect_lookup_items_stack(node).unwrap();
    let unknown_method_name = node.get_text(db.upcast());
    missing_traits_actions(db, file_id, lookup_items, node, &unknown_method_name, uri)
        .unwrap_or_default()
}

/// Returns a list of code actions to add missing traits to the current module, or `None` if the
/// type is missing.
fn missing_traits_actions(
    db: &AnalysisDatabase,
    file_id: FileId,
    lookup_items: Vec<LookupItemId>,
    node: &SyntaxNode,
    unknown_method_name: &str,
    uri: Url,
) -> Option<Vec<CodeAction>> {
    let syntax_db = db.upcast();
    // Get a resolver in the current context.
    let lookup_item_id = lookup_items.into_iter().next()?;
    let function_with_body = lookup_item_id.function_with_body()?;
    let resolver_data = lookup_item_id.resolver_data(db).ok()?;
    let resolver = Resolver::with_data(
        db,
        resolver_data.as_ref().clone_with_inference_id(db, InferenceId::NoContext),
    );
    let mut expr_node = node.clone();
    while expr_node.kind(db.upcast()) != SyntaxKind::ExprBinary {
        expr_node = expr_node.parent()?;
    }
    let expr_node = ast::ExprBinary::from_syntax_node(db.upcast(), expr_node).lhs(db.upcast());
    let stable_ptr = expr_node.stable_ptr().untyped();
    // Get its semantic model.
    let expr_id = db.lookup_expr_by_ptr(function_with_body, expr_node.stable_ptr()).ok()?;
    let semantic_expr = db.expr_semantic(function_with_body, expr_id);
    // Get the type.
    let ty = semantic_expr.ty();
    if ty.is_missing(db) {
        debug!("type is missing");
        return None;
    }

    let module_start_offset =
        if let Some(ModuleId::Submodule(submodule_id)) = db.find_module_containing_node(node) {
            let module_def_ast = submodule_id.stable_ptr(db.upcast()).lookup(syntax_db);
            if let ast::MaybeModuleBody::Some(body) = module_def_ast.body(syntax_db) {
                body.items(syntax_db).as_syntax_node().span_start_without_trivia(syntax_db)
            } else {
                TextOffset::default()
            }
        } else {
            TextOffset::default()
        };
    let module_start_position =
        module_start_offset.position_in_file(db.upcast(), file_id).unwrap().to_lsp();
    let relevant_methods = find_methods_for_type(db, resolver, ty, stable_ptr);
    let current_module = db.find_module_containing_node(node)?;
    let module_visible_traits = db.visible_traits_from_module(current_module)?;
    let mut code_actions = vec![];
    for method in relevant_methods {
        let method_name = method.name(db.upcast());
        if method_name == unknown_method_name {
            if let Some(trait_path) = module_visible_traits.get(&method.trait_id(db.upcast())) {
                code_actions.push(CodeAction {
                    title: format!("Import {}", trait_path),
                    kind: Some(CodeActionKind::QUICKFIX),
                    edit: Some(WorkspaceEdit {
                        changes: Some(HashMap::from_iter([(uri.clone(), vec![TextEdit {
                            range: Range::new(module_start_position, module_start_position),
                            new_text: format!("use {};\n", trait_path),
                        }])])),
                        document_changes: None,
                        change_annotations: None,
                    }),
                    diagnostics: None,
                    ..Default::default()
                });
            }
        }
    }
    Some(code_actions)
}
