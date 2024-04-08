use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId, ModuleItemId};
use cairo_lang_diagnostics::{format_diagnostics, DiagnosticLocation, Severity};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;

/// Returns the expanded code for `module_id` after running all plugins and extends `diagnostics`
/// with all the plugins diagnostics.
pub fn expand_module_text(
    db: &dyn DefsGroup,
    module_id: ModuleId,
    diagnostics: &mut Vec<String>,
) -> String {
    let mut output = String::new();
    // A collection of all the use statements in the module.
    let mut uses_list = UnorderedHashSet::<_>::default();
    let syntax_db = db.upcast();
    // Collect the module diagnostics.
    for (file_id, diag) in db.module_plugin_diagnostics(module_id).unwrap().iter() {
        let syntax_node = diag.stable_ptr.lookup(syntax_db);
        let location = DiagnosticLocation {
            file_id: file_id.file_id(db.upcast()).unwrap(),
            span: syntax_node.span_without_trivia(syntax_db),
        };
        diagnostics.push(format!(
            "{}: {}",
            Severity::Error,
            format_diagnostics(db.upcast(), &diag.message, location)
        ));
    }
    for item_id in db.module_items(module_id).unwrap().iter() {
        if let ModuleItemId::Submodule(item) = item_id {
            let submodule_item = item.stable_ptr(db).lookup(syntax_db);
            if let ast::MaybeModuleBody::Some(body) = submodule_item.body(syntax_db) {
                // Recursively expand inline submodules.
                output.extend([
                    submodule_item.attributes(syntax_db).as_syntax_node().get_text(syntax_db),
                    submodule_item.visibility(syntax_db).as_syntax_node().get_text(syntax_db),
                    submodule_item.module_kw(syntax_db).as_syntax_node().get_text(syntax_db),
                    submodule_item.name(syntax_db).as_syntax_node().get_text(syntax_db),
                    body.lbrace(syntax_db).as_syntax_node().get_text(syntax_db),
                    expand_module_text(db, ModuleId::Submodule(*item), diagnostics),
                    body.rbrace(syntax_db).as_syntax_node().get_text(syntax_db),
                ]);
                continue;
            }
        } else if let ModuleItemId::Use(use_id) = item_id {
            let mut use_item = use_id.stable_ptr(db).lookup(syntax_db).as_syntax_node();
            // Climb up the AST until the syntax kind is ItemUse. This is needed since the use item
            // points to the use leaf as one use statement can represent multiple use items.
            while let Some(parent) = use_item.parent() {
                use_item = parent;
                if use_item.kind(syntax_db) == SyntaxKind::ItemUse {
                    break;
                }
            }
            if uses_list.insert(use_item.clone()) {
                output.push_str(&use_item.get_text(syntax_db));
            }
            continue;
        }
        let syntax_item = item_id.untyped_stable_ptr(db);
        // Output other items as is.
        output.push_str(&syntax_item.lookup(syntax_db).get_text(syntax_db));
    }
    output
}
