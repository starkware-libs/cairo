use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId, ModuleItemId};
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_diagnostics::{
    DiagnosticEntry, DiagnosticLocation, DiagnosticsBuilder, ErrorCode, Severity,
};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;

/// Returns the expanded code for `module_id` after running all plugins and extends `diagnostics`
/// with all the plugins diagnostics.
pub fn expand_module_text(
    db: &(dyn DefsGroup + 'static),
    module_id: ModuleId,
    diagnostics: &mut Vec<String>,
) -> String {
    let mut output = String::new();
    // A collection of all the use statements in the module.
    let mut uses_list = UnorderedHashSet::<_>::default();
    // Collect the module diagnostics.
    let mut builder = DiagnosticsBuilder::default();
    for (_file_id, diag) in db.module_plugin_diagnostics(module_id).unwrap().iter() {
        builder.add(TestDiagnosticEntry(diag.clone()));
    }
    let build = builder.build();
    let file_notes = db.module_plugin_diagnostics_notes(module_id).unwrap();
    let formatted = build.format_with_severity(db, &file_notes);
    diagnostics.extend(
        formatted
            .into_iter()
            .map(|formatted| format!("{}: {}", Severity::Error, formatted.message())),
    );
    for item_id in db.module_items(module_id).unwrap().iter() {
        if let ModuleItemId::Submodule(item) = item_id {
            let submodule_item = item.stable_ptr(db).lookup(db);
            if let ast::MaybeModuleBody::Some(body) = submodule_item.body(db) {
                // Recursively expand inline submodules.
                output.extend([
                    submodule_item.attributes(db).as_syntax_node().get_text(db),
                    submodule_item.visibility(db).as_syntax_node().get_text(db),
                    submodule_item.module_kw(db).as_syntax_node().get_text(db),
                    submodule_item.name(db).as_syntax_node().get_text(db),
                    body.lbrace(db).as_syntax_node().get_text(db),
                    expand_module_text(db, ModuleId::Submodule(*item), diagnostics),
                    body.rbrace(db).as_syntax_node().get_text(db),
                ]);
                continue;
            }
        } else if let ModuleItemId::Use(use_id) = item_id {
            let mut use_item = use_id.stable_ptr(db).lookup(db).as_syntax_node();
            // Climb up the AST until the syntax kind is ItemUse. This is needed since the use item
            // points to the use leaf as one use statement can represent multiple use items.
            while let Some(parent) = use_item.parent(db) {
                use_item = parent;
                if use_item.kind(db) == SyntaxKind::ItemUse {
                    break;
                }
            }
            if uses_list.insert(use_item) {
                output.push_str(&use_item.get_text(db));
            }
            continue;
        }
        let syntax_item = item_id.untyped_stable_ptr(db);
        // Output other items as is.
        output.push_str(&syntax_item.lookup(db).get_text(db));
    }
    output
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct TestDiagnosticEntry(pub PluginDiagnostic);
impl DiagnosticEntry for TestDiagnosticEntry {
    type DbType = dyn DefsGroup;
    fn format(&self, _db: &Self::DbType) -> String {
        self.0.message.to_string()
    }
    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        match self.0.inner_span {
            Some(inner_span) => StableLocation::with_inner_span(self.0.stable_ptr, inner_span)
                .diagnostic_location(db),
            None => StableLocation::new(self.0.stable_ptr).diagnostic_location(db),
        }
    }
    fn severity(&self) -> Severity {
        self.0.severity
    }
    fn error_code(&self) -> Option<ErrorCode> {
        None
    }
    fn is_same_kind(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
