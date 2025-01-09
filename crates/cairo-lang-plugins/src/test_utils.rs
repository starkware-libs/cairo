use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId, ModuleItemId};
<<<<<<< HEAD
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_diagnostics::{
    DiagnosticEntry, DiagnosticLocation, DiagnosticsBuilder, ErrorCode, Severity,
};
=======
use cairo_lang_diagnostics::{DiagnosticLocation, Severity, format_diagnostics};
use cairo_lang_filesystem::span::TextSpan;
>>>>>>> d24ed1917 (Change macro syntax to be token tree based and fix legacy macros. (#6388))
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
    let syntax_db = db.upcast();
    // Collect the module diagnostics.
<<<<<<< HEAD
    let mut builder = DiagnosticsBuilder::default();
    for (_file_id, diag) in db.module_plugin_diagnostics(module_id).unwrap().iter() {
        builder.add(TestDiagnosticEntry(diag.clone()));
=======
    for (file_id, diag) in db.module_plugin_diagnostics(module_id).unwrap().iter() {
        let syntax_node = diag.stable_ptr.lookup(syntax_db);
        let file_id = file_id.file_id(db.upcast()).unwrap();
        let location = match diag.inner_span {
            Some((start, width)) => {
                let start = syntax_node.offset().add_width(start);
                let end = start.add_width(width);
                DiagnosticLocation { file_id, span: TextSpan { start, end } }
            }
            None => {
                DiagnosticLocation { file_id, span: syntax_node.span_without_trivia(db.upcast()) }
            }
        };
        diagnostics.push(format!(
            "{}: {}",
            Severity::Error,
            format_diagnostics(db.upcast(), &diag.message, location)
        ));
>>>>>>> d24ed1917 (Change macro syntax to be token tree based and fix legacy macros. (#6388))
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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct TestDiagnosticEntry(pub PluginDiagnostic);
impl DiagnosticEntry for TestDiagnosticEntry {
    type DbType = dyn DefsGroup;
    fn format(&self, _db: &Self::DbType) -> String {
        self.0.message.to_string()
    }
    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        StableLocation::new(self.0.stable_ptr).diagnostic_location(db)
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
