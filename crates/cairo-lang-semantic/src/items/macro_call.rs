use cairo_lang_defs::ids::{LanguageElementId, MacroCallId, MacroDeclarationId, ModuleId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_filesystem::ids::{FileKind, FileLongId, VirtualFile};
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::Intern;

use crate::SemanticDiagnostic;
use crate::db::SemanticGroup;
use crate::diagnostic::{
    NotFoundItemType, SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder,
};
use crate::expr::inference::InferenceId;
use crate::items::macro_declaration::{MatcherContext, expand_macro_rule, is_macro_rule_match};
use crate::resolve::{ResolutionContext, ResolvedGenericItem, Resolver};

/// The data associated with a macro call in item context.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroCallData {
    /// The macro declaration that this macro call refers to, if found.
    pub macro_declaration_id: Option<MacroDeclarationId>,
    /// The module to which the macro call was expanded to.
    pub macro_call_module: Option<ModuleId>,
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
}

/// Query implementation of [crate::db::SemanticGroup::priv_macro_call_data].
pub fn priv_macro_call_data(
    db: &dyn SemanticGroup,
    macro_call_id: MacroCallId,
) -> Maybe<MacroCallData> {
    let inference_id = InferenceId::MacroCall(macro_call_id);
    let module_file_id = macro_call_id.module_file_id(db);
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    let mut diagnostics = SemanticDiagnostics::default();
    let macro_call_syntax = db.module_macro_call_by_id(macro_call_id)?.to_maybe()?;
    // Resolve the macro call path, and report diagnostics if it finds no match or
    // the resolved item is not a macro declaration.
    let macro_call_path = macro_call_syntax.path(db);
    let macro_name = macro_call_path.as_syntax_node().get_text_without_trivia(db).to_string();
    let macro_declaration_id = resolver.resolve_generic_path(
        &mut diagnostics,
        &macro_call_path,
        NotFoundItemType::Macro,
        ResolutionContext::Default,
    );
    let macro_declaration_id = match macro_declaration_id {
        Ok(ResolvedGenericItem::Macro(macro_declaration_id)) => Some(macro_declaration_id),
        Ok(_) => {
            diagnostics.report(
                macro_call_path.stable_ptr(db).untyped(),
                SemanticDiagnosticKind::MacroCallToNotAMacro(macro_name.clone().into()),
            );
            None
        }
        Err(_) => {
            // Resolver error, the diagnostics should already have been reported in the resolver.
            None
        }
    };
    let macro_call_module = if let Some(macro_declaration_id) = macro_declaration_id {
        let macro_rules = db.macro_declaration_rules(macro_declaration_id)?;
        let Some((rule, (captures, placeholder_to_rep_id))) = macro_rules.iter().find_map(|rule| {
            is_macro_rule_match(db, rule, &macro_call_syntax.arguments(db)).map(|res| (rule, res))
        }) else {
            return Err(diagnostics.report(
                macro_call_syntax.stable_ptr(db),
                SemanticDiagnosticKind::InlineMacroNoMatchingRule(macro_name.clone().into()),
            ));
        };
        let mut matcher_ctx =
            MatcherContext { captures, placeholder_to_rep_id, ..Default::default() };
        let expanded_code = expand_macro_rule(db, rule, &mut matcher_ctx)?;
        let new_file = FileLongId::Virtual(VirtualFile {
            parent: Some(macro_call_syntax.stable_ptr(db).untyped().file_id(db)),
            name: macro_name.into(),
            content: expanded_code.text.clone(),
            code_mappings: expanded_code.code_mappings.clone(),
            kind: FileKind::Module,
            original_item_removed: false,
        })
        .intern(db);
        Some(ModuleId::MacroCall(macro_call_id, new_file))
    } else {
        None
    };

    Ok(MacroCallData { macro_declaration_id, macro_call_module, diagnostics: diagnostics.build() })
}

/// Cycle handling for the `priv_macro_call_data` query.
pub fn priv_macro_call_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &salsa::Cycle,
    macro_call_id: &MacroCallId,
) -> Maybe<MacroCallData> {
    // If we are in a cycle, we return an empty MacroCallData with no diagnostics.
    // This is to prevent infinite recursion in case of cyclic macro calls.
    let mut diagnostics = SemanticDiagnostics::default();
    let macro_call_syntax = db.module_macro_call_by_id(*macro_call_id)?.to_maybe()?;
    let macro_call_path = macro_call_syntax.path(db);
    let macro_name = macro_call_path.as_syntax_node().get_text_without_trivia(db).to_string();

    diagnostics.report(
        macro_call_id.stable_ptr(db).untyped(),
        SemanticDiagnosticKind::InlineMacroNotFound(macro_name.into()),
    );
    Ok(MacroCallData {
        macro_declaration_id: None,
        macro_call_module: None,
        diagnostics: diagnostics.build(),
    })
}

/// Query implementation of [crate::db::SemanticGroup::macro_call_diagnostics].
pub fn macro_call_diagnostics(
    db: &dyn SemanticGroup,
    macro_call_id: MacroCallId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_macro_call_data(macro_call_id).map(|data| data.diagnostics).unwrap_or_default()
}
/// Cycle handling for the `macro_call_diagnostics` query.
pub fn macro_call_diagnostics_cycle(
    db: &dyn SemanticGroup,
    _cycle: &salsa::Cycle,
    macro_call_id: &MacroCallId,
) -> Diagnostics<SemanticDiagnostic> {
    priv_macro_call_data(db, *macro_call_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::macro_call_declaration_id].
pub fn macro_call_declaration_id(
    db: &dyn SemanticGroup,
    macro_call_id: MacroCallId,
) -> Maybe<Option<MacroDeclarationId>> {
    db.priv_macro_call_data(macro_call_id).map(|data| data.macro_declaration_id)
}
/// Cycle handling for the `macro_call_declaration_id` query.
pub fn macro_call_declaration_id_cycle(
    db: &dyn SemanticGroup,
    _cycle: &salsa::Cycle,
    macro_call_id: &MacroCallId,
) -> Maybe<Option<MacroDeclarationId>> {
    priv_macro_call_data(db, *macro_call_id).map(|data| data.macro_declaration_id)
}
/// Query implementation of [crate::db::SemanticGroup::macro_call_module_id].
pub fn macro_call_module_id(
    db: &dyn SemanticGroup,
    macro_call_id: MacroCallId,
) -> Maybe<Option<ModuleId>> {
    db.priv_macro_call_data(macro_call_id).map(|data| data.macro_call_module)
}
/// Cycle handling for the `macro_call_module_id` query.
pub fn macro_call_module_id_cycle(
    db: &dyn SemanticGroup,
    _cycle: &salsa::Cycle,
    macro_call_id: &MacroCallId,
) -> Maybe<Option<ModuleId>> {
    priv_macro_call_data(db, *macro_call_id).map(|data| data.macro_call_module)
}
