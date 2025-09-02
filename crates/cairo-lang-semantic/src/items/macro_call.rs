use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{LanguageElementId, MacroCallId, ModuleId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, skip_diagnostic};
use cairo_lang_filesystem::ids::{CodeMapping, FileKind, FileLongId, VirtualFile};
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::Intern;

use crate::SemanticDiagnostic;
use crate::db::SemanticGroup;
use crate::diagnostic::{
    NotFoundItemType, SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder,
};
use crate::expr::inference::InferenceId;
use crate::items::macro_declaration::{MatcherContext, expand_macro_rule, is_macro_rule_match};
use crate::resolve::{ResolutionContext, ResolvedGenericItem, Resolver, ResolverMacroData};

/// The data associated with a macro call in item context.
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub struct MacroCallData<'db> {
    /// The module to which the macro call was expanded to.
    pub macro_call_module: Maybe<ModuleId<'db>>,
    pub diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    pub defsite_module_id: ModuleId<'db>,
    pub callsite_module_id: ModuleId<'db>,
    pub expansion_mappings: Arc<[CodeMapping]>,
    pub parent_macro_call_data: Option<Arc<ResolverMacroData<'db>>>,
}

/// Implementation of [crate::db::SemanticGroup::priv_macro_call_data].
pub fn priv_macro_call_data<'db>(
    db: &'db dyn SemanticGroup,
    macro_call_id: MacroCallId<'db>,
) -> Maybe<MacroCallData<'db>> {
    let inference_id = InferenceId::MacroCall(macro_call_id);
    let module_file_id = macro_call_id.module_file_id(db);
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    let mut diagnostics = SemanticDiagnostics::default();
    let macro_call_syntax = db.module_macro_call_by_id(macro_call_id)?;
    // Resolve the macro call path, and report diagnostics if it finds no match or
    // the resolved item is not a macro declaration.
    let macro_call_path = macro_call_syntax.path(db);
    let macro_name = macro_call_path.as_syntax_node().get_text_without_trivia(db);
    let callsite_module_id = macro_call_id.parent_module(db);
    let macro_declaration_id = match resolver.resolve_generic_path(
        &mut diagnostics,
        &macro_call_path,
        NotFoundItemType::Macro,
        ResolutionContext::Default,
    ) {
        Ok(ResolvedGenericItem::Macro(macro_declaration_id)) => macro_declaration_id,
        Ok(_) => {
            let diag_added = diagnostics.report(
                macro_call_syntax.stable_ptr(db),
                SemanticDiagnosticKind::InlineMacroNotFound(macro_name.into()),
            );
            return Ok(MacroCallData {
                macro_call_module: Err(diag_added),
                diagnostics: diagnostics.build(),
                defsite_module_id: callsite_module_id,
                callsite_module_id,
                expansion_mappings: Arc::new([]),
                parent_macro_call_data: None,
            });
        }
        Err(diag_added) => {
            return Ok(MacroCallData {
                macro_call_module: Err(diag_added),
                diagnostics: diagnostics.build(),
                defsite_module_id: callsite_module_id,
                callsite_module_id,
                expansion_mappings: Arc::new([]),
                parent_macro_call_data: None,
            });
        }
    };
    let defsite_module_id = macro_declaration_id.parent_module(db);
    let macro_rules = match db.macro_declaration_rules(macro_declaration_id) {
        Ok(rules) => rules,
        Err(diag_added) => {
            return Ok(MacroCallData {
                macro_call_module: Err(diag_added),
                diagnostics: diagnostics.build(),
                defsite_module_id,
                callsite_module_id,
                expansion_mappings: Arc::new([]),
                parent_macro_call_data: None,
            });
        }
    };
    let Some((rule, (captures, placeholder_to_rep_id))) = macro_rules.iter().find_map(|rule| {
        is_macro_rule_match(db, rule, &macro_call_syntax.arguments(db)).map(|res| (rule, res))
    }) else {
        let diag_added = diagnostics.report(
            macro_call_syntax.stable_ptr(db),
            SemanticDiagnosticKind::InlineMacroNoMatchingRule(macro_name.into()),
        );
        return Ok(MacroCallData {
            macro_call_module: Err(diag_added),
            diagnostics: diagnostics.build(),
            defsite_module_id,
            callsite_module_id,
            expansion_mappings: Arc::new([]),
            parent_macro_call_data: None,
        });
    };
    let mut matcher_ctx = MatcherContext { captures, placeholder_to_rep_id, ..Default::default() };
    let expanded_code = expand_macro_rule(db, rule, &mut matcher_ctx).unwrap();
    let parent_macro_call_data = resolver.macro_call_data;
    let new_file = FileLongId::Virtual(VirtualFile {
        parent: Some(macro_call_syntax.stable_ptr(db).untyped().file_id(db)),
        name: macro_name.into(),
        content: expanded_code.text.clone(),
        code_mappings: expanded_code.code_mappings.clone(),
        kind: FileKind::Module,
        original_item_removed: false,
    })
    .intern(db);
    let macro_call_module = ModuleId::MacroCall { id: macro_call_id, generated_file_id: new_file };
    Ok(MacroCallData {
        macro_call_module: Ok(macro_call_module),
        diagnostics: diagnostics.build(),
        defsite_module_id,
        callsite_module_id,
        expansion_mappings: expanded_code.code_mappings,
        parent_macro_call_data,
    })
}

/// Query implementation of [crate::db::SemanticGroup::priv_macro_call_data].
#[salsa::tracked]
pub fn priv_macro_call_data_tracked<'db>(
    db: &'db dyn SemanticGroup,
    macro_call_id: MacroCallId<'db>,
) -> Maybe<MacroCallData<'db>> {
    priv_macro_call_data(db, macro_call_id)
}

/// Cycle handling for the `priv_macro_call_data` query.
pub fn priv_macro_call_data_cycle<'db>(
    db: &'db dyn SemanticGroup,
    macro_call_id: &MacroCallId<'db>,
) -> Maybe<MacroCallData<'db>> {
    // If we are in a cycle, we return an empty MacroCallData with no diagnostics.
    // This is to prevent infinite recursion in case of cyclic macro calls.
    let mut diagnostics = SemanticDiagnostics::default();
    let macro_call_syntax = db.module_macro_call_by_id(*macro_call_id)?;
    let macro_call_path = macro_call_syntax.path(db);
    let macro_name = macro_call_path.as_syntax_node().get_text_without_trivia(db);

    let diag_added = diagnostics.report(
        macro_call_id.stable_ptr(db).untyped(),
        SemanticDiagnosticKind::InlineMacroNotFound(macro_name.into()),
    );
    let module_id = macro_call_id.parent_module(db);

    Ok(MacroCallData {
        macro_call_module: Err(diag_added),
        diagnostics: diagnostics.build(),
        defsite_module_id: module_id,
        callsite_module_id: module_id,
        expansion_mappings: Arc::new([]),
        parent_macro_call_data: None,
    })
}

/// Implementation of [crate::db::SemanticGroup::macro_call_diagnostics].
pub fn macro_call_diagnostics<'db>(
    db: &'db dyn SemanticGroup,
    macro_call_id: MacroCallId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    priv_macro_call_data(db, macro_call_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::macro_call_diagnostics].
#[salsa::tracked(cycle_result=macro_call_diagnostics_cycle)]
pub fn macro_call_diagnostics_tracked<'db>(
    db: &'db dyn SemanticGroup,
    macro_call_id: MacroCallId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    macro_call_diagnostics(db, macro_call_id)
}

/// Cycle handling for the `macro_call_diagnostics` query.
pub fn macro_call_diagnostics_cycle<'db>(
    db: &'db dyn SemanticGroup,
    macro_call_id: MacroCallId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    priv_macro_call_data(db, macro_call_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Implementation of [crate::db::SemanticGroup::macro_call_module_id].
pub fn macro_call_module_id<'db>(
    db: &'db dyn SemanticGroup,
    macro_call_id: MacroCallId<'db>,
) -> Maybe<ModuleId<'db>> {
    db.priv_macro_call_data(macro_call_id)?.macro_call_module
}

/// Query implementation of [crate::db::SemanticGroup::macro_call_module_id].
#[salsa::tracked(cycle_result=macro_call_module_id_cycle)]
pub fn macro_call_module_id_tracked<'db>(
    db: &'db dyn SemanticGroup,
    macro_call_id: MacroCallId<'db>,
) -> Maybe<ModuleId<'db>> {
    macro_call_module_id(db, macro_call_id)
}
/// Cycle handling for the `macro_call_module_id` query.
pub fn macro_call_module_id_cycle<'db>(
    _db: &'db dyn SemanticGroup,
    _macro_call_id: MacroCallId<'db>,
) -> Maybe<ModuleId<'db>> {
    Err(skip_diagnostic())
}
