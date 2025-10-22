use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{LanguageElementId, MacroCallId, ModuleId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, skip_diagnostic};
use cairo_lang_filesystem::ids::{
    CodeMapping, CodeOrigin, FileKind, FileLongId, SmolStrId, VirtualFile,
};
use cairo_lang_filesystem::span::{TextOffset, TextSpan};
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::Intern;
use salsa::Database;

use crate::SemanticDiagnostic;
use crate::diagnostic::{
    NotFoundItemType, SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder,
};
use crate::expr::inference::InferenceId;
use crate::items::macro_declaration::{
    MacroDeclarationSemantic, MatcherContext, expand_macro_rule, is_macro_rule_match,
};
use crate::items::module::ModuleSemantic;
use crate::lsp_helpers::LspHelpers;
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

/// Implementation of [MacroCallSemantic::priv_macro_call_data].
fn priv_macro_call_data<'db>(
    db: &'db dyn Database,
    macro_call_id: MacroCallId<'db>,
) -> Maybe<MacroCallData<'db>> {
    let inference_id = InferenceId::MacroCall(macro_call_id);
    let module_id = macro_call_id.module_id(db);
    let mut resolver = Resolver::new(db, module_id, inference_id);
    let macro_call_syntax = db.module_macro_call_by_id(macro_call_id)?;
    // Resolve the macro call path, and report diagnostics if it finds no match or
    // the resolved item is not a macro declaration.
    let macro_call_path = macro_call_syntax.path(db);
    let macro_name = macro_call_path.as_syntax_node().get_text_without_trivia(db);
    let callsite_module_id = macro_call_id.parent_module(db);
    // If the call is to `expose!` and no other `expose` item is locally declared - using expose.
    if macro_name.long(db) == EXPOSE_MACRO_NAME
        && let Ok(None) = db.module_item_by_name(callsite_module_id, macro_name)
    {
        let (content, mapping) = expose_content_and_mapping(db, macro_call_syntax.arguments(db))?;
        let code_mappings: Arc<[CodeMapping]> = [mapping].into();
        let parent_macro_call_data = resolver.macro_call_data;
        let generated_file_long_id = FileLongId::Virtual(VirtualFile {
            parent: Some(macro_call_syntax.stable_ptr(db).untyped().file_id(db)),
            name: macro_name,
            content: SmolStrId::from(db, content),
            code_mappings: code_mappings.clone(),
            kind: FileKind::Module,
            original_item_removed: false,
        });
        db.accumulate_inline_macro_expansion(&generated_file_long_id);
        let generated_file_id = generated_file_long_id.intern(db);
        let macro_call_module =
            ModuleId::MacroCall { id: macro_call_id, generated_file_id, is_expose: true };
        return Ok(MacroCallData {
            macro_call_module: Ok(macro_call_module),
            diagnostics: Default::default(),
            // Defsite and callsite aren't actually used, as it defines nothing in its code.
            defsite_module_id: callsite_module_id,
            callsite_module_id,
            expansion_mappings: code_mappings,
            parent_macro_call_data,
        });
    }
    let mut diagnostics = SemanticDiagnostics::default();
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
                SemanticDiagnosticKind::InlineMacroNotFound(macro_name),
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
            SemanticDiagnosticKind::InlineMacroNoMatchingRule(macro_name),
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
    let generated_file_long_id = FileLongId::Virtual(VirtualFile {
        parent: Some(macro_call_syntax.stable_ptr(db).untyped().file_id(db)),
        name: macro_name,
        content: SmolStrId::from_arcstr(db, &expanded_code.text),
        code_mappings: expanded_code.code_mappings.clone(),
        kind: FileKind::Module,
        original_item_removed: false,
    });
    db.accumulate_inline_macro_expansion(&generated_file_long_id);
    let generated_file_id = generated_file_long_id.intern(db);
    let macro_call_module =
        ModuleId::MacroCall { id: macro_call_id, generated_file_id, is_expose: false };
    Ok(MacroCallData {
        macro_call_module: Ok(macro_call_module),
        diagnostics: diagnostics.build(),
        defsite_module_id,
        callsite_module_id,
        expansion_mappings: expanded_code.code_mappings,
        parent_macro_call_data,
    })
}

/// Query implementation of [MacroCallSemantic::priv_macro_call_data].
#[salsa::tracked(cycle_fn=priv_macro_call_data_cycle, cycle_initial=priv_macro_call_data_initial)]
fn priv_macro_call_data_tracked<'db>(
    db: &'db dyn Database,
    macro_call_id: MacroCallId<'db>,
) -> Maybe<MacroCallData<'db>> {
    priv_macro_call_data(db, macro_call_id)
}

/// The name of the `expose!` macro.
pub const EXPOSE_MACRO_NAME: &str = "expose";

// TODO(eytan-starkware): Return SmolStrId
/// Get the content and mappings for the `expose!` macro call.
pub fn expose_content_and_mapping<'db>(
    db: &'db dyn Database,
    args: ast::TokenTreeNode<'db>,
) -> Maybe<(String, CodeMapping)> {
    let tokens = match args.subtree(db) {
        ast::WrappedTokenTree::Parenthesized(tree) => tree.tokens(db),
        ast::WrappedTokenTree::Braced(tree) => tree.tokens(db),
        ast::WrappedTokenTree::Bracketed(tree) => tree.tokens(db),
        ast::WrappedTokenTree::Missing(_) => return Err(skip_diagnostic()),
    };
    let tokens_node = tokens.as_syntax_node();
    let tokens_span = tokens_node.span(db);
    Ok((
        tokens_node.get_text(db).to_string(),
        CodeMapping {
            span: TextSpan::new_with_width(TextOffset::START, tokens_span.width()),
            origin: CodeOrigin::Start(tokens_span.start),
        },
    ))
}

/// Cycle handling for [MacroCallSemantic::priv_macro_call_data].
fn priv_macro_call_data_cycle<'db>(
    _db: &'db dyn Database,
    _value: &Maybe<MacroCallData<'db>>,
    _count: u32,
    _macro_call_id: MacroCallId<'db>,
) -> salsa::CycleRecoveryAction<Maybe<MacroCallData<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}
/// Cycle handling for [MacroCallSemantic::priv_macro_call_data].
fn priv_macro_call_data_initial<'db>(
    db: &'db dyn Database,
    macro_call_id: MacroCallId<'db>,
) -> Maybe<MacroCallData<'db>> {
    // If we are in a cycle, we return an empty MacroCallData with no diagnostics.
    // This is to prevent infinite recursion in case of cyclic macro calls.
    let mut diagnostics = SemanticDiagnostics::default();
    let macro_call_syntax = db.module_macro_call_by_id(macro_call_id)?;
    let macro_call_path = macro_call_syntax.path(db);
    let macro_name = macro_call_path.as_syntax_node().get_text_without_trivia(db);

    let diag_added = diagnostics.report(
        macro_call_id.stable_ptr(db).untyped(),
        SemanticDiagnosticKind::InlineMacroNotFound(macro_name),
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

/// Implementation of [MacroCallSemantic::macro_call_diagnostics].
fn macro_call_diagnostics<'db>(
    db: &'db dyn Database,
    macro_call_id: MacroCallId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    db.priv_macro_call_data(macro_call_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [MacroCallSemantic::macro_call_diagnostics].
#[salsa::tracked]
fn macro_call_diagnostics_tracked<'db>(
    db: &'db dyn Database,
    macro_call_id: MacroCallId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    macro_call_diagnostics(db, macro_call_id)
}

/// Implementation of [MacroCallSemantic::macro_call_module_id].
fn macro_call_module_id<'db>(
    db: &'db dyn Database,
    macro_call_id: MacroCallId<'db>,
) -> Maybe<ModuleId<'db>> {
    db.priv_macro_call_data(macro_call_id)?.macro_call_module
}

/// Query implementation of [MacroCallSemantic::macro_call_module_id].
#[salsa::tracked(cycle_fn=macro_call_module_id_cycle, cycle_initial=macro_call_module_id_initial)]
fn macro_call_module_id_tracked<'db>(
    db: &'db dyn Database,
    macro_call_id: MacroCallId<'db>,
) -> Maybe<ModuleId<'db>> {
    macro_call_module_id(db, macro_call_id)
}

/// Cycle handling for [MacroCallSemantic::macro_call_module_id].
fn macro_call_module_id_cycle<'db>(
    _db: &'db dyn Database,
    _value: &Maybe<ModuleId<'db>>,
    _count: u32,
    _macro_call_id: MacroCallId<'db>,
) -> salsa::CycleRecoveryAction<Maybe<ModuleId<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}
/// Cycle handling for [MacroCallSemantic::macro_call_module_id].
fn macro_call_module_id_initial<'db>(
    _db: &'db dyn Database,
    _macro_call_id: MacroCallId<'db>,
) -> Maybe<ModuleId<'db>> {
    Err(skip_diagnostic())
}

/// Returns the modules that are considered a part of this module.
///
/// If `include_all` is true, all modules are returned, regardless if exposed, or are the main
/// module.
#[salsa::tracked(returns(ref))]
pub fn module_macro_modules<'db>(
    db: &'db dyn Database,
    include_all: bool,
    module_id: ModuleId<'db>,
) -> Vec<ModuleId<'db>> {
    let mut modules = vec![];
    let mut stack = vec![(module_id, include_all)];
    while let Some((module_id, expose)) = stack.pop() {
        if expose {
            modules.push(module_id);
        }
        if let Ok(macro_calls) = db.module_macro_calls_ids(module_id) {
            for macro_call in macro_calls.iter().rev() {
                let Ok(macro_module_id) = db.macro_call_module_id(*macro_call) else {
                    continue;
                };
                let expose = expose
                    || matches!(macro_module_id, ModuleId::MacroCall { is_expose: true, .. });
                stack.push((macro_module_id, expose));
            }
        }
    }
    modules
}

/// Trait for macro call-related semantic queries.
pub trait MacroCallSemantic<'db>: Database {
    /// Returns the semantic data of a macro call.
    fn priv_macro_call_data(
        &'db self,
        macro_call_id: MacroCallId<'db>,
    ) -> Maybe<MacroCallData<'db>> {
        priv_macro_call_data_tracked(self.as_dyn_database(), macro_call_id)
    }
    /// Returns the expansion result of a macro call.
    fn macro_call_module_id(&'db self, macro_call_id: MacroCallId<'db>) -> Maybe<ModuleId<'db>> {
        macro_call_module_id_tracked(self.as_dyn_database(), macro_call_id)
    }
    /// Returns the semantic diagnostics of a macro call.
    fn macro_call_diagnostics(
        &'db self,
        macro_call_id: MacroCallId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        macro_call_diagnostics_tracked(self.as_dyn_database(), macro_call_id)
    }
}
impl<'db, T: Database + ?Sized> MacroCallSemantic<'db> for T {}
