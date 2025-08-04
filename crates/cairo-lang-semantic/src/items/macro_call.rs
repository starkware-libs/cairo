use cairo_lang_defs::ids::{LanguageElementId, MacroCallId, MacroDeclarationId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};

use crate::SemanticDiagnostic;
use crate::db::SemanticGroup;
use crate::diagnostic::{
    NotFoundItemType, SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder,
};
use crate::expr::inference::InferenceId;
use crate::resolve::{ResolutionContext, ResolvedGenericItem, Resolver};

/// The data associated with a macro call in item context.
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub struct MacroCallData<'db> {
    /// The macro declaration that this macro call refers to, if found.
    pub macro_declaration_id: Option<MacroDeclarationId<'db>>,
    pub diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
}

/// Query implementation of [crate::db::SemanticGroup::priv_macro_call_data].
pub fn priv_macro_call_data<'db>(
    db: &'db dyn SemanticGroup,
    macro_call_id: MacroCallId<'db>,
) -> Maybe<MacroCallData<'db>> {
    let inference_id = InferenceId::MacroCall(macro_call_id);
    let module_file_id = macro_call_id.module_file_id(db);
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    let mut diagnostics = SemanticDiagnostics::default();
    let macro_call_syntax = db.module_macro_call_by_id(macro_call_id)?.to_maybe()?;
    // Resolve the macro call path, and report diagnostics if it finds no match or
    // the resolved item is not a macro declaration.
    let macro_call_path = macro_call_syntax.path(db);
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
                SemanticDiagnosticKind::MacroCallToNotAMacro(
                    macro_call_path.as_syntax_node().get_text_without_trivia(db).into(),
                ),
            );
            None
        }
        Err(_) => {
            // Resolver error, the diagnostics should already have been reported in the resolver.
            None
        }
    };

    Ok(MacroCallData { macro_declaration_id, diagnostics: diagnostics.build() })
}

/// Query implementation of [crate::db::SemanticGroup::macro_call_diagnostics].
pub fn macro_call_diagnostics<'db>(
    db: &'db dyn SemanticGroup,
    macro_call_id: MacroCallId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    priv_macro_call_data(db, macro_call_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::macro_call_declaration_id].
pub fn macro_call_declaration_id<'db>(
    db: &'db dyn SemanticGroup,
    macro_call_id: MacroCallId<'db>,
) -> Maybe<Option<MacroDeclarationId<'db>>> {
    priv_macro_call_data(db, macro_call_id).map(|data| data.macro_declaration_id)
}
