use std::sync::Arc;

use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::attribute::consts::{IMPLICIT_PRECEDENCE_ATTR, INLINE_ATTR};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeArg, AttributeArgVariant};
use cairo_lang_syntax::node::{ast, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::Upcast;
use id_arena::Arena;
use itertools::Itertools;

use super::functions::InlineConfiguration;
use crate::corelib::try_get_core_ty_by_name;
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics};
use crate::items::functions::ImplicitPrecedence;
use crate::resolve::ResolverData;
use crate::{semantic, ExprId, PatternId, SemanticDiagnostic, TypeId};

// === Declaration ===

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::function_declaration_diagnostics].
pub fn function_declaration_diagnostics(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Diagnostics<SemanticDiagnostic> {
    let declaration_data = match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            db.priv_free_function_declaration_data(free_function_id)
        }
        FunctionWithBodyId::Impl(impl_function_id) => db
            .priv_impl_function_declaration_data(impl_function_id)
            .map(|x| x.function_declaration_data),
    };
    declaration_data.map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::function_declaration_inline_config].
pub fn function_declaration_inline_config(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<InlineConfiguration> {
    match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            db.free_function_declaration_inline_config(free_function_id)
        }
        FunctionWithBodyId::Impl(impl_function_id) => {
            db.impl_function_declaration_inline_config(impl_function_id)
        }
    }
}

/// Query implementation of [SemanticGroup::function_declaration_implicit_precedence].
pub fn function_declaration_implicit_precedence(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<ImplicitPrecedence> {
    match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            db.free_function_declaration_implicit_precedence(free_function_id)
        }
        FunctionWithBodyId::Impl(impl_function_id) => {
            db.impl_function_declaration_implicit_precedence(impl_function_id)
        }
    }
}

/// Query implementation of [crate::db::SemanticGroup::function_with_body_signature].
pub fn function_with_body_signature(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<semantic::Signature> {
    match function_id {
        FunctionWithBodyId::Free(free_function_id) => db.free_function_signature(free_function_id),
        FunctionWithBodyId::Impl(impl_function_id) => db.impl_function_signature(impl_function_id),
    }
}

/// Query implementation of
/// [crate::db::SemanticGroup::function_with_body_generic_params].
pub fn function_with_body_generic_params(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Vec<semantic::GenericParam>> {
    match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            db.free_function_generic_params(free_function_id)
        }
        FunctionWithBodyId::Impl(impl_function_id) => {
            let mut res = db.impl_def_generic_params(impl_function_id.impl_def_id(db.upcast()))?;
            res.extend(db.impl_function_generic_params(impl_function_id)?);
            Ok(res)
        }
    }
}

/// Query implementation of [crate::db::SemanticGroup::function_with_body_attributes].
pub fn function_with_body_attributes(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Vec<Attribute>> {
    match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            Ok(db.priv_free_function_declaration_data(free_function_id)?.attributes)
        }
        FunctionWithBodyId::Impl(impl_function_id) => Ok(db
            .priv_impl_function_declaration_data(impl_function_id)?
            .function_declaration_data
            .attributes),
    }
}

// === Body ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct FunctionBodyData {
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
    pub expr_lookup: UnorderedHashMap<ast::ExprPtr, ExprId>,
    pub pattern_lookup: UnorderedHashMap<ast::PatternPtr, PatternId>,
    pub resolver_data: Arc<ResolverData>,
    pub body: Arc<FunctionBody>,
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct FunctionBody {
    pub exprs: Arena<semantic::Expr>,
    pub patterns: Arena<semantic::Pattern>,
    pub statements: Arena<semantic::Statement>,
    pub body_expr: semantic::ExprId,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::function_body_diagnostics].
pub fn function_body_diagnostics(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Diagnostics<SemanticDiagnostic> {
    let body_data = match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            db.priv_free_function_body_data(free_function_id)
        }
        FunctionWithBodyId::Impl(impl_function_id) => {
            db.priv_impl_function_body_data(impl_function_id)
        }
    };
    body_data.map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::function_body_expr].
pub fn function_body_expr(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<semantic::ExprId> {
    Ok(db.function_body(function_id)?.body_expr)
}

/// Query implementation of [crate::db::SemanticGroup::function_body].
pub fn function_body(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Arc<FunctionBody>> {
    match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            Ok(db.priv_free_function_body_data(free_function_id)?.body)
        }
        FunctionWithBodyId::Impl(impl_function_id) => {
            Ok(db.priv_impl_function_body_data(impl_function_id)?.body)
        }
    }
}

// =========================================================

/// Query implementation of [crate::db::SemanticGroup::expr_semantic].
pub fn expr_semantic(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
    id: semantic::ExprId,
) -> semantic::Expr {
    db.function_body(function_id).unwrap().exprs.get(id).unwrap().clone()
}

/// Query implementation of [crate::db::SemanticGroup::pattern_semantic].
pub fn pattern_semantic(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
    id: semantic::PatternId,
) -> semantic::Pattern {
    db.function_body(function_id).unwrap().patterns.get(id).unwrap().clone()
}

/// Query implementation of [crate::db::SemanticGroup::statement_semantic].
pub fn statement_semantic(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
    id: semantic::StatementId,
) -> semantic::Statement {
    db.function_body(function_id).unwrap().statements.get(id).unwrap().clone()
}

pub trait SemanticExprLookup<'a>: Upcast<dyn SemanticGroup + 'a> {
    fn lookup_expr_by_ptr(
        &self,
        function_id: FunctionWithBodyId,
        ptr: ast::ExprPtr,
    ) -> Maybe<ExprId> {
        let body_data = match function_id {
            FunctionWithBodyId::Free(free_function_id) => {
                self.upcast().priv_free_function_body_data(free_function_id)
            }
            FunctionWithBodyId::Impl(impl_function_id) => {
                self.upcast().priv_impl_function_body_data(impl_function_id)
            }
        };
        body_data?.expr_lookup.get(&ptr).copied().to_maybe()
    }
    fn lookup_pattern_by_ptr(
        &self,
        function_id: FunctionWithBodyId,
        ptr: ast::PatternPtr,
    ) -> Maybe<PatternId> {
        let body_data = match function_id {
            FunctionWithBodyId::Free(free_function_id) => {
                self.upcast().priv_free_function_body_data(free_function_id)
            }
            FunctionWithBodyId::Impl(impl_function_id) => {
                self.upcast().priv_impl_function_body_data(impl_function_id)
            }
        };
        body_data?.pattern_lookup.get(&ptr).copied().to_maybe()
    }
}
impl<'a, T: Upcast<dyn SemanticGroup + 'a> + ?Sized> SemanticExprLookup<'a> for T {}

/// Get the inline configuration of the given function by parsing its attributes.
pub fn get_inline_config(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    attributes: &[Attribute],
) -> Maybe<InlineConfiguration> {
    let mut config = InlineConfiguration::None;
    let mut seen_inline_attr = false;
    for attr in attributes {
        if attr.id != INLINE_ATTR {
            continue;
        }

        match &attr.args[..] {
            [
                AttributeArg {
                    variant: AttributeArgVariant::Unnamed { value: ast::Expr::Path(path), .. },
                    ..
                },
            ] if &path.node.get_text(db.upcast()) == "always" => {
                config = InlineConfiguration::Always(attr.clone());
            }
            [
                AttributeArg {
                    variant: AttributeArgVariant::Unnamed { value: ast::Expr::Path(path), .. },
                    ..
                },
            ] if &path.node.get_text(db.upcast()) == "never" => {
                config = InlineConfiguration::Never(attr.clone());
            }
            [] => {
                config = InlineConfiguration::Should(attr.clone());
            }
            _ => {
                diagnostics.report_by_ptr(
                    attr.args_stable_ptr.untyped(),
                    SemanticDiagnosticKind::UnsupportedInlineArguments,
                );
            }
        }

        if seen_inline_attr {
            diagnostics.report_by_ptr(
                attr.id_stable_ptr.untyped(),
                SemanticDiagnosticKind::RedundantInlineAttribute,
            );
            // If we have multiple inline attributes revert to InlineConfiguration::None.
            config = InlineConfiguration::None;
        }

        seen_inline_attr = true;
    }
    Ok(config)
}

/// Get [ImplicitPrecedence] of the given function by looking at its attributes.
///
/// Returns the generated implicit precedence and the attribute used to get it, if one exists.
/// If there is no implicit precedence influencing attribute, then this function returns
/// [ImplicitPrecedence::UNSPECIFIED].
pub fn get_implicit_precedence<'a>(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    attributes: &'a [Attribute],
) -> Maybe<(ImplicitPrecedence, Option<&'a Attribute>)> {
    let syntax_db = db.upcast();

    let mut attributes = attributes.iter().rev().filter(|attr| attr.id == IMPLICIT_PRECEDENCE_ATTR);

    // Pick the last attribute if any.
    let Some(attr) = attributes.next() else { return Ok((ImplicitPrecedence::UNSPECIFIED, None)) };

    // Report warnings for overridden attributes if any.
    for attr in attributes {
        diagnostics.report_by_ptr(
            attr.id_stable_ptr.untyped(),
            SemanticDiagnosticKind::RedundantImplicitPrecedenceAttribute,
        );
    }

    let types: Vec<TypeId> = attr
        .args
        .iter()
        .map(|arg| match &arg.variant {
            AttributeArgVariant::Unnamed { value, .. } => {
                let ast::Expr::Path(path) = value else {
                    return Err(diagnostics.report(
                        value,
                        SemanticDiagnosticKind::UnsupportedImplicitPrecedenceArguments,
                    ));
                };

                let type_name = path.as_syntax_node().get_text_without_trivia(syntax_db);
                try_get_core_ty_by_name(db, type_name.into(), vec![])
                    .map_err(|kind| diagnostics.report(value, kind))
            }

            _ => Err(diagnostics.report_by_ptr(
                arg.arg_stable_ptr.untyped(),
                SemanticDiagnosticKind::UnsupportedImplicitPrecedenceArguments,
            )),
        })
        .try_collect()?;
    let precedence = ImplicitPrecedence::from_iter(types);

    Ok((precedence, Some(attr)))
}
