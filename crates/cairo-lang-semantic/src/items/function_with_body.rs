use std::sync::Arc;

use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_diagnostics::{DiagnosticAdded, Diagnostics, Maybe, ToMaybe};
use cairo_lang_filesystem::ids::Tracked;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::attribute::consts::{IMPLICIT_PRECEDENCE_ATTR, INLINE_ATTR};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeArg, AttributeArgVariant};
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::try_extract_matches;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::{Itertools, chain};
use salsa::Database;

use super::functions::InlineConfiguration;
use crate::diagnostic::{
    NotFoundItemType, SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder,
};
use crate::items::free_function::FreeFunctionSemantic;
use crate::items::functions::ImplicitPrecedence;
use crate::items::imp::ImplSemantic;
use crate::items::trt::TraitSemantic;
use crate::resolve::{ResolvedConcreteItem, Resolver, ResolverData};
use crate::{Arenas, ExprId, PatternId, SemanticDiagnostic, TypeId, semantic};

/// Query implementation of [FunctionWithBodySemantic::function_with_body_generic_params].
#[salsa::tracked]
fn function_with_body_generic_params<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    function_id: FunctionWithBodyId<'db>,
) -> Maybe<Vec<semantic::GenericParam<'db>>> {
    Ok(match function_id {
        FunctionWithBodyId::Free(free_function_id) => {
            db.free_function_generic_params(free_function_id)?.to_vec()
        }
        FunctionWithBodyId::Impl(impl_function_id) => chain!(
            db.impl_def_generic_params(impl_function_id.impl_def_id(db))?,
            db.impl_function_generic_params(impl_function_id)?
        )
        .cloned()
        .collect(),
        FunctionWithBodyId::Trait(trait_function_id) => chain!(
            db.trait_generic_params(trait_function_id.trait_id(db))?,
            db.trait_function_generic_params(trait_function_id)?
        )
        .cloned()
        .collect(),
    })
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn Database)]
pub struct FunctionBodyData<'db> {
    pub diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    pub expr_lookup: UnorderedHashMap<ast::ExprPtr<'db>, ExprId>,
    pub pattern_lookup: UnorderedHashMap<ast::PatternPtr<'db>, PatternId>,
    pub resolver_data: Arc<ResolverData<'db>>,
    pub body: FunctionBody<'db>,
}

unsafe impl<'db> salsa::Update for FunctionBodyData<'db> {
    // Using existing salsa::Update implementations for the fields.
    // For lookups we assume they are built from the arena,
    // so a change will be detected and they will be copied.
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_value = unsafe { &mut *old_pointer };
        let res = unsafe {
            Diagnostics::maybe_update(&mut old_value.diagnostics, new_value.diagnostics)
                | Arc::maybe_update(&mut old_value.resolver_data, new_value.resolver_data)
                | FunctionBody::maybe_update(&mut old_value.body, new_value.body)
        };
        if res {
            old_value.expr_lookup = new_value.expr_lookup;
            old_value.pattern_lookup = new_value.pattern_lookup;
            return true;
        }
        false
    }
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn Database)]
pub struct FunctionBody<'db> {
    pub arenas: Arenas<'db>,
    pub body_expr: semantic::ExprId,
}

unsafe impl<'db> salsa::Update for FunctionBody<'db> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        // The function body contains both the arena and the expr id, so a change will be detected.
        // The comparison should still be safe to do as we won't follow expired references.
        let old_value = unsafe { &mut *old_pointer };

        if old_value != &new_value {
            *old_value = new_value;
            return true;
        }

        false
    }
}

pub trait SemanticExprLookup<'db>: Database {
    fn lookup_expr_by_ptr(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
        ptr: ast::ExprPtr<'db>,
    ) -> Maybe<ExprId> {
        let body_data = match function_id {
            FunctionWithBodyId::Free(id) => self.priv_free_function_body_data(id)?,
            FunctionWithBodyId::Impl(id) => self.priv_impl_function_body_data(id)?,
            FunctionWithBodyId::Trait(id) => {
                self.priv_trait_function_body_data(id)?.ok_or(DiagnosticAdded)?
            }
        };
        body_data.expr_lookup.get(&ptr).copied().to_maybe()
    }
    fn lookup_pattern_by_ptr(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
        ptr: ast::PatternPtr<'db>,
    ) -> Maybe<PatternId> {
        let body_data = match function_id {
            FunctionWithBodyId::Free(id) => self.priv_free_function_body_data(id)?,
            FunctionWithBodyId::Impl(id) => self.priv_impl_function_body_data(id)?,
            FunctionWithBodyId::Trait(id) => {
                self.priv_trait_function_body_data(id)?.ok_or(DiagnosticAdded)?
            }
        };
        body_data.pattern_lookup.get(&ptr).copied().to_maybe()
    }
}
impl<'db, T: Database + ?Sized> SemanticExprLookup<'db> for T {}

/// Get the inline configuration of the given function by parsing its attributes.
pub fn get_inline_config<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    attributes: &[Attribute<'db>],
) -> Maybe<InlineConfiguration<'db>> {
    let mut config = InlineConfiguration::None;
    let mut seen_inline_attr = false;
    for attr in attributes {
        if attr.id.long(db) != INLINE_ATTR {
            continue;
        }

        match &attr.args[..] {
            [
                AttributeArg {
                    variant: AttributeArgVariant::Unnamed(ast::Expr::Path(path)), ..
                },
            ] if path.as_syntax_node().get_text(db) == "always" => {
                config = InlineConfiguration::Always(attr.stable_ptr);
            }
            [
                AttributeArg {
                    variant: AttributeArgVariant::Unnamed(ast::Expr::Path(path)), ..
                },
            ] if path.as_syntax_node().get_text(db) == "never" => {
                config = InlineConfiguration::Never(attr.stable_ptr);
            }
            [] => {
                config = InlineConfiguration::Should(attr.stable_ptr);
            }
            _ => {
                diagnostics.report(
                    attr.args_stable_ptr.untyped(),
                    SemanticDiagnosticKind::UnsupportedInlineArguments,
                );
            }
        }

        if seen_inline_attr {
            diagnostics.report(
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
pub fn get_implicit_precedence<'a, 'r>(
    db: &'a dyn Database,
    diagnostics: &mut SemanticDiagnostics<'a>,
    resolver: &mut Resolver<'a>,
    attributes: &'r [Attribute<'a>],
) -> (ImplicitPrecedence<'a>, Option<&'r Attribute<'a>>) {
    let mut attributes =
        attributes.iter().rev().filter(|attr| attr.id.long(db) == IMPLICIT_PRECEDENCE_ATTR);

    // Pick the last attribute if any.
    let Some(attr) = attributes.next() else { return (ImplicitPrecedence::UNSPECIFIED, None) };

    // Report warnings for overridden attributes if any.
    for attr in attributes {
        diagnostics.report(
            attr.id_stable_ptr,
            SemanticDiagnosticKind::RedundantImplicitPrecedenceAttribute,
        );
    }

    let Ok(types) =
        attr.args
            .iter()
            .map(|arg| match &arg.variant {
                AttributeArgVariant::Unnamed(value) => {
                    let ast::Expr::Path(path) = value else {
                        return Err(diagnostics.report(
                            value.stable_ptr(db),
                            SemanticDiagnosticKind::UnsupportedImplicitPrecedenceArguments,
                        ));
                    };

                    resolver
                        .resolve_concrete_path(diagnostics, path, NotFoundItemType::Type)
                        .and_then(|resolved_item: crate::resolve::ResolvedConcreteItem<'_>| {
                            try_extract_matches!(resolved_item, ResolvedConcreteItem::Type)
                                .ok_or_else(|| {
                                    diagnostics.report(
                                        value.stable_ptr(db),
                                        SemanticDiagnosticKind::UnknownType,
                                    )
                                })
                        })
                }

                _ => Err(diagnostics.report(
                    arg.arg.stable_ptr(db),
                    SemanticDiagnosticKind::UnsupportedImplicitPrecedenceArguments,
                )),
            })
            .try_collect::<TypeId<'_>, Vec<_>, _>()
    else {
        return (ImplicitPrecedence::UNSPECIFIED, None);
    };

    let precedence = ImplicitPrecedence::from_iter(types);

    (precedence, Some(attr))
}

/// Trait for function-with-body-related semantic queries.
pub trait FunctionWithBodySemantic<'db>: Database {
    /// Returns the semantic diagnostics of a declaration (signature) of a function with a body.
    fn function_declaration_diagnostics(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        match function_id {
            FunctionWithBodyId::Free(id) => self.free_function_declaration_diagnostics(id),
            FunctionWithBodyId::Impl(id) => self.impl_function_declaration_diagnostics(id),
            FunctionWithBodyId::Trait(id) => self.trait_function_declaration_diagnostics(id),
        }
    }
    /// Returns the inline configuration of a declaration (signature) of a function with a body.
    fn function_declaration_inline_config(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<InlineConfiguration<'db>> {
        match function_id {
            FunctionWithBodyId::Free(id) => self.free_function_declaration_inline_config(id),
            FunctionWithBodyId::Impl(id) => self.impl_function_declaration_inline_config(id),
            FunctionWithBodyId::Trait(id) => self.trait_function_declaration_inline_config(id),
        }
    }
    /// Returns the implicit order of a declaration (signature) of a function with a body.
    fn function_declaration_implicit_precedence(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<&'db ImplicitPrecedence<'db>> {
        match function_id {
            FunctionWithBodyId::Free(id) => self.free_function_declaration_implicit_precedence(id),
            FunctionWithBodyId::Impl(id) => self.impl_function_declaration_implicit_precedence(id),
            FunctionWithBodyId::Trait(id) => {
                self.trait_function_declaration_implicit_precedence(id)
            }
        }
    }
    /// Returns the signature of a function with a body.
    fn function_with_body_signature(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<&'db semantic::Signature<'db>> {
        match function_id {
            FunctionWithBodyId::Free(id) => self.free_function_signature(id),
            FunctionWithBodyId::Impl(id) => self.impl_function_signature(id),
            FunctionWithBodyId::Trait(id) => self.trait_function_signature(id),
        }
    }
    /// Returns all the available generic params inside a function body.
    fn function_with_body_generic_params(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<Vec<semantic::GenericParam<'db>>> {
        function_with_body_generic_params(self.as_dyn_database(), (), function_id)
    }
    /// Returns the attributes of a function with a body.
    fn function_with_body_attributes(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<&'db [Attribute<'db>]> {
        match function_id {
            FunctionWithBodyId::Free(id) => self.free_function_attributes(id),
            FunctionWithBodyId::Impl(id) => self.impl_function_attributes(id),
            FunctionWithBodyId::Trait(id) => self.trait_function_attributes(id),
        }
    }
    /// Returns the semantic diagnostics of a body of a function (with a body).
    fn function_body_diagnostics(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        match function_id {
            FunctionWithBodyId::Free(id) => self.free_function_body_diagnostics(id),
            FunctionWithBodyId::Impl(id) => self.impl_function_body_diagnostics(id),
            FunctionWithBodyId::Trait(id) => self.trait_function_body_diagnostics(id),
        }
    }
    /// Returns the body of a function (with a body).
    fn function_body(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<&'db FunctionBody<'db>> {
        match function_id {
            FunctionWithBodyId::Free(id) => self.free_function_body(id),
            FunctionWithBodyId::Impl(id) => self.impl_function_body(id),
            FunctionWithBodyId::Trait(id) => self.trait_function_body(id)?.ok_or(DiagnosticAdded),
        }
    }
    /// Returns the body expr of a function (with a body).
    fn function_body_expr(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
    ) -> Maybe<semantic::ExprId> {
        Ok(self.function_body(function_id)?.body_expr)
    }
    /// Assumes function and expression are present.
    fn expr_semantic(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
        id: semantic::ExprId,
    ) -> semantic::Expr<'db> {
        self.function_body(function_id).unwrap().arenas.exprs.get(id).unwrap().clone()
    }
    /// Assumes function and statement are valid.
    fn statement_semantic(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
        id: semantic::StatementId,
    ) -> semantic::Statement<'db> {
        self.function_body(function_id).unwrap().arenas.statements.get(id).unwrap().clone()
    }
    /// Assumes function and pattern are present.
    fn pattern_semantic(
        &'db self,
        function_id: FunctionWithBodyId<'db>,
        id: semantic::PatternId,
    ) -> semantic::Pattern<'db> {
        self.function_body(function_id).unwrap().arenas.patterns.get(id).unwrap().clone()
    }
}
impl<'db, T: Database + ?Sized> FunctionWithBodySemantic<'db> for T {}
