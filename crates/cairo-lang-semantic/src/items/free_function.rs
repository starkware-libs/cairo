use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    FreeFunctionId, FunctionTitleId, LanguageElementId, LookupItemId, ModuleItemId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_syntax::attribute::structured::AttributeListStructurize;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::Intern;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use salsa::Database;

use super::function_with_body::{FunctionBody, FunctionBodyData, get_inline_config};
use super::functions::{
    FunctionDeclarationData, GenericFunctionId, InlineConfiguration,
    forbid_inline_always_with_impl_generic_param,
};
use super::generics::{GenericParamsData, semantic_generic_params};
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::{ComputationContext, ContextFunction, Environment, compute_root_expr};
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::items::function_with_body::get_implicit_precedence;
use crate::items::functions::ImplicitPrecedence;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::{FunctionLongId, GenericParam, SemanticDiagnostic, TypeId, semantic};

#[cfg(test)]
#[path = "free_function_test.rs"]
mod test;

// === Declaration ===

// --- Selectors ---

/// Implementation of [FreeFunctionSemantic::free_function_declaration_diagnostics].
fn free_function_declaration_diagnostics<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    db.priv_free_function_declaration_data(free_function_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [FreeFunctionSemantic::free_function_declaration_diagnostics].
#[salsa::tracked]
fn free_function_declaration_diagnostics_tracked<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    free_function_declaration_diagnostics(db, free_function_id)
}

/// Implementation of [FreeFunctionSemantic::free_function_signature].
fn free_function_signature<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<semantic::Signature<'db>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.signature)
}

/// Query implementation of [FreeFunctionSemantic::free_function_signature].
#[salsa::tracked]
fn free_function_signature_tracked<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<semantic::Signature<'db>> {
    free_function_signature(db, free_function_id)
}

/// Implementation of [FreeFunctionSemantic::free_function_declaration_implicits].
fn free_function_declaration_implicits<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<Vec<TypeId<'db>>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.signature.implicits)
}

/// Query implementation of [FreeFunctionSemantic::free_function_declaration_implicits].
#[salsa::tracked]
fn free_function_declaration_implicits_tracked<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<Vec<TypeId<'db>>> {
    free_function_declaration_implicits(db, free_function_id)
}

/// Implementation of [FreeFunctionSemantic::free_function_declaration_implicit_precedence]
fn free_function_declaration_implicit_precedence<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<ImplicitPrecedence<'db>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.implicit_precedence)
}

/// Query implementation of [SemanticGroup::free_function_declaration_implicit_precedence]
#[salsa::tracked]
fn free_function_declaration_implicit_precedence_tracked<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<ImplicitPrecedence<'db>> {
    free_function_declaration_implicit_precedence(db, free_function_id)
}

/// Implementation of [FreeFunctionSemantic::free_function_generic_params].
fn free_function_generic_params<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<Vec<semantic::GenericParam<'db>>> {
    Ok(db.free_function_generic_params_data(free_function_id)?.generic_params)
}

/// Query implementation of [FreeFunctionSemantic::free_function_generic_params].
#[salsa::tracked]
fn free_function_generic_params_tracked<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<Vec<semantic::GenericParam<'db>>> {
    free_function_generic_params(db, free_function_id)
}

/// Implementation of [FreeFunctionSemantic::free_function_generic_params_data].
fn free_function_generic_params_data<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_file_id = free_function_id.module_file_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let free_function_syntax = db.module_free_function_by_id(free_function_id)?;
    let declaration = free_function_syntax.declaration(db);

    // Generic params.
    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(
        ModuleItemId::FreeFunction(free_function_id),
    ));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    resolver.set_feature_config(&free_function_id, &free_function_syntax, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &declaration.generic_params(db),
    );

    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, free_function_syntax.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Query implementation of [FreeFunctionSemantic::free_function_generic_params_data].
#[salsa::tracked]
fn free_function_generic_params_data_tracked<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    free_function_generic_params_data(db, free_function_id)
}

/// Implementation of [FreeFunctionSemantic::free_function_declaration_resolver_data].
fn free_function_declaration_resolver_data<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.resolver_data)
}

/// Query implementation of [FreeFunctionSemantic::free_function_declaration_resolver_data].
#[salsa::tracked]
fn free_function_declaration_resolver_data_tracked<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    free_function_declaration_resolver_data(db, free_function_id)
}

/// Implementation of [FreeFunctionSemantic::free_function_declaration_inline_config].
fn free_function_declaration_inline_config<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<InlineConfiguration<'db>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.inline_config)
}

/// Query implementation of [FreeFunctionSemantic::free_function_declaration_inline_config].
#[salsa::tracked]
fn free_function_declaration_inline_config_tracked<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<InlineConfiguration<'db>> {
    free_function_declaration_inline_config(db, free_function_id)
}

// --- Computation ---

/// Implementation of [FreeFunctionSemantic::priv_free_function_declaration_data].
fn priv_free_function_declaration_data<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<FunctionDeclarationData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    let free_function_syntax = db.module_free_function_by_id(free_function_id)?;
    let declaration = free_function_syntax.declaration(db);

    // Generic params.
    let generic_params_data = db.free_function_generic_params_data(free_function_id)?;
    let generic_params = generic_params_data.generic_params;
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::FreeFunction(free_function_id));
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);

    let mut environment = Environment::empty();

    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &declaration,
        FunctionTitleId::Free(free_function_id),
        &mut environment,
    );

    let attributes = free_function_syntax.attributes(db).structurize(db);

    let inline_config = get_inline_config(db, &mut diagnostics, &attributes)?;

    forbid_inline_always_with_impl_generic_param(&mut diagnostics, &generic_params, &inline_config);

    let (implicit_precedence, _) =
        get_implicit_precedence(db, &mut diagnostics, &mut resolver, &attributes);

    // Check fully resolved.
    let inference = &mut resolver.inference();

    inference.finalize(&mut diagnostics, declaration.stable_ptr(db).untyped());
    let signature = inference.rewrite(signature).no_err();
    let generic_params = inference.rewrite(generic_params).no_err();

    Ok(FunctionDeclarationData {
        diagnostics: diagnostics.build(),
        signature,
        environment,
        generic_params,
        attributes,
        resolver_data: Arc::new(resolver.data),
        inline_config,
        implicit_precedence,
    })
}

/// Query implementation of [FreeFunctionSemantic::priv_free_function_declaration_data].
#[salsa::tracked]
fn priv_free_function_declaration_data_tracked<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<FunctionDeclarationData<'db>> {
    priv_free_function_declaration_data(db, free_function_id)
}

// === Body ===

// --- Selectors ---

/// Implementation of [FreeFunctionSemantic::free_function_body_diagnostics].
fn free_function_body_diagnostics<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    db.priv_free_function_body_data(free_function_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [FreeFunctionSemantic::free_function_body_diagnostics].
#[salsa::tracked]
fn free_function_body_diagnostics_tracked<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    free_function_body_diagnostics(db, free_function_id)
}

/// Implementation of [FreeFunctionSemantic::free_function_body_resolver_data].
fn free_function_body_resolver_data<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    Ok(db.priv_free_function_body_data(free_function_id)?.resolver_data)
}

/// Query implementation of [FreeFunctionSemantic::free_function_body_resolver_data].
#[salsa::tracked]
fn free_function_body_resolver_data_tracked<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    free_function_body_resolver_data(db, free_function_id)
}

// --- Computation ---

/// Implementation of [FreeFunctionSemantic::priv_free_function_body_data].
fn priv_free_function_body_data<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<FunctionBodyData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    let free_function_syntax = db.module_free_function_by_id(free_function_id)?;
    // Compute declaration semantic.
    let declaration = db.priv_free_function_declaration_data(free_function_id)?;

    // Generic params.
    let parent_resolver_data = db.free_function_declaration_resolver_data(free_function_id)?;
    let inference_id = InferenceId::LookupItemDefinition(LookupItemId::ModuleItem(
        ModuleItemId::FreeFunction(free_function_id),
    ));
    let mut resolver =
        Resolver::with_data(db, (*parent_resolver_data).clone_with_inference_id(db, inference_id));

    let environment = declaration.environment;
    let function_id = (|| {
        let generic_function = GenericFunctionId::Free(free_function_id);

        Ok(FunctionLongId::from_generic(db, generic_function)?.intern(db))
    })();
    // Compute body semantic expr.
    let mut ctx = ComputationContext::new(
        db,
        &mut diagnostics,
        &mut resolver,
        Some(&declaration.signature),
        environment,
        ContextFunction::Function(function_id),
    );
    let function_body = free_function_syntax.body(db);
    let return_type = declaration.signature.return_type;
    let body_expr = compute_root_expr(&mut ctx, &function_body, return_type)?;
    let ComputationContext { arenas, .. } = ctx;

    let expr_lookup: UnorderedHashMap<_, _> =
        arenas.exprs.iter().map(|(id, expr)| (expr.stable_ptr(), id)).collect();
    let pattern_lookup: UnorderedHashMap<_, _> =
        arenas.patterns.iter().map(|(id, pattern)| (pattern.stable_ptr(), id)).collect();
    let resolver_data = Arc::new(resolver.data);
    Ok(FunctionBodyData {
        diagnostics: diagnostics.build(),
        expr_lookup,
        pattern_lookup,
        resolver_data,
        body: Arc::new(FunctionBody { arenas, body_expr }),
    })
}

/// Query implementation of [FreeFunctionSemantic::priv_free_function_body_data].
#[salsa::tracked]
fn priv_free_function_body_data_tracked<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<FunctionBodyData<'db>> {
    priv_free_function_body_data(db, free_function_id)
}

/// Trait for free function-related semantic queries.
pub trait FreeFunctionSemantic<'db>: Database {
    /// Returns the semantic diagnostics of a free function's declaration (signature).
    fn free_function_declaration_diagnostics(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        free_function_declaration_diagnostics_tracked(self.as_dyn_database(), free_function_id)
    }
    /// Returns the signature of a free function.
    fn free_function_signature(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>> {
        free_function_signature_tracked(self.as_dyn_database(), free_function_id)
    }
    /// Returns the explicit implicits of a signature of a free function.
    fn free_function_declaration_implicits(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<Vec<TypeId<'db>>> {
        free_function_declaration_implicits_tracked(self.as_dyn_database(), free_function_id)
    }
    /// Returns the implicits precedence of a free function.
    fn free_function_declaration_implicit_precedence(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<ImplicitPrecedence<'db>> {
        free_function_declaration_implicit_precedence_tracked(
            self.as_dyn_database(),
            free_function_id,
        )
    }
    /// Returns the generic params of a free function.
    fn free_function_generic_params(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>> {
        free_function_generic_params_tracked(self.as_dyn_database(), free_function_id)
    }
    /// Returns the generic params data of a free function.
    fn free_function_generic_params_data(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<GenericParamsData<'db>> {
        free_function_generic_params_data_tracked(self.as_dyn_database(), free_function_id)
    }
    /// Returns the resolution resolved_items of a free function's declaration.
    fn free_function_declaration_resolver_data(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        free_function_declaration_resolver_data_tracked(self.as_dyn_database(), free_function_id)
    }
    /// Returns the inline configuration of a free function's declaration.
    fn free_function_declaration_inline_config(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<InlineConfiguration<'db>> {
        free_function_declaration_inline_config_tracked(self.as_dyn_database(), free_function_id)
    }
    /// Private query to compute data about a free function declaration - its signature excluding
    /// its body.
    fn priv_free_function_declaration_data(
        &'db self,
        function_id: FreeFunctionId<'db>,
    ) -> Maybe<FunctionDeclarationData<'db>> {
        priv_free_function_declaration_data_tracked(self.as_dyn_database(), function_id)
    }

    /// Returns the semantic diagnostics of a free function's body.
    fn free_function_body_diagnostics(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        free_function_body_diagnostics_tracked(self.as_dyn_database(), free_function_id)
    }
    /// Returns the resolution resolved_items of a free function's body.
    fn free_function_body_resolver_data(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        free_function_body_resolver_data_tracked(self.as_dyn_database(), free_function_id)
    }
    /// Private query to compute data about a free function's body.
    fn priv_free_function_body_data(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<FunctionBodyData<'db>> {
        priv_free_function_body_data_tracked(self.as_dyn_database(), free_function_id)
    }
}
impl<'db, T: Database + ?Sized> FreeFunctionSemantic<'db> for T {}
