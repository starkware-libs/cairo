use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    FreeFunctionId, FunctionTitleId, LanguageElementId, LookupItemId, ModuleItemId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, MaybeAsRef};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
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
use crate::{FunctionLongId, GenericParam, SemanticDiagnostic, semantic};

#[cfg(test)]
#[path = "free_function_test.rs"]
mod test;

/// Returns the generic params data of a free function.
#[salsa::tracked(returns(ref))]
fn free_function_generic_params_data<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_id = free_function_id.module_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let free_function_syntax = db.module_free_function_by_id(free_function_id)?;
    let declaration = free_function_syntax.declaration(db);

    // Generic params.
    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(
        ModuleItemId::FreeFunction(free_function_id),
    ));
    let mut resolver = Resolver::new(db, module_id, inference_id);
    resolver.set_feature_config(&free_function_id, &free_function_syntax, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_id,
        &declaration.generic_params(db),
    );

    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, free_function_syntax.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Returns data about a free function declaration - its signature excluding its body.
#[salsa::tracked(returns(ref))]
fn free_function_declaration_data<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<FunctionDeclarationData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    let free_function_syntax = db.module_free_function_by_id(free_function_id)?;
    let declaration = free_function_syntax.declaration(db);

    // Generic params.
    let generic_params_data =
        free_function_generic_params_data(db, free_function_id).maybe_as_ref()?;
    let generic_params = &generic_params_data.generic_params;
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::FreeFunction(free_function_id));
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics.clone());

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

    forbid_inline_always_with_impl_generic_param(&mut diagnostics, generic_params, &inline_config);

    let (implicit_precedence, _) =
        get_implicit_precedence(db, &mut diagnostics, &mut resolver, &attributes);

    // Check fully resolved.
    let inference = &mut resolver.inference();

    inference.finalize(&mut diagnostics, declaration.stable_ptr(db).untyped());
    let signature = inference.rewrite(signature).no_err();

    Ok(FunctionDeclarationData {
        diagnostics: diagnostics.build(),
        signature,
        environment,
        attributes,
        resolver_data: Arc::new(resolver.data),
        inline_config,
        implicit_precedence,
    })
}

/// Query implementation of [FreeFunctionSemantic::priv_free_function_body_data].
#[salsa::tracked(returns(ref))]
fn priv_free_function_body_data<'db>(
    db: &'db dyn Database,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<FunctionBodyData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    let free_function_syntax = db.module_free_function_by_id(free_function_id)?;
    // Compute declaration semantic.
    let declaration = free_function_declaration_data(db, free_function_id).maybe_as_ref()?;

    // Generic params.
    let parent_resolver_data = db.free_function_declaration_resolver_data(free_function_id)?;
    let inference_id = InferenceId::LookupItemDefinition(LookupItemId::ModuleItem(
        ModuleItemId::FreeFunction(free_function_id),
    ));
    let mut resolver =
        Resolver::with_data(db, (*parent_resolver_data).clone_with_inference_id(db, inference_id));

    let environment = declaration.environment.clone();
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
        body: FunctionBody { arenas, body_expr },
    })
}

/// Trait for free function-related semantic queries.
pub trait FreeFunctionSemantic<'db>: Database {
    /// Returns the semantic diagnostics of a free function's declaration (signature).
    fn free_function_declaration_diagnostics(
        &'db self,
        id: FreeFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        free_function_declaration_data(self.as_dyn_database(), id)
            .as_ref()
            .map(|data| data.diagnostics.clone())
            .unwrap_or_default()
    }
    /// Returns the signature of a free function.
    fn free_function_signature(
        &'db self,
        id: FreeFunctionId<'db>,
    ) -> Maybe<&'db semantic::Signature<'db>> {
        Ok(&free_function_declaration_data(self.as_dyn_database(), id).maybe_as_ref()?.signature)
    }
    /// Returns the implicits precedence of a free function.
    fn free_function_declaration_implicit_precedence(
        &'db self,
        id: FreeFunctionId<'db>,
    ) -> Maybe<&'db ImplicitPrecedence<'db>> {
        Ok(&free_function_declaration_data(self.as_dyn_database(), id)
            .maybe_as_ref()?
            .implicit_precedence)
    }
    /// Returns the generic params of a free function.
    fn free_function_generic_params(
        &'db self,
        id: FreeFunctionId<'db>,
    ) -> Maybe<&'db [GenericParam<'db>]> {
        Ok(&free_function_generic_params_data(self.as_dyn_database(), id)
            .maybe_as_ref()?
            .generic_params)
    }
    /// Returns the attributes of a free function.
    fn free_function_attributes(
        &'db self,
        id: FreeFunctionId<'db>,
    ) -> Maybe<&'db [Attribute<'db>]> {
        Ok(&free_function_declaration_data(self.as_dyn_database(), id).maybe_as_ref()?.attributes)
    }
    /// Returns the resolution resolved_items of a free function's declaration.
    fn free_function_declaration_resolver_data(
        &'db self,
        id: FreeFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        Ok(free_function_declaration_data(self.as_dyn_database(), id)
            .maybe_as_ref()?
            .resolver_data
            .clone())
    }
    /// Returns the inline configuration of a free function's declaration.
    fn free_function_declaration_inline_config(
        &'db self,
        id: FreeFunctionId<'db>,
    ) -> Maybe<InlineConfiguration<'db>> {
        Ok(free_function_declaration_data(self.as_dyn_database(), id)
            .maybe_as_ref()?
            .inline_config
            .clone())
    }
    /// Returns the semantic diagnostics of a free function's body.
    fn free_function_body_diagnostics(
        &'db self,
        id: FreeFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        self.priv_free_function_body_data(id)
            .map(|data| data.diagnostics.clone())
            .unwrap_or_default()
    }
    /// Returns the definition of a free function.
    fn free_function_body(&'db self, id: FreeFunctionId<'db>) -> Maybe<&'db FunctionBody<'db>> {
        Ok(&self.priv_free_function_body_data(id)?.body)
    }
    /// Returns the resolution resolved_items of a free function's body.
    fn free_function_body_resolver_data(
        &'db self,
        id: FreeFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        Ok(self.priv_free_function_body_data(id)?.resolver_data.clone())
    }
    /// Returns the semantic body of a free function.
    fn priv_free_function_body_data(
        &'db self,
        id: FreeFunctionId<'db>,
    ) -> Maybe<&'db FunctionBodyData<'db>> {
        priv_free_function_body_data(self.as_dyn_database(), id).maybe_as_ref()
    }
}
impl<'db, T: Database + ?Sized> FreeFunctionSemantic<'db> for T {}
