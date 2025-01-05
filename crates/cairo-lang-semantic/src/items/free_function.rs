use std::sync::Arc;

use cairo_lang_defs::ids::{
    FreeFunctionId, FunctionTitleId, LanguageElementId, LookupItemId, ModuleItemId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_syntax::attribute::structured::AttributeListStructurize;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::Intern;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use super::function_with_body::{FunctionBody, FunctionBodyData, get_inline_config};
use super::functions::{
    FunctionDeclarationData, GenericFunctionId, InlineConfiguration,
    forbid_inline_always_with_impl_generic_param,
};
use super::generics::{GenericParamsData, semantic_generic_params};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::{ComputationContext, ContextFunction, Environment, compute_root_expr};
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::items::function_with_body::get_implicit_precedence;
use crate::items::functions::ImplicitPrecedence;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::{Arenas, FunctionLongId, SemanticDiagnostic, TypeId, semantic};

#[cfg(test)]
#[path = "free_function_test.rs"]
mod test;

// === Declaration ===

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_diagnostics].
pub fn free_function_declaration_diagnostics(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_free_function_declaration_data(free_function_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::free_function_signature].
pub fn free_function_signature(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<semantic::Signature> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.signature)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_implicits].
pub fn free_function_declaration_implicits(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Vec<TypeId>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.signature.implicits)
}

/// Query implementation of [SemanticGroup::free_function_declaration_implicit_precedence]
pub fn free_function_declaration_implicit_precedence(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<ImplicitPrecedence> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.implicit_precedence)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_generic_params].
pub fn free_function_generic_params(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Vec<semantic::GenericParam>> {
    Ok(db.free_function_generic_params_data(free_function_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_generic_params_data].
pub fn free_function_generic_params_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<GenericParamsData> {
    let syntax_db = db.upcast();
    let module_file_id = free_function_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::default();
    let free_function_syntax = db.module_free_function_by_id(free_function_id)?.to_maybe()?;
    let declaration = free_function_syntax.declaration(syntax_db);

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
        &declaration.generic_params(syntax_db),
    );

    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, free_function_syntax.stable_ptr().untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_resolver_data].
pub fn free_function_declaration_resolver_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_inline_config].
pub fn free_function_declaration_inline_config(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<InlineConfiguration> {
    Ok(db.priv_free_function_declaration_data(free_function_id)?.inline_config)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_free_function_declaration_data].
pub fn priv_free_function_declaration_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<FunctionDeclarationData> {
    let syntax_db = db.upcast();
    let mut diagnostics = SemanticDiagnostics::default();
    let free_function_syntax = db.module_free_function_by_id(free_function_id)?.to_maybe()?;
    let declaration = free_function_syntax.declaration(syntax_db);

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

    let attributes = free_function_syntax.attributes(syntax_db).structurize(syntax_db);

    let inline_config = get_inline_config(db, &mut diagnostics, &attributes)?;

    forbid_inline_always_with_impl_generic_param(&mut diagnostics, &generic_params, &inline_config);

    let (implicit_precedence, _) =
        get_implicit_precedence(&mut diagnostics, &mut resolver, &attributes);

    // Check fully resolved.
    let inference = &mut resolver.inference();

    inference.finalize(&mut diagnostics, declaration.stable_ptr().untyped());
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

// === Body ===

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::free_function_body_diagnostics].
pub fn free_function_body_diagnostics(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_free_function_body_data(free_function_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::free_function_body_resolver_data].
pub fn free_function_body_resolver_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_free_function_body_data(free_function_id)?.resolver_data)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_free_function_body_data].
pub fn priv_free_function_body_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<FunctionBodyData> {
    let mut diagnostics = SemanticDiagnostics::default();
    let free_function_syntax = db.module_free_function_by_id(free_function_id)?.to_maybe()?;
    // Compute declaration semantic.
    let declaration = db.priv_free_function_declaration_data(free_function_id)?;

    // Generic params.
    let parent_resolver_data = db.free_function_declaration_resolver_data(free_function_id)?;
    let inference_id = InferenceId::LookupItemDefinition(LookupItemId::ModuleItem(
        ModuleItemId::FreeFunction(free_function_id),
    ));
    let resolver =
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
        resolver,
        Some(&declaration.signature),
        environment,
        ContextFunction::Function(function_id),
    );
    let function_body = free_function_syntax.body(db.upcast());
    let return_type = declaration.signature.return_type;
    let body_expr = compute_root_expr(&mut ctx, &function_body, return_type)?;
    let ComputationContext { arenas: Arenas { exprs, patterns, statements }, resolver, .. } = ctx;

    let expr_lookup: UnorderedHashMap<_, _> =
        exprs.iter().map(|(expr_id, expr)| (expr.stable_ptr(), expr_id)).collect();
    let pattern_lookup: UnorderedHashMap<_, _> =
        patterns.iter().map(|(pattern_id, pattern)| (pattern.stable_ptr(), pattern_id)).collect();
    let resolver_data = Arc::new(resolver.data);
    Ok(FunctionBodyData {
        diagnostics: diagnostics.build(),
        expr_lookup,
        pattern_lookup,
        resolver_data,
        body: Arc::new(FunctionBody { arenas: Arenas { exprs, patterns, statements }, body_expr }),
    })
}
