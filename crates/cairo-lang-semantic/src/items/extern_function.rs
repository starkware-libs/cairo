use std::sync::Arc;

use cairo_lang_defs::ids::{
    ExternFunctionId, FunctionTitleId, GenericKind, LanguageElementId, LookupItemId, ModuleItemId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_syntax::attribute::structured::AttributeListStructurize;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::extract_matches;

use super::function_with_body::get_inline_config;
use super::functions::{FunctionDeclarationData, GenericFunctionId, InlineConfiguration};
use super::generics::{semantic_generic_params, GenericParamsData};
use crate::corelib::get_core_generic_function_id;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::Environment;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::InferenceId;
use crate::items::function_with_body::get_implicit_precedence;
use crate::items::functions::ImplicitPrecedence;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::{semantic, Mutability, Parameter, SemanticDiagnostic, TypeId};

#[cfg(test)]
#[path = "extern_function_test.rs"]
mod test;

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::extern_function_declaration_inline_config].
pub fn extern_function_declaration_inline_config(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<InlineConfiguration> {
    Ok(db.priv_extern_function_declaration_data(extern_function_id)?.inline_config)
}
// TODO(spapini): Remove declaration from the names.
/// Query implementation of [crate::db::SemanticGroup::extern_function_declaration_diagnostics].
pub fn extern_function_declaration_diagnostics(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_extern_function_declaration_data(extern_function_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}
/// Query implementation of [crate::db::SemanticGroup::extern_function_signature].
pub fn extern_function_signature(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<semantic::Signature> {
    Ok(db.priv_extern_function_declaration_data(extern_function_id)?.signature)
}
/// Query implementation of [crate::db::SemanticGroup::extern_function_declaration_generic_params].
pub fn extern_function_declaration_generic_params(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<Vec<semantic::GenericParam>> {
    Ok(db.extern_function_declaration_generic_params_data(extern_function_id)?.generic_params)
}

/// Query implementation of
/// [crate::db::SemanticGroup::extern_function_declaration_generic_params_data].
pub fn extern_function_declaration_generic_params_data(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<GenericParamsData> {
    let syntax_db = db.upcast();
    let module_file_id = extern_function_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let module_extern_functions = db.module_extern_functions(module_file_id.0)?;
    let function_syntax = module_extern_functions.get(&extern_function_id).to_maybe()?;
    let declaration = function_syntax.declaration(syntax_db);

    // Generic params.
    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(
        ModuleItemId::ExternFunction(extern_function_id),
    ));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &declaration.generic_params(syntax_db),
    )?;
    if let Some(param) = generic_params.iter().find(|param| param.kind() == GenericKind::Impl) {
        diagnostics.report_by_ptr(
            param.stable_ptr(db.upcast()).untyped(),
            ExternItemWithImplGenericsNotSupported,
        );
    }
    resolver.inference().finalize().map(|(_, inference_err)| {
        inference_err.report(&mut diagnostics, function_syntax.stable_ptr().untyped())
    });
    let generic_params = resolver.inference().rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::extern_function_declaration_implicits].
pub fn extern_function_declaration_implicits(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<Vec<TypeId>> {
    Ok(db.priv_extern_function_declaration_data(extern_function_id)?.signature.implicits)
}

/// Query implementation of [crate::db::SemanticGroup::extern_function_declaration_refs].
pub fn extern_function_declaration_refs(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<Vec<Parameter>> {
    Ok(db
        .priv_extern_function_declaration_data(extern_function_id)?
        .signature
        .params
        .into_iter()
        .filter(|param| param.mutability == Mutability::Reference)
        .collect())
}

/// Query implementation of
/// [crate::db::SemanticGroup::extern_function_declaration_resolver_data].
pub fn extern_function_declaration_resolver_data(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_extern_function_declaration_data(extern_function_id)?.resolver_data)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_extern_function_declaration_data].
pub fn priv_extern_function_declaration_data(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<FunctionDeclarationData> {
    let syntax_db = db.upcast();
    let module_file_id = extern_function_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let module_extern_functions = db.module_extern_functions(module_file_id.0)?;
    let function_syntax = module_extern_functions.get(&extern_function_id).to_maybe()?;
    let declaration = function_syntax.declaration(syntax_db);

    // Generic params.
    let generic_params_data =
        db.extern_function_declaration_generic_params_data(extern_function_id)?;
    let generic_params = generic_params_data.generic_params;
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
        ModuleItemId::ExternFunction(extern_function_id),
    ));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.diagnostics.extend(generic_params_data.diagnostics);

    let mut environment = Environment::default();
    let signature_syntax = declaration.signature(syntax_db);
    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &signature_syntax,
        FunctionTitleId::Extern(extern_function_id),
        &mut environment,
    );

    if signature.panicable {
        let panic_function = extract_matches!(
            get_core_generic_function_id(db.upcast(), "panic".into()),
            GenericFunctionId::Extern
        );
        if extern_function_id != panic_function {
            diagnostics.report(function_syntax, PanicableExternFunction);
        }
    }

    let attributes = function_syntax.attributes(syntax_db).structurize(syntax_db);
    let inline_config = get_inline_config(db, &mut diagnostics, &attributes)?;

    match &inline_config {
        InlineConfiguration::None => {}
        InlineConfiguration::Always(attr)
        | InlineConfiguration::Never(attr)
        | InlineConfiguration::Should(attr) => {
            diagnostics
                .report_by_ptr(attr.stable_ptr.untyped(), InlineAttrForExternFunctionNotAllowed);
        }
    }

    let (_, implicit_precedence_attr) = get_implicit_precedence(db, &mut diagnostics, &attributes)?;
    if let Some(attr) = implicit_precedence_attr {
        diagnostics.report_by_ptr(
            attr.stable_ptr.untyped(),
            ImplicitPrecedenceAttrForExternFunctionNotAllowed,
        );
    }

    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = resolver.inference().finalize() {
        inference_err
            .report(&mut diagnostics, stable_ptr.unwrap_or(function_syntax.stable_ptr().untyped()));
    }
    let signature = resolver.inference().rewrite(signature).no_err();
    let generic_params = resolver.inference().rewrite(generic_params).no_err();

    Ok(FunctionDeclarationData {
        diagnostics: diagnostics.build(),
        signature,
        environment,
        generic_params,
        attributes,
        resolver_data: Arc::new(resolver.data),
        inline_config,
        implicit_precedence: ImplicitPrecedence::UNSPECIFIED,
    })
}
