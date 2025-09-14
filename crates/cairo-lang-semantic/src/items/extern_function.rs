use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    ExternFunctionId, FunctionTitleId, LanguageElementId, LookupItemId, ModuleItemId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_syntax::attribute::structured::AttributeListStructurize;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::extract_matches;
use salsa::Database;

use super::function_with_body::get_inline_config;
use super::functions::{FunctionDeclarationData, GenericFunctionId, InlineConfiguration};
use super::generics::{GenericParamsData, semantic_generic_params};
use crate::corelib::get_core_generic_function_id;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::compute::Environment;
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::items::function_with_body::get_implicit_precedence;
use crate::items::functions::ImplicitPrecedence;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::{GenericParam, Mutability, Parameter, SemanticDiagnostic, TypeId, semantic};

#[cfg(test)]
#[path = "extern_function_test.rs"]
mod test;

// --- Selectors ---

/// Implementation of [ExternFunctionSemantic::extern_function_declaration_inline_config].
fn extern_function_declaration_inline_config<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<InlineConfiguration<'db>> {
    Ok(db.priv_extern_function_declaration_data(extern_function_id)?.inline_config)
}

/// Query implementation of [ExternFunctionSemantic::extern_function_declaration_inline_config].
#[salsa::tracked]
fn extern_function_declaration_inline_config_tracked<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<InlineConfiguration<'db>> {
    extern_function_declaration_inline_config(db, extern_function_id)
}
// TODO(spapini): Remove declaration from the names.
/// Implementation of [ExternFunctionSemantic::extern_function_declaration_diagnostics].
fn extern_function_declaration_diagnostics<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    db.priv_extern_function_declaration_data(extern_function_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [ExternFunctionSemantic::extern_function_declaration_diagnostics].
#[salsa::tracked]
fn extern_function_declaration_diagnostics_tracked<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    extern_function_declaration_diagnostics(db, extern_function_id)
}

/// Implementation of [ExternFunctionSemantic::extern_function_signature].
fn extern_function_signature<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<semantic::Signature<'db>> {
    Ok(db.priv_extern_function_declaration_data(extern_function_id)?.signature)
}

/// Query implementation of [ExternFunctionSemantic::extern_function_signature].
#[salsa::tracked]
fn extern_function_signature_tracked<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<semantic::Signature<'db>> {
    extern_function_signature(db, extern_function_id)
}

/// Implementation of [ExternFunctionSemantic::extern_function_declaration_generic_params].
fn extern_function_declaration_generic_params<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<Vec<semantic::GenericParam<'db>>> {
    Ok(db.extern_function_declaration_generic_params_data(extern_function_id)?.generic_params)
}

/// Query implementation of [ExternFunctionSemantic::extern_function_declaration_generic_params].
#[salsa::tracked]
fn extern_function_declaration_generic_params_tracked<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<Vec<semantic::GenericParam<'db>>> {
    extern_function_declaration_generic_params(db, extern_function_id)
}

/// Implementation of
/// [ExternFunctionSemantic::extern_function_declaration_generic_params_data].
fn extern_function_declaration_generic_params_data<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_file_id = extern_function_id.module_file_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let extern_function_syntax = db.module_extern_function_by_id(extern_function_id)?;
    let declaration = extern_function_syntax.declaration(db);

    // Generic params.
    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(
        ModuleItemId::ExternFunction(extern_function_id),
    ));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    resolver.set_feature_config(&extern_function_id, &extern_function_syntax, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &declaration.generic_params(db),
    );

    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, extern_function_syntax.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Query implementation of
/// [ExternFunctionSemantic::extern_function_declaration_generic_params_data].
#[salsa::tracked]
fn extern_function_declaration_generic_params_data_tracked<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    extern_function_declaration_generic_params_data(db, extern_function_id)
}

/// Implementation of [ExternFunctionSemantic::extern_function_declaration_implicits].
fn extern_function_declaration_implicits<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<Vec<TypeId<'db>>> {
    Ok(db.priv_extern_function_declaration_data(extern_function_id)?.signature.implicits)
}

/// Query implementation of [ExternFunctionSemantic::extern_function_declaration_implicits].
#[salsa::tracked]
fn extern_function_declaration_implicits_tracked<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<Vec<TypeId<'db>>> {
    extern_function_declaration_implicits(db, extern_function_id)
}

/// Implementation of [ExternFunctionSemantic::extern_function_declaration_refs].
fn extern_function_declaration_refs<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<Vec<Parameter<'db>>> {
    Ok(db
        .priv_extern_function_declaration_data(extern_function_id)?
        .signature
        .params
        .into_iter()
        .filter(|param| param.mutability == Mutability::Reference)
        .collect())
}

/// Query implementation of [ExternFunctionSemantic::extern_function_declaration_refs].
#[salsa::tracked]
fn extern_function_declaration_refs_tracked<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<Vec<Parameter<'db>>> {
    extern_function_declaration_refs(db, extern_function_id)
}

/// Implementation of
/// [ExternFunctionSemantic::extern_function_declaration_resolver_data].
fn extern_function_declaration_resolver_data<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    Ok(db.priv_extern_function_declaration_data(extern_function_id)?.resolver_data)
}

/// Query implementation of
/// [ExternFunctionSemantic::extern_function_declaration_resolver_data].
#[salsa::tracked]
fn extern_function_declaration_resolver_data_tracked<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    extern_function_declaration_resolver_data(db, extern_function_id)
}

// --- Computation ---

/// Implementation of [ExternFunctionSemantic::priv_extern_function_declaration_data].
fn priv_extern_function_declaration_data<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<FunctionDeclarationData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    let extern_function_syntax = db.module_extern_function_by_id(extern_function_id)?;

    let declaration = extern_function_syntax.declaration(db);

    // Generic params.
    let generic_params_data =
        db.extern_function_declaration_generic_params_data(extern_function_id)?;
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::ExternFunction(extern_function_id));
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);
    resolver.set_feature_config(&extern_function_id, &extern_function_syntax, &mut diagnostics);

    let mut environment = Environment::empty();
    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &declaration,
        FunctionTitleId::Extern(extern_function_id),
        &mut environment,
    );

    if signature.panicable {
        let panic_function =
            extract_matches!(get_core_generic_function_id(db, "panic"), GenericFunctionId::Extern);
        if extern_function_id != panic_function {
            diagnostics.report(extern_function_syntax.stable_ptr(db), PanicableExternFunction);
        }
    }

    let attributes = extern_function_syntax.attributes(db).structurize(db);
    let inline_config = get_inline_config(db, &mut diagnostics, &attributes)?;

    match &inline_config {
        InlineConfiguration::None => {}
        InlineConfiguration::Always(stable_ptr)
        | InlineConfiguration::Never(stable_ptr)
        | InlineConfiguration::Should(stable_ptr) => {
            diagnostics.report(stable_ptr.untyped(), InlineAttrForExternFunctionNotAllowed);
        }
    }

    let (_, implicit_precedence_attr) =
        get_implicit_precedence(db, &mut diagnostics, &mut resolver, &attributes);
    if let Some(attr) = implicit_precedence_attr {
        diagnostics
            .report(attr.stable_ptr.untyped(), ImplicitPrecedenceAttrForExternFunctionNotAllowed);
    }

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, extern_function_syntax.stable_ptr(db).untyped());

    let signature = inference.rewrite(signature).no_err();

    Ok(FunctionDeclarationData {
        diagnostics: diagnostics.build(),
        signature,
        environment,
        attributes,
        resolver_data: Arc::new(resolver.data),
        inline_config,
        implicit_precedence: ImplicitPrecedence::UNSPECIFIED,
    })
}

/// Query implementation of [ExternFunctionSemantic::priv_extern_function_declaration_data].
#[salsa::tracked]
fn priv_extern_function_declaration_data_tracked<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<FunctionDeclarationData<'db>> {
    priv_extern_function_declaration_data(db, extern_function_id)
}

/// Trait for extern function-related semantic queries.
pub trait ExternFunctionSemantic<'db>: Database {
    /// Private query to compute data about an extern function declaration. An extern function has
    /// no body, and thus only has a declaration.
    fn priv_extern_function_declaration_data(
        &'db self,
        function_id: ExternFunctionId<'db>,
    ) -> Maybe<FunctionDeclarationData<'db>> {
        priv_extern_function_declaration_data_tracked(self.as_dyn_database(), function_id)
    }
    /// Returns the inline configuration of an extern function's declaration.
    fn extern_function_declaration_inline_config(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<InlineConfiguration<'db>> {
        extern_function_declaration_inline_config_tracked(
            self.as_dyn_database(),
            extern_function_id,
        )
    }
    /// Returns the semantic diagnostics of an extern function declaration. An extern function has
    /// no body, and thus only has a declaration.
    fn extern_function_declaration_diagnostics(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        extern_function_declaration_diagnostics_tracked(self.as_dyn_database(), extern_function_id)
    }
    /// Returns the signature of an extern function.
    fn extern_function_signature(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<semantic::Signature<'db>> {
        extern_function_signature_tracked(self.as_dyn_database(), extern_function_id)
    }
    /// Returns the generic params of an extern function.
    fn extern_function_declaration_generic_params(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>> {
        extern_function_declaration_generic_params_tracked(
            self.as_dyn_database(),
            extern_function_id,
        )
    }
    /// Returns the generic params data of an extern function.
    fn extern_function_declaration_generic_params_data(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<GenericParamsData<'db>> {
        extern_function_declaration_generic_params_data_tracked(
            self.as_dyn_database(),
            extern_function_id,
        )
    }
    /// Returns the explicit implicits of an extern function declaration.
    fn extern_function_declaration_implicits(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<Vec<TypeId<'db>>> {
        extern_function_declaration_implicits_tracked(self.as_dyn_database(), extern_function_id)
    }
    /// Returns the ref parameters of an extern function declaration.
    fn extern_function_declaration_refs(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<Vec<Parameter<'db>>> {
        extern_function_declaration_refs_tracked(self.as_dyn_database(), extern_function_id)
    }
    /// Returns the resolution resolved_items of an extern function.
    fn extern_function_declaration_resolver_data(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        extern_function_declaration_resolver_data_tracked(
            self.as_dyn_database(),
            extern_function_id,
        )
    }
}
impl<'db, T: Database + ?Sized> ExternFunctionSemantic<'db> for T {}
