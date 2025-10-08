use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    ExternFunctionId, FunctionTitleId, LanguageElementId, LookupItemId, ModuleItemId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, MaybeAsRef};
use cairo_lang_filesystem::ids::SmolStrId;
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
use crate::{GenericParam, SemanticDiagnostic, semantic};

#[cfg(test)]
#[path = "extern_function_test.rs"]
mod test;

/// Query implementation of
/// [ExternFunctionSemantic::extern_function_declaration_generic_params_data].
#[salsa::tracked(returns(ref))]
fn extern_function_declaration_generic_params_data<'db>(
    db: &'db dyn Database,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_id = extern_function_id.module_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let extern_function_syntax = db.module_extern_function_by_id(extern_function_id)?;
    let declaration = extern_function_syntax.declaration(db);

    // Generic params.
    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(
        ModuleItemId::ExternFunction(extern_function_id),
    ));
    let mut resolver = Resolver::new(db, module_id, inference_id);
    resolver.set_feature_config(&extern_function_id, &extern_function_syntax, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_id,
        &declaration.generic_params(db),
    );

    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, extern_function_syntax.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Query implementation of [ExternFunctionSemantic::priv_extern_function_declaration_data].
#[salsa::tracked(returns(ref))]
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
    diagnostics.extend(generic_params_data.diagnostics.clone());
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
        let panic_function = extract_matches!(
            get_core_generic_function_id(db, SmolStrId::from(db, "panic")),
            GenericFunctionId::Extern
        );
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

/// Trait for extern function-related semantic queries.
pub trait ExternFunctionSemantic<'db>: Database {
    /// Returns the inline configuration of an extern function's declaration.
    fn extern_function_declaration_inline_config(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<InlineConfiguration<'db>> {
        Ok(self.priv_extern_function_declaration_data(extern_function_id)?.inline_config.clone())
    }
    /// Returns the semantic diagnostics of an extern function declaration. An extern function has
    /// no body, and thus only has a declaration.
    fn extern_function_declaration_diagnostics(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        self.priv_extern_function_declaration_data(extern_function_id)
            .map(|data| data.diagnostics.clone())
            .unwrap_or_default()
    }
    /// Returns the signature of an extern function.
    fn extern_function_signature(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<&'db semantic::Signature<'db>> {
        Ok(&self.priv_extern_function_declaration_data(extern_function_id)?.signature)
    }
    /// Returns the generic params of an extern function.
    fn extern_function_declaration_generic_params(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<&'db [GenericParam<'db>]> {
        Ok(&self
            .extern_function_declaration_generic_params_data(extern_function_id)?
            .generic_params)
    }
    /// Returns the resolution resolved_items of an extern function.
    fn extern_function_declaration_resolver_data(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        Ok(self.priv_extern_function_declaration_data(extern_function_id)?.resolver_data.clone())
    }
}
impl<'db, T: Database + ?Sized> ExternFunctionSemantic<'db> for T {}

/// Trait for extern function-related semantic queries.
trait PrivExternFunctionSemantic<'db>: Database {
    /// Private query to compute data about an extern function declaration. An extern function has
    /// no body, and thus only has a declaration.
    fn priv_extern_function_declaration_data(
        &'db self,
        function_id: ExternFunctionId<'db>,
    ) -> Maybe<&'db FunctionDeclarationData<'db>> {
        priv_extern_function_declaration_data(self.as_dyn_database(), function_id).maybe_as_ref()
    }
    /// Returns the generic params data of an extern function.
    fn extern_function_declaration_generic_params_data(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<&'db GenericParamsData<'db>> {
        extern_function_declaration_generic_params_data(self.as_dyn_database(), extern_function_id)
            .maybe_as_ref()
    }
}
impl<'db, T: Database + ?Sized> PrivExternFunctionSemantic<'db> for T {}
