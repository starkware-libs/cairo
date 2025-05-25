use std::sync::Arc;

use cairo_lang_defs::ids::{
    ExternTypeId, GenericKind, LanguageElementId, LookupItemId, ModuleItemId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};

use super::generics::{GenericParamsData, semantic_generic_params};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::resolve::Resolver;
use crate::substitution::SemanticRewriter;
use crate::{GenericParam, SemanticDiagnostic};

#[cfg(test)]
#[path = "extern_type_test.rs"]
mod test;

// Declaration.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ExternTypeDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<GenericParam>,
    attributes: Vec<Attribute>,
}

// Selectors.
/// Query implementation of [crate::db::SemanticGroup::extern_type_declaration_diagnostics].
pub fn extern_type_declaration_diagnostics(
    db: &dyn SemanticGroup,
    extern_type_id: ExternTypeId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_extern_type_declaration_data(extern_type_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}
/// Query implementation of [crate::db::SemanticGroup::extern_type_declaration_generic_params].
pub fn extern_type_declaration_generic_params(
    db: &dyn SemanticGroup,
    extern_type_id: ExternTypeId,
) -> Maybe<Vec<GenericParam>> {
    Ok(db.extern_type_declaration_generic_params_data(extern_type_id)?.generic_params)
}

// Computation.
/// Query implementation of [crate::db::SemanticGroup::extern_type_declaration_generic_params_data].
pub fn extern_type_declaration_generic_params_data(
    db: &dyn SemanticGroup,
    extern_type_id: ExternTypeId,
) -> Maybe<GenericParamsData> {
    let module_file_id = extern_type_id.module_file_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let extern_type_syntax = db.module_extern_type_by_id(extern_type_id)?.to_maybe()?;

    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(
        ModuleItemId::ExternType(extern_type_id),
    ));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    resolver.set_feature_config(&extern_type_id, &extern_type_syntax, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &extern_type_syntax.generic_params(db),
    );
    if let Some(param) = generic_params.iter().find(|param| param.kind() == GenericKind::Impl) {
        diagnostics.report(param.stable_ptr(db).untyped(), ExternTypeWithImplGenericsNotSupported);
    }
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, extern_type_syntax.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::priv_extern_type_declaration_data].
pub fn priv_extern_type_declaration_data(
    db: &dyn SemanticGroup,
    extern_type_id: ExternTypeId,
) -> Maybe<ExternTypeDeclarationData> {
    let mut diagnostics = SemanticDiagnostics::default();
    let extern_type_syntax = db.module_extern_type_by_id(extern_type_id)?.to_maybe()?;

    // Generic params.
    let generic_params_data = extern_type_declaration_generic_params_data(db, extern_type_id)?;
    let generic_params = generic_params_data.generic_params;
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
        ModuleItemId::ExternType(extern_type_id),
    ));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);
    let attributes = extern_type_syntax.attributes(db).structurize(db);

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, extern_type_syntax.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();

    Ok(ExternTypeDeclarationData { diagnostics: diagnostics.build(), generic_params, attributes })
}

/// Query implementation of [crate::db::SemanticGroup::extern_type_attributes].
pub fn extern_type_attributes(
    db: &dyn SemanticGroup,
    extern_type_id: ExternTypeId,
) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_extern_type_declaration_data(extern_type_id)?.attributes)
}
