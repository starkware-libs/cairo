use std::sync::Arc;

use cairo_lang_defs::ids::{
    ExternTypeId, GenericKind, LanguageElementId, LookupItemId, ModuleItemId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};

use super::generics::{semantic_generic_params, GenericParamsData};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::InferenceId;
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
    let module_file_id = extern_type_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let extern_type_syntax = db.module_extern_type_by_id(extern_type_id)?.to_maybe()?;

    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(
        ModuleItemId::ExternType(extern_type_id),
    ));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &extern_type_syntax.generic_params(db.upcast()),
    )?;
    if let Some(param) = generic_params.iter().find(|param| param.kind() == GenericKind::Impl) {
        diagnostics.report_by_ptr(
            param.stable_ptr(db.upcast()).untyped(),
            ExternTypeWithImplGenericsNotSupported,
        );
    }
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, extern_type_syntax.stable_ptr().untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::priv_extern_type_declaration_data].
pub fn priv_extern_type_declaration_data(
    db: &dyn SemanticGroup,
    extern_type_id: ExternTypeId,
) -> Maybe<ExternTypeDeclarationData> {
    let module_file_id = extern_type_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
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
    diagnostics.diagnostics.extend(generic_params_data.diagnostics);

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, extern_type_syntax.stable_ptr().untyped());

    let generic_params = inference.rewrite(generic_params).no_err();

    Ok(ExternTypeDeclarationData { diagnostics: diagnostics.build(), generic_params })
}
