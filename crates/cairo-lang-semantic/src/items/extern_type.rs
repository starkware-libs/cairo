use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    ExternTypeId, GenericKind, LanguageElementId, LookupItemId, ModuleItemId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, MaybeAsRef};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};
use salsa::Database;

use super::generics::{GenericParamsData, semantic_generic_params};
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
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
struct ExternTypeDeclarationData<'db> {
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    generic_params: Vec<GenericParam<'db>>,
    attributes: Vec<Attribute<'db>>,
}

// Computation.
/// Implementation of [ExternTypeSemantic::extern_type_declaration_generic_params_data].
fn extern_type_declaration_generic_params_data<'db>(
    db: &'db dyn Database,
    extern_type_id: ExternTypeId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_id = extern_type_id.module_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let extern_type_syntax = db.module_extern_type_by_id(extern_type_id)?;

    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(
        ModuleItemId::ExternType(extern_type_id),
    ));
    let mut resolver = Resolver::new(db, module_id, inference_id);
    resolver.set_feature_config(&extern_type_id, &extern_type_syntax, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_id,
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

/// Query implementation of [ExternTypeSemantic::extern_type_declaration_generic_params_data].
#[salsa::tracked]
fn extern_type_declaration_generic_params_data_tracked<'db>(
    db: &'db dyn Database,
    extern_type_id: ExternTypeId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    extern_type_declaration_generic_params_data(db, extern_type_id)
}

/// Returns the declaration data of an extern type.
#[salsa::tracked(returns(ref))]
fn extern_type_declaration_data<'db>(
    db: &'db dyn Database,
    extern_type_id: ExternTypeId<'db>,
) -> Maybe<ExternTypeDeclarationData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    let extern_type_syntax = db.module_extern_type_by_id(extern_type_id)?;

    // Generic params.
    let generic_params_data_result =
        extern_type_declaration_generic_params_data(db, extern_type_id);
    let generic_params_data = generic_params_data_result.maybe_as_ref()?;
    let generic_params = generic_params_data.generic_params.clone();
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
        ModuleItemId::ExternType(extern_type_id),
    ));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics.clone());
    let attributes = extern_type_syntax.attributes(db).structurize(db);

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, extern_type_syntax.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();

    Ok(ExternTypeDeclarationData { diagnostics: diagnostics.build(), generic_params, attributes })
}

/// Trait for extern type-related semantic queries.
pub trait ExternTypeSemantic<'db>: Database {
    /// Returns the semantic diagnostics of an extern type declaration. An extern type has
    /// no body, and thus only has a declaration.
    fn extern_type_declaration_diagnostics(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        let db = self.as_dyn_database();
        extern_type_declaration_data(db, extern_type_id)
            .as_ref()
            .map(|data| data.diagnostics.clone())
            .unwrap_or_default()
    }
    /// Returns the generic params of an extern type.
    fn extern_type_declaration_generic_params(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Maybe<&'db [GenericParam<'db>]> {
        let db = self.as_dyn_database();
        Ok(&extern_type_declaration_data(db, extern_type_id).maybe_as_ref()?.generic_params)
    }
    /// Returns the generic params data of an extern type.
    fn extern_type_declaration_generic_params_data(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Maybe<GenericParamsData<'db>> {
        extern_type_declaration_generic_params_data_tracked(self.as_dyn_database(), extern_type_id)
    }
    /// Returns the attributes of an extern type.
    fn extern_type_attributes(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Maybe<&'db [Attribute<'db>]> {
        let db = self.as_dyn_database();
        Ok(&extern_type_declaration_data(db, extern_type_id).maybe_as_ref()?.attributes)
    }
}
impl<'db, T: Database + ?Sized> ExternTypeSemantic<'db> for T {}
