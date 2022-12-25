use std::sync::Arc;

use defs::ids::{ExternFunctionId, GenericFunctionId, GenericParamId, LanguageElementId};
use diagnostics::{Diagnostics, Maybe, ToMaybe};
use diagnostics_proc_macros::DebugWithDb;
use utils::extract_matches;

use super::generics::semantic_generic_params;
use crate::corelib::get_core_generic_function_id;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::PanicableExternFunction;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::Environment;
use crate::resolve_path::{ResolvedLookback, Resolver};
use crate::{semantic, Mutability, Parameter, SemanticDiagnostic, TypeId};

#[cfg(test)]
#[path = "extern_function_test.rs"]
mod test;

// Declaration.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ExternFunctionDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    signature: semantic::Signature,
    generic_params: Vec<GenericParamId>,
    resolved_lookback: Arc<ResolvedLookback>,
}

// Selectors.
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
/// Query implementation of [crate::db::SemanticGroup::extern_function_declaration_signature].
pub fn extern_function_declaration_signature(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<semantic::Signature> {
    Ok(db.priv_extern_function_declaration_data(extern_function_id)?.signature)
}
/// Query implementation of [crate::db::SemanticGroup::extern_function_declaration_generic_params].
pub fn extern_function_declaration_generic_params(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<Vec<GenericParamId>> {
    Ok(db.priv_extern_function_declaration_data(extern_function_id)?.generic_params)
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
/// [crate::db::SemanticGroup::extern_function_declaration_resolved_lookback].
pub fn extern_function_declaration_resolved_lookback(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<Arc<ResolvedLookback>> {
    Ok(db.priv_extern_function_declaration_data(extern_function_id)?.resolved_lookback)
}

// Computation.
/// Query implementation of [crate::db::SemanticGroup::priv_extern_function_declaration_data].
pub fn priv_extern_function_declaration_data(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<ExternFunctionDeclarationData> {
    let module_file_id = extern_function_id.module_file(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let module_data = db.module_data(module_file_id.0)?;
    let function_syntax = module_data.extern_functions.get(&extern_function_id).to_maybe()?;
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_file_id,
        &function_syntax.generic_params(db.upcast()),
    );
    let mut resolver = Resolver::new(db, module_file_id, &generic_params);
    let mut environment = Environment::default();
    let signature_syntax = function_syntax.signature(db.upcast());
    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &signature_syntax,
        GenericFunctionId::Extern(extern_function_id),
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

    let resolved_lookback = Arc::new(resolver.lookback);
    Ok(ExternFunctionDeclarationData {
        diagnostics: diagnostics.build(),
        signature,
        generic_params,
        resolved_lookback,
    })
}
