use defs::ids::{ExternFunctionId, GenericParamId, LanguageElementId};
use diagnostics::Diagnostics;
use diagnostics_proc_macros::DebugWithDb;

use super::functions::{function_signature_params, function_signature_return_type};
use super::generics::semantic_generic_params;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::resolve_path::ResolveScope;
use crate::{semantic, SemanticDiagnostic};

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
}

// Selectors.
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
) -> Option<semantic::Signature> {
    Some(db.priv_extern_function_declaration_data(extern_function_id)?.signature)
}
/// Query implementation of [crate::db::SemanticGroup::extern_function_declaration_generic_params].
pub fn extern_function_declaration_generic_params(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Option<Vec<GenericParamId>> {
    Some(db.priv_extern_function_declaration_data(extern_function_id)?.generic_params)
}

// Computation.
/// Query implementation of [crate::db::SemanticGroup::priv_extern_function_declaration_data].
pub fn priv_extern_function_declaration_data(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Option<ExternFunctionDeclarationData> {
    let module_id = extern_function_id.module(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_id);
    let module_data = db.module_data(module_id)?;
    let function_syntax = module_data.extern_functions.get(&extern_function_id)?;
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_id,
        &function_syntax.generic_params(db.upcast()),
    );
    let scope = ResolveScope::new(db, module_id, &generic_params);
    let signature_syntax = function_syntax.signature(db.upcast());
    let return_type =
        function_signature_return_type(&mut diagnostics, db, &scope, &signature_syntax);
    let (params, _environment) =
        function_signature_params(&mut diagnostics, db, scope, &signature_syntax);
    Some(ExternFunctionDeclarationData {
        diagnostics: diagnostics.diagnostics,
        signature: semantic::Signature { params, return_type },
        generic_params,
    })
}
