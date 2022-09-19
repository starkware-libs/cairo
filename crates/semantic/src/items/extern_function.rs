use defs::ids::{ExternFunctionId, LanguageElementId};
use diagnostics::Diagnostics;
use diagnostics_proc_macros::DebugWithDb;

use super::functions::{
    function_signature_generic_params, function_signature_params, function_signature_return_type,
};
use crate::db::SemanticGroup;
use crate::{semantic, SemanticDiagnostic};

// Declaration.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct ExternFunctionDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    signature: semantic::Signature,
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

// Computation.
/// Query implementation of [crate::db::SemanticGroup::priv_extern_function_declaration_data].
pub fn priv_extern_function_declaration_data(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Option<ExternFunctionDeclarationData> {
    let mut diagnostics = Diagnostics::default();
    let module_id = extern_function_id.module(db.upcast());
    let module_data = db.module_data(module_id)?;
    let signature_syntax =
        module_data.extern_functions.get(&extern_function_id)?.signature(db.upcast());
    let return_type =
        function_signature_return_type(&mut diagnostics, db, module_id, &signature_syntax);
    let (params, _environment) =
        function_signature_params(&mut diagnostics, db, module_id, &signature_syntax);
    let generic_params =
        function_signature_generic_params(&mut diagnostics, db, module_id, &signature_syntax);
    Some(ExternFunctionDeclarationData {
        diagnostics,
        signature: semantic::Signature { params, generic_params, return_type },
    })
}
