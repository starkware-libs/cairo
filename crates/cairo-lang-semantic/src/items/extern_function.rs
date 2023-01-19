use std::sync::Arc;

use cairo_lang_defs::ids::{
    ExternFunctionId, FunctionSignatureId, GenericParamId, LanguageElementId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_utils::extract_matches;

use super::attribute::ast_attributes_to_semantic;
use super::functions::{FunctionDeclarationData, GenericFunctionId};
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

// --- Selectors ---

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

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_extern_function_declaration_data].
pub fn priv_extern_function_declaration_data(
    db: &dyn SemanticGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<FunctionDeclarationData> {
    let syntax_db = db.upcast();
    let module_file_id = extern_function_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let module_extern_functions = db.module_extern_functions(module_file_id.0)?;
    let function_syntax = module_extern_functions.get(&extern_function_id).to_maybe()?;
    let declaration = function_syntax.declaration(syntax_db);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_file_id,
        &declaration.generic_params(syntax_db),
    );
    let mut resolver = Resolver::new(db, module_file_id, &generic_params);
    let mut environment = Environment::default();
    let signature_syntax = declaration.signature(syntax_db);
    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &signature_syntax,
        FunctionSignatureId::Extern(extern_function_id),
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

    Ok(FunctionDeclarationData {
        diagnostics: diagnostics.build(),
        signature,
        environment,
        generic_params,
        attributes: ast_attributes_to_semantic(syntax_db, function_syntax.attributes(syntax_db)),
        resolved_lookback: Arc::new(resolver.lookback),
    })
}
