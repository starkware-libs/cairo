use defs::ids::{FreeFunctionId, LanguageElementId};
use diagnostics::Diagnostics;
use diagnostics_proc_macros::DebugWithDb;
use syntax::node::ast;

use super::functions::{function_signature_params, function_signature_return_type};
use crate::db::SemanticGroup;
use crate::expr::compute::{compute_expr_semantic, ComputationContext, Environment};
use crate::{semantic, SemanticDiagnostic};

// Declaration.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct FreeFunctionDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    signature: semantic::Signature,
    environment: Environment,
}

// Selectors.
/// Query implementation of [crate::db::SemanticGroup::free_function_declaration_diagnostics].
pub fn free_function_declaration_diagnostics(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_free_function_declaration_data(free_function_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}
/// Query implementation of [crate::db::SemanticGroup::free_function_signature].
pub fn free_function_signature(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Option<semantic::Signature> {
    Some(db.priv_free_function_declaration_data(free_function_id)?.signature)
}

// Computation.
/// Query implementation of [crate::db::SemanticGroup::priv_free_function_declaration_data].
pub fn priv_free_function_declaration_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Option<FreeFunctionDeclarationData> {
    let mut diagnostics = Diagnostics::default();
    let module_id = free_function_id.module(db.as_defs_group());
    let module_data = db.module_data(module_id)?;
    let signature_syntax =
        module_data.free_functions.get(&free_function_id)?.signature(db.as_syntax_group());
    let return_type =
        function_signature_return_type(&mut diagnostics, db, module_id, &signature_syntax);
    let (params, environment) =
        function_signature_params(&mut diagnostics, db, module_id, &signature_syntax);
    Some(FreeFunctionDeclarationData {
        diagnostics,
        signature: semantic::Signature { params, return_type },
        environment,
    })
}

// Definition.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct FreeFunctionDefinitionData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    body: semantic::ExprId,
}

// Selectors.
/// Query implementation of [crate::db::SemanticGroup::free_function_definition_diagnostics].
pub fn free_function_definition_diagnostics(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_free_function_definition_data(free_function_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}
/// Query implementation of [crate::db::SemanticGroup::free_function_body].
pub fn free_function_body(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Option<semantic::ExprId> {
    Some(db.priv_free_function_definition_data(free_function_id)?.body)
}

// Computation.
/// Query implementation of [crate::db::SemanticGroup::priv_free_function_definition_data].
pub fn priv_free_function_definition_data(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Option<FreeFunctionDefinitionData> {
    let mut diagnostics = Diagnostics::default();
    let module_id = free_function_id.module(db.as_defs_group());
    let module_data = db.module_data(module_id)?;
    let syntax = module_data.free_functions.get(&free_function_id)?.clone();
    // Compute signature semantic.
    let environment = db.priv_free_function_declaration_data(free_function_id)?.environment;
    // Compute body semantic expr.
    let mut ctx = ComputationContext::new(&mut diagnostics, db, module_id, environment);
    let expr = compute_expr_semantic(&mut ctx, ast::Expr::Block(syntax.body(db.as_syntax_group())));
    Some(FreeFunctionDefinitionData { diagnostics, body: db.intern_expr(expr) })
}
