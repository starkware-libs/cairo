use defs::ids::{FreeFunctionId, GenericFunctionId, LanguageElementId};
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use syntax::node::ast;

use crate::db::SemanticGroup;
use crate::expr::compute::{compute_expr_semantic, ComputationContext};
use crate::{semantic, SemanticDiagnostic};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FreeFunction {
    pub signature: semantic::Signature,
    pub body: semantic::ExprId,
}

#[with_diagnostics]
/// Query implementation of [crate::db::SemanticGroup::priv_free_function_semantic].
pub fn priv_free_function_semantic(
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Option<FreeFunction> {
    let module_id = free_function_id.module(db.as_defs_group());
    let syntax = db.module_data(module_id)?.free_functions.get(&free_function_id)?.clone();

    // Compute signature semantic.
    let generic_function_signature_data = db
        .priv_generic_function_signature_data(GenericFunctionId::Free(free_function_id))
        .ignore()?;

    // Compute body semantic expr.
    let mut ctx = ComputationContext::new(
        diagnostics,
        db,
        module_id,
        generic_function_signature_data.variables,
    );
    let expr = compute_expr_semantic(&mut ctx, ast::Expr::Block(syntax.body(db.as_syntax_group())));
    let body = db.intern_expr(expr);

    Some(FreeFunction { signature: generic_function_signature_data.signature, body })
}

/// Query implementation of [crate::db::SemanticGroup::free_function_semantic].
pub fn free_function_semantic(
    db: &dyn SemanticGroup,
    free_function_id: FreeFunctionId,
) -> Option<FreeFunction> {
    db.priv_free_function_semantic(free_function_id).ignore()
}
