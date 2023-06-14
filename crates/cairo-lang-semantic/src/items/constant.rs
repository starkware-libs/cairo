use std::sync::Arc;

use cairo_lang_defs::ids::{ConstantId, LanguageElementId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::TypedSyntaxNode;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::{compute_expr_semantic, ComputationContext, Environment};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::conform::InferenceConform;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::types::resolve_type;
use crate::{Expr, SemanticDiagnostic};

#[cfg(test)]
#[path = "constant_test.rs"]
mod test;

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Constant {
    pub value: Expr,
}

/// Information about a constant definition.
///
/// Helper struct for the data returned by [SemanticGroup::priv_constant_semantic_data].
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConstantData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    constant: Maybe<Constant>,
    resolver_data: Arc<ResolverData>,
}

/// Query implementation of [SemanticGroup::priv_constant_semantic_data].
pub fn priv_constant_semantic_data(
    db: &dyn SemanticGroup,
    const_id: ConstantId,
) -> Maybe<ConstantData> {
    let module_file_id = const_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let module_constants = db.module_constants(module_file_id.0)?;
    let const_ast = module_constants.get(&const_id).to_maybe()?;
    let syntax_db = db.upcast();

    let mut resolver = Resolver::new(db, module_file_id);

    let const_type = resolve_type(
        db,
        &mut diagnostics,
        &mut resolver,
        &const_ast.type_clause(syntax_db).ty(syntax_db),
    );

    let mut ctx =
        ComputationContext::new(db, &mut diagnostics, resolver, None, Environment::default());
    let value = compute_expr_semantic(&mut ctx, &const_ast.value(syntax_db));
    if let Err(err) = ctx.resolver.inference().conform_ty(value.ty(), const_type) {
        err.report(ctx.diagnostics, const_ast.stable_ptr().untyped());
    }

    // Check that the expression is a literal.
    if !matches!(value.expr, Expr::Literal(_)) {
        ctx.diagnostics.report(
            &const_ast.value(syntax_db),
            crate::diagnostic::SemanticDiagnosticKind::OnlyLiteralConstants,
        );
    };

    let constant = Constant { value: value.expr };

    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = ctx.resolver.inference().finalize() {
        inference_err
            .report(ctx.diagnostics, stable_ptr.unwrap_or(const_ast.stable_ptr().untyped()));
    }
    let constant = ctx.resolver.inference().rewrite(constant).no_err();

    let resolver_data = Arc::new(ctx.resolver.data);
    Ok(ConstantData { diagnostics: diagnostics.build(), constant: Ok(constant), resolver_data })
}

/// Query implementation of [SemanticGroup::constant_semantic_diagnostics].
pub fn constant_semantic_diagnostics(
    db: &dyn SemanticGroup,
    const_id: ConstantId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_constant_semantic_data(const_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [SemanticGroup::constant_semantic_data].
pub fn constant_semantic_data(db: &dyn SemanticGroup, const_id: ConstantId) -> Maybe<Constant> {
    db.priv_constant_semantic_data(const_id)?.constant
}

/// Query implementation of [crate::db::SemanticGroup::constant_resolver_data].
pub fn constant_resolver_data(
    db: &dyn SemanticGroup,
    const_id: ConstantId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_constant_semantic_data(const_id)?.resolver_data)
}
