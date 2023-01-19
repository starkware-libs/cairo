use std::sync::Arc;

use cairo_lang_defs::ids::{ConstantId, LanguageElementId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::{compute_expr_semantic, ComputationContext, Environment};
use crate::resolve_path::{ResolvedLookback, Resolver};
use crate::types::resolve_type;
use crate::{Expr, SemanticDiagnostic};

#[cfg(test)]
#[path = "constant_test.rs"]
mod test;

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
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
    constant: Constant,
    resolved_lookback: Arc<ResolvedLookback>,
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

    let mut resolver = Resolver::new(db, module_file_id, &[]);

    let const_type = resolve_type(
        db,
        &mut diagnostics,
        &mut resolver,
        &const_ast.type_clause(syntax_db).ty(syntax_db),
    );

    let mut ctx =
        ComputationContext::new(db, &mut diagnostics, resolver, None, Environment::default());
    let value = compute_expr_semantic(&mut ctx, &const_ast.value(syntax_db));
    let value_type = value.ty();

    // Check that the type matches.
    if !const_type.is_missing(db) && !value_type.is_missing(db) && value_type != const_type {
        ctx.diagnostics.report(
            &const_ast.value(syntax_db),
            crate::diagnostic::SemanticDiagnosticKind::WrongType {
                expected_ty: const_type,
                actual_ty: value_type,
            },
        );
    }

    // Check that the expression is a literal.
    if !matches!(value, Expr::Literal(_)) {
        ctx.diagnostics.report(
            &const_ast.value(syntax_db),
            crate::diagnostic::SemanticDiagnosticKind::OnlyLiteralConstants,
        );
    };

    let constant = Constant { value };
    let resolved_lookback = Arc::new(ctx.resolver.lookback);
    Ok(ConstantData { diagnostics: diagnostics.build(), constant, resolved_lookback })
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
    Ok(db.priv_constant_semantic_data(const_id)?.constant)
}

/// Query implementation of [crate::db::SemanticGroup::constant_resolved_lookback].
pub fn constant_resolved_lookback(
    db: &dyn SemanticGroup,
    const_id: ConstantId,
) -> Maybe<Arc<ResolvedLookback>> {
    Ok(db.priv_constant_semantic_data(const_id)?.resolved_lookback)
}
