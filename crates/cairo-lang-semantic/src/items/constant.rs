use std::sync::Arc;

use cairo_lang_defs::ids::{ConstantId, LanguageElementId, LookupItemId, ModuleItemId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::TypedSyntaxNode;
use id_arena::Arena;

use crate::corelib::validate_literal;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::{compute_expr_semantic, ComputationContext, Environment};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::conform::InferenceConform;
use crate::expr::inference::InferenceId;
use crate::literals::try_extract_minus_literal;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::types::resolve_type;
use crate::{Expr, ExprId, ExprStructCtor, ExprTuple, SemanticDiagnostic, TypeId};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Constant {
    /// The actual id of the const expression value.
    pub value: ExprId,
    /// The type of the constant.
    pub ty: TypeId,
    /// The arena of all the expressions for the const calculation.
    pub exprs: Arc<Arena<Expr>>,
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
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let const_ast = db.module_constant_by_id(const_id)?.to_maybe()?;
    let syntax_db = db.upcast();

    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::Constant(const_id));
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let mut resolver = Resolver::new(db, module_file_id, inference_id);

    let const_type = resolve_type(
        db,
        &mut diagnostics,
        &mut resolver,
        &const_ast.type_clause(syntax_db).ty(syntax_db),
    );

    let environment = Environment::from_lookup_item_id(db, lookup_item_id, &mut diagnostics);
    let mut ctx = ComputationContext::new(db, &mut diagnostics, None, resolver, None, environment);
    let value = compute_expr_semantic(&mut ctx, &const_ast.value(syntax_db));
    if let Err(err) = ctx.resolver.inference().conform_ty(value.ty(), const_type) {
        err.report(ctx.diagnostics, const_ast.stable_ptr().untyped());
    }
    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = ctx.resolver.inference().finalize() {
        inference_err
            .report(ctx.diagnostics, stable_ptr.unwrap_or(const_ast.stable_ptr().untyped()));
    }
    for (_, expr) in ctx.exprs.iter_mut() {
        *expr = ctx.resolver.inference().rewrite(expr.clone()).no_err();
    }

    // Check that the expression is a valid constant.
    validate_constant_expr(db, &mut ctx.resolver, &ctx.exprs, value.id, ctx.diagnostics);

    let resolver_data = Arc::new(ctx.resolver.data);
    let constant = Constant { value: value.id, ty: const_type, exprs: Arc::new(ctx.exprs) };
    Ok(ConstantData { diagnostics: diagnostics.build(), constant: Ok(constant), resolver_data })
}

/// Validates that the given expression is a valid constant.
fn validate_constant_expr(
    db: &dyn SemanticGroup,
    resolver: &mut Resolver<'_>,
    exprs: &Arena<Expr>,
    expr_id: ExprId,
    diagnostics: &mut SemanticDiagnostics,
) {
    let expr = &exprs[expr_id];
    let report_err = |diagnostics: &mut SemanticDiagnostics, err| {
        diagnostics.report_by_ptr(expr.stable_ptr().untyped(), err);
    };
    let mut handle_literal = |diagnostics, value| {
        let ty = resolver.inference().rewrite(expr.ty()).no_err();
        if let Err(err) = validate_literal(db, ty, value) {
            report_err(diagnostics, crate::diagnostic::SemanticDiagnosticKind::LiteralError(err));
        }
    };
    match &expr {
        Expr::Literal(expr) => handle_literal(diagnostics, expr.value.clone()),
        Expr::FunctionCall(expr) => {
            if let Some(value) = try_extract_minus_literal(db, exprs, expr) {
                handle_literal(diagnostics, value);
            } else {
                report_err(
                    diagnostics,
                    crate::diagnostic::SemanticDiagnosticKind::UnsupportedConstant,
                );
            }
        }
        Expr::Tuple(ExprTuple { items, .. }) => {
            items.iter().for_each(|expr_id| {
                validate_constant_expr(db, resolver, exprs, *expr_id, diagnostics)
            });
        }
        Expr::StructCtor(ExprStructCtor { members, base_struct: None, .. }) => {
            members.iter().for_each(|(_, expr_id)| {
                validate_constant_expr(db, resolver, exprs, *expr_id, diagnostics)
            });
        }
        // TODO(orizi): Handle `Expr::EnumVariantCtor`.
        // TODO(orizi): Handle `Expr::Constant`.
        _ => {
            report_err(diagnostics, crate::diagnostic::SemanticDiagnosticKind::UnsupportedConstant)
        }
    }
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
