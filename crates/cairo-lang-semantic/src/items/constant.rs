use std::sync::Arc;

use cairo_lang_defs::ids::{
    ConstantId, LanguageElementId, LookupItemId, ModuleItemId, NamedLanguageElementId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::try_extract_matches;
use id_arena::Arena;

use super::functions::GenericFunctionWithBodyId;
use crate::corelib::get_core_trait;
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics};
use crate::expr::compute::{compute_expr_semantic, ComputationContext, Environment};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::conform::InferenceConform;
use crate::expr::inference::InferenceId;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::types::resolve_type;
use crate::{
    Expr, ExprBlock, ExprFunctionCall, ExprFunctionCallArg, ExprId, ExprStructCtor, ExprTuple,
    FunctionId, SemanticDiagnostic, TypeId,
};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Constant {
    /// The actual id of the const expression value.
    pub value: ExprId,
    /// The arena of all the expressions for the const calculation.
    pub exprs: Arc<Arena<Expr>>,
}
impl Constant {
    pub fn ty(&self) -> TypeId {
        self.exprs[self.value].ty()
    }
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
    validate_constant_expr(db, &ctx.exprs, value.id, ctx.diagnostics);

    let resolver_data = Arc::new(ctx.resolver.data);
    let constant = Constant { value: value.id, exprs: Arc::new(ctx.exprs) };
    Ok(ConstantData { diagnostics: diagnostics.build(), constant: Ok(constant), resolver_data })
}

/// Cycle handling for [SemanticGroup::priv_constant_semantic_data].
pub fn priv_constant_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    const_id: &ConstantId,
) -> Maybe<ConstantData> {
    let module_file_id = const_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let const_ast = db.module_constant_by_id(*const_id)?.to_maybe()?;
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::Constant(*const_id));
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    Ok(ConstantData {
        constant: Err(diagnostics.report(&const_ast, SemanticDiagnosticKind::ConstCycle)),
        diagnostics: diagnostics.build(),
        resolver_data: Arc::new(Resolver::new(db, module_file_id, inference_id).data),
    })
}

/// Validates that the given expression is a valid constant.
fn validate_constant_expr(
    db: &dyn SemanticGroup,
    exprs: &Arena<Expr>,
    expr_id: ExprId,
    diagnostics: &mut SemanticDiagnostics,
) {
    let expr = &exprs[expr_id];
    match expr {
        Expr::Constant(_) | Expr::Literal(_) => {}
        Expr::Block(ExprBlock { statements, tail: Some(inner), .. }) if statements.is_empty() => {
            validate_constant_expr(db, exprs, *inner, diagnostics)
        }
        Expr::FunctionCall(ExprFunctionCall { function, args, coupon_arg: None, .. })
            if is_function_const(db, *function) =>
        {
            args.iter()
                .filter_map(|arg| try_extract_matches!(arg, ExprFunctionCallArg::Value))
                .for_each(|arg| validate_constant_expr(db, exprs, *arg, diagnostics));
        }
        Expr::Tuple(ExprTuple { items, .. }) => {
            items
                .iter()
                .for_each(|expr_id| validate_constant_expr(db, exprs, *expr_id, diagnostics));
        }
        Expr::StructCtor(ExprStructCtor { members, base_struct: None, .. }) => {
            members
                .iter()
                .for_each(|(_, expr_id)| validate_constant_expr(db, exprs, *expr_id, diagnostics));
        }
        Expr::EnumVariantCtor(expr) => {
            validate_constant_expr(db, exprs, expr.value_expr, diagnostics)
        }
        // Adding a diagnostic for unsupported constants only if there is no more internal error.
        _ if diagnostics.diagnostics.error_count == 0 => {
            diagnostics.report_by_ptr(
                expr.stable_ptr().untyped(),
                SemanticDiagnosticKind::UnsupportedConstant,
            );
        }
        _ => {}
    }
}

/// Returns true if the given function is allowed to be called in constant context.
fn is_function_const(db: &dyn SemanticGroup, function_id: FunctionId) -> bool {
    let concrete_function = function_id.get_concrete(db);
    let Ok(Some(body)) = concrete_function.body(db) else { return false };
    let GenericFunctionWithBodyId::Impl(imp) = body.generic_function(db) else { return false };
    let impl_def = imp.concrete_impl_id.impl_def_id(db);
    if impl_def.parent_module(db.upcast()).owning_crate(db.upcast()) != db.core_crate() {
        return false;
    }
    let Ok(trait_id) = db.impl_def_trait(impl_def) else {
        return false;
    };
    let expected_trait_name = match imp.function.name(db.upcast()).as_str() {
        "neg" => "Neg",
        "add" => "Add",
        "sub" => "Sub",
        "mul" => "Mul",
        "div" => "Div",
        "rem" => "Rem",
        "bitand" => "BitAnd",
        "bitor" => "BitOr",
        "bitxor" => "BitXor",
        _ => return false,
    };
    trait_id == get_core_trait(db, expected_trait_name.into())
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

/// Cycle handling for [SemanticGroup::constant_semantic_data].
pub fn constant_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    const_id: &ConstantId,
) -> Maybe<Constant> {
    // Forwarding cycle handling to `priv_constant_semantic_data` handler.
    constant_semantic_data(db, *const_id)
}

/// Query implementation of [crate::db::SemanticGroup::constant_resolver_data].
pub fn constant_resolver_data(
    db: &dyn SemanticGroup,
    const_id: ConstantId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_constant_semantic_data(const_id)?.resolver_data)
}

/// Cycle handling for [crate::db::SemanticGroup::constant_resolver_data].
pub fn constant_resolver_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    const_id: &ConstantId,
) -> Maybe<Arc<ResolverData>> {
    constant_resolver_data(db, *const_id)
}
