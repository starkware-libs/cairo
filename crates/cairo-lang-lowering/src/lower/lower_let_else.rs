use cairo_lang_semantic::usage::MemberPath;
use cairo_lang_semantic::{Binding, Expr, Pattern, TypeLongId, VarId};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ast::StatementPtr;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::Intern;
use id_arena::Id;
use itertools::zip_eq;

use super::block_builder::{BlockBuilder, SealedBlockBuilder};
use super::context::{LoweredExpr, LoweringContext, LoweringFlowError, VarRequest};
use super::generators;
use super::lower_match::{MatchArmWrapper, lower_match_arms};
use crate::diagnostic::MatchKind;

/// Lowers a let-else statement.
///
/// For example,
/// ```plain
/// let Some((x, y)) = opt else {
///     return;
/// }
/// ```
///
/// A let-else statement is handled as a match as follows:
///
/// ```plain
/// let (v0, v1, ...) = match expr {
///     pattern => { (v0, v1, ...) },  // Success arm.
///     _ => { else_expr },            // Else arm.
/// };
/// ```
pub fn lower_let_else(
    ctx: &mut LoweringContext<'_, '_>,
    builder: &mut BlockBuilder,
    pattern_id: &Id<Pattern>,
    expr: &Id<Expr>,
    lowered_expr: LoweredExpr,
    else_clause: &Id<Expr>,
    stable_ptr: &StatementPtr,
) -> Result<(), LoweringFlowError> {
    let pattern = ctx.function_body.arenas.patterns[*pattern_id].clone();
    let variables = pattern.variables(&ctx.function_body.arenas.patterns);

    // Create a match expression with two arms.
    let patterns = &[*pattern_id];
    let var_ids_and_stable_ptrs = variables
        .iter()
        .map(|pattern_var| (VarId::Local(pattern_var.var.id), pattern_var.stable_ptr.untyped()))
        .collect();
    let arms = vec![
        MatchArmWrapper::LetElseSuccess(patterns, var_ids_and_stable_ptrs, stable_ptr.untyped()),
        MatchArmWrapper::ElseClause(*else_clause),
    ];

    // Lower the match expression.
    // The result is a tuple with the values of the pattern's variables.
    let match_lowered = lower_match_arms(
        ctx,
        builder,
        *expr,
        lowered_expr,
        arms,
        ctx.get_location(stable_ptr.untyped()),
        MatchKind::Match,
    )?;

    // Destruct the tuple.
    let reqs = variables
        .iter()
        .map(|pattern_var| VarRequest {
            ty: pattern_var.var.ty,
            location: ctx.get_location(pattern_var.stable_ptr.untyped()),
        })
        .collect();
    let output_vars = generators::StructDestructure {
        input: match_lowered.as_var_usage(ctx, builder)?,
        var_reqs: reqs,
    }
    .add(ctx, &mut builder.statements);

    // Bind the values to the variables.
    for (pattern_var, output_var) in zip_eq(variables, output_vars) {
        let sem_var = Binding::LocalVar(pattern_var.var.clone());
        // Override variable location.
        ctx.variables.variables[output_var].location =
            ctx.get_location(pattern_var.stable_ptr.untyped());
        builder.put_semantic(sem_var.id(), output_var);
        ctx.semantic_defs.insert(sem_var.id(), sem_var);
    }

    Ok(())
}

/// Similar to [super::lower_tail_expr] expect that the result is a tuple of the given variables.
///
/// Used in the lowering of the success arm's body in let-else.
pub fn lower_success_arm_body(
    ctx: &mut LoweringContext<'_, '_>,
    mut builder: BlockBuilder,
    vars: &[(VarId, SyntaxStablePtrId)],
    stable_ptr: &SyntaxStablePtrId,
) -> SealedBlockBuilder {
    log::trace!("Lowering success arm body");

    let inputs: Vec<_> = vars
        .iter()
        .map(|(var_id, stable_ptr)| {
            builder
                .get_ref_raw(ctx, &MemberPath::Var(*var_id), ctx.get_location(*stable_ptr))
                .unwrap()
        })
        .collect();

    let tys = inputs.iter().map(|var_usage| ctx.variables[var_usage.var_id].ty).collect();
    let tuple_ty = TypeLongId::Tuple(tys).intern(ctx.db);
    let tuple_var_usage = generators::StructConstruct {
        inputs,
        ty: tuple_ty,
        location: ctx.get_location(*stable_ptr),
    }
    .add(ctx, &mut builder.statements);

    builder.goto_callsite(Some(tuple_var_usage))
}
