use cairo_lang_semantic::usage::MemberPath;
use cairo_lang_semantic::{Binding, ExprId, PatternId, TypeLongId, VarId};
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_syntax::node::ast::StatementPtr;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::Intern;
use itertools::zip_eq;

use super::block_builder::BlockBuilder;
use super::context::{LoweringContext, LoweringFlowError, VarRequest};
use super::flow_control::create_graph::create_graph_expr_let_else;
use super::flow_control::lower_graph::lower_graph;
use super::generators;
use crate::VarUsage;
use crate::ids::LocationId;

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
pub fn lower_let_else<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    pattern_id: PatternId,
    expr: ExprId,
    else_clause: ExprId,
    stable_ptr: &StatementPtr<'db>,
) -> Result<(), LoweringFlowError<'db>> {
    let pattern = &ctx.function_body.arenas.patterns[pattern_id];
    let variables = pattern.variables(&ctx.function_body.arenas.patterns);

    // Create a list of the variables in the pattern, and their stable pointers.
    let var_ids_and_stable_ptrs = variables
        .iter()
        .map(|pattern_var| (VarId::Local(pattern_var.var.id), pattern_var.stable_ptr.untyped()))
        .collect();

    let graph =
        create_graph_expr_let_else(ctx, pattern_id, expr, else_clause, var_ids_and_stable_ptrs);
    let lowered_expr = lower_graph(ctx, builder, &graph, ctx.get_location(stable_ptr.untyped()))?;

    // Destruct the tuple.
    let reqs = variables
        .iter()
        .map(|pattern_var| VarRequest {
            ty: pattern_var.var.ty,
            location: ctx.get_location(pattern_var.stable_ptr.untyped()),
        })
        .collect();
    let output_vars = generators::StructDestructure {
        input: lowered_expr.as_var_usage(ctx, builder)?,
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
pub fn lower_success_arm_body<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    mut builder: BlockBuilder<'db>,
    vars: &[(VarId<'db>, SyntaxStablePtrId<'db>)],
    location: LocationId<'db>,
) -> (BlockBuilder<'db>, VarUsage<'db>) {
    log::trace!("Lowering success arm body");

    let inputs: Vec<_> = vars
        .iter()
        .map(|(var_id, stable_ptr)| {
            builder
                .get_ref_raw(ctx, &MemberPath::Var(*var_id), ctx.get_location(*stable_ptr), None)
                .unwrap()
        })
        .collect();

    let tys = inputs.iter().map(|var_usage| ctx.variables[var_usage.var_id].ty).collect();
    let tuple_ty = TypeLongId::Tuple(tys).intern(ctx.db);
    let tuple_var_usage = generators::StructConstruct { inputs, ty: tuple_ty, location }
        .add(ctx, &mut builder.statements);

    (builder, tuple_var_usage)
}
