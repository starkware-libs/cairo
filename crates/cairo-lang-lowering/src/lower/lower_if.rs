use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_syntax::node::TypedStablePtr;

use super::block_builder::{BlockBuilder, SealedBlockBuilder};
use super::context::{LoweredExpr, LoweringContext, LoweringResult};
use super::flow_control::create_graph::create_graph_expr_if;
use super::flow_control::lower_graph::lower_graph;
use super::lowered_expr_to_block_scope_end;
use crate::ids::LocationId;
use crate::lower::lower_block;

/// Lowers an expression of type [semantic::ExprIf].
pub fn lower_expr_if<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    expr: &semantic::ExprIf<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    let graph = create_graph_expr_if(ctx, expr);
    lower_graph(ctx, builder, &graph, ctx.get_location(expr.stable_ptr.untyped()))
}

/// Lowers an optional else block. If the else block is missing it is replaced with a block
/// returning a unit.
/// Returns the sealed block builder of the else block.
fn lower_optional_else_block<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    mut builder: BlockBuilder<'db>,
    else_expr_opt: Option<semantic::ExprId>,
    if_location: LocationId<'db>,
) -> Maybe<SealedBlockBuilder<'db>> {
    log::trace!("Started lowering of an optional else block.");
    match else_expr_opt {
        Some(else_expr) => {
            let expr = ctx.function_body.arenas.exprs[else_expr].clone();
            match &expr {
                semantic::Expr::Block(block) => lower_block(ctx, builder, block),
                semantic::Expr::If(if_expr) => {
                    let lowered_if = lower_expr_if(ctx, &mut builder, if_expr);
                    lowered_expr_to_block_scope_end(ctx, builder, lowered_if)
                }
                _ => unreachable!(),
            }
        }
        None => lowered_expr_to_block_scope_end(
            ctx,
            builder,
            Ok(LoweredExpr::Tuple { exprs: vec![], location: if_location }),
        ),
    }
}
