use cairo_lang_semantic as semantic;
use cairo_lang_syntax::node::TypedStablePtr;

use super::block_builder::BlockBuilder;
use super::context::{LoweredExpr, LoweringContext, LoweringResult};
use super::flow_control::create_graph::create_graph_expr_if;
use super::flow_control::lower_graph::lower_graph;

/// Lowers an expression of type [semantic::ExprIf].
pub fn lower_expr_if<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    builder: &mut BlockBuilder<'db>,
    expr: &semantic::ExprIf<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    let graph = create_graph_expr_if(ctx, expr);
    lower_graph(ctx, builder, &graph, ctx.get_location(expr.stable_ptr.untyped()))
}
