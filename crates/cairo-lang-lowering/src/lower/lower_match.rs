use cairo_lang_debug::DebugWithDb;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::{FlagId, FlagLongId};
use cairo_lang_semantic as semantic;
use cairo_lang_syntax::node::TypedStablePtr;

use super::block_builder::BlockBuilder;
use super::context::{LoweredExpr, LoweringContext, LoweringResult};
use crate::lower::flow_control::create_graph::create_graph_expr_match;
use crate::lower::flow_control::lower_graph::lower_graph;

/// Lowers an expression of type [semantic::ExprMatch].
pub(crate) fn lower_expr_match<'db>(
    ctx: &mut LoweringContext<'db, '_>,
    expr: &semantic::ExprMatch<'db>,
    builder: &mut BlockBuilder<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    log::trace!("Lowering a match expression: {:?}", expr.debug(&ctx.expr_formatter));
    let graph = create_graph_expr_match(ctx, expr);
    lower_graph(ctx, builder, &graph, ctx.get_location(expr.stable_ptr.untyped()))
}

/// Returns the threshold for the number of arms for optimising numeric match expressions, by using
/// a jump table instead of an if-else construct.
/// `is_small_type` means the matched type has < 2**128 possible values.
pub fn numeric_match_optimization_threshold<'db>(
    ctx: &LoweringContext<'db, '_>,
    is_small_type: bool,
) -> usize {
    // For felt252 the number of steps with if-else is 2 * min(n, number_of_arms) + 2 and 11~13 for
    // jump table for small_types the number of steps with if-else is 2 * min(n, number_of_arms) + 4
    // and 9~12 for jump table.
    let default_threshold = if is_small_type { 8 } else { 10 };
    ctx.db
        .get_flag(FlagId::new(
            ctx.db,
            FlagLongId("numeric_match_optimization_min_arms_threshold".into()),
        ))
        .map(|flag| match *flag {
            Flag::NumericMatchOptimizationMinArmsThreshold(threshold) => threshold,
            _ => panic!("Wrong type flag `{flag:?}`."),
        })
        .unwrap_or(default_threshold)
}
