//! This module is responsible for the lowering of a flow control graph [FlowControlGraph].

use cairo_lang_semantic as semantic;
use cairo_lang_syntax::node::TypedStablePtr;
use cairo_lang_utils::extract_matches;

use super::graph::{FlowControlGraph, FlowControlNode};
use crate::ids::LocationId;
use crate::lower::block_builder::BlockBuilder;
use crate::lower::context::{LoweredExpr, LoweringContext, LoweringResult};

/// Lowers a flow control graph.
#[allow(dead_code)]
pub fn lower_graph(
    ctx: &mut LoweringContext<'_, '_>,
    _builder: &mut BlockBuilder,
    graph: &FlowControlGraph,
) -> LoweringResult<LoweredExpr> {
    // TODO(lior): replace the following dummy code with a real implementation.
    let condition_expr =
        extract_matches!(&graph.nodes[graph.root.0], FlowControlNode::BooleanIf).condition;
    let location = get_expr_location(ctx, &condition_expr);
    Ok(LoweredExpr::Tuple { exprs: vec![], location })
}

/// Returns the location of the given expression.
fn get_expr_location(ctx: &LoweringContext<'_, '_>, expr: &semantic::ExprId) -> LocationId {
    let condition_expr = &ctx.function_body.arenas.exprs[*expr];
    let stable_ptr = condition_expr.stable_ptr().untyped();
    ctx.get_location(stable_ptr)
}
