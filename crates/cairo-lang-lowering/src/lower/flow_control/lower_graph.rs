//! This module is responsible for the lowering of a flow control graph [FlowControlGraph].

use super::graph::FlowControlGraph;
use crate::ids::LocationId;
use crate::lower::block_builder::BlockBuilder;
use crate::lower::context::{LoweredExpr, LoweringContext, LoweringResult};

/// Lowers a flow control graph.
#[allow(dead_code)]
pub fn lower_graph<'db>(
    _ctx: &mut LoweringContext<'db, '_>,
    _builder: &mut BlockBuilder<'db>,
    _graph: &FlowControlGraph<'db>,
    location: LocationId<'db>,
) -> LoweringResult<'db, LoweredExpr<'db>> {
    // TODO(lior): replace the following dummy code with a real implementation.
    Ok(LoweredExpr::Tuple { exprs: vec![], location })
}
