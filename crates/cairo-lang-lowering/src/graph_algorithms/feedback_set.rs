use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::flag::FlagsGroup;
use cairo_lang_utils::graph_algos::feedback_set::calc_feedback_set;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use salsa::Database;

use super::concrete_function_node::ConcreteFunctionWithBodyNode;
use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::{DependencyType, LoweringStage};

/// Query implementation of [crate::db::LoweringGroup::function_with_body_feedback_set].
#[salsa::tracked(returns(ref))]
pub fn function_with_body_feedback_set<'db>(
    db: &'db dyn Database,
    function: ConcreteFunctionWithBodyId<'db>,
    stage: LoweringStage,
) -> Maybe<OrderedHashSet<ConcreteFunctionWithBodyId<'db>>> {
    let r = db.lowered_scc_representative(function, DependencyType::Cost, stage);
    function_with_body_feedback_set_of_representative(db, r.0, stage)
}

/// Query implementation of [crate::db::LoweringGroup::needs_withdraw_gas].
#[salsa::tracked]
pub fn needs_withdraw_gas<'db>(
    db: &'db dyn Database,
    function: ConcreteFunctionWithBodyId<'db>,
) -> Maybe<bool> {
    Ok(db.flag_add_withdraw_gas()
        && db
            .function_with_body_feedback_set(function, LoweringStage::Monomorphized)?
            .contains(&function))
}

/// Returns the feedback-vertex-set of the given concrete-function SCC-representative. A
/// feedback-vertex-set is the set of vertices whose removal leaves a graph without cycles.
#[salsa::tracked]
fn function_with_body_feedback_set_of_representative<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
    stage: LoweringStage,
) -> Maybe<OrderedHashSet<ConcreteFunctionWithBodyId<'db>>> {
    Ok(calc_feedback_set(
        ConcreteFunctionWithBodyNode {
            function_id,
            db,
            dependency_type: DependencyType::Cost,
            stage,
        }
        .into(),
    ))
}
