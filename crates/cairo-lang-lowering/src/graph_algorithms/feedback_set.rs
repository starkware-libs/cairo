use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::{FlagId, FlagLongId};
use cairo_lang_utils::graph_algos::feedback_set::calc_feedback_set;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::smol_str::SmolStr;

use super::concrete_function_node::ConcreteFunctionWithBodyNode;
use crate::db::{ConcreteSCCRepresentative, LoweringGroup};
use crate::ids::ConcreteFunctionWithBodyId;
use crate::{DependencyType, LoweringStage};

/// Query implementation of [crate::db::LoweringGroup::function_with_body_feedback_set].
pub fn function_with_body_feedback_set<'db>(
    db: &'db dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId<'db>,
    stage: LoweringStage,
) -> Maybe<OrderedHashSet<ConcreteFunctionWithBodyId<'db>>> {
    let r = db.lowered_scc_representative(function, DependencyType::Cost, stage);
    db.priv_function_with_body_feedback_set_of_representative(r, stage)
}

/// Returns the value of the `add_withdraw_gas` flag, or `true` if the flag is not set.
pub fn flag_add_withdraw_gas(db: &dyn LoweringGroup) -> bool {
    db.get_flag(FlagId::new(db, FlagLongId(SmolStr::from("add_withdraw_gas"))))
        .map(|flag| *flag == Flag::AddWithdrawGas(true))
        .unwrap_or(true)
}

/// Query implementation of [crate::db::LoweringGroup::needs_withdraw_gas].
pub fn needs_withdraw_gas<'db>(
    db: &'db dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId<'db>,
) -> Maybe<bool> {
    Ok(flag_add_withdraw_gas(db)
        && db
            .function_with_body_feedback_set(function, LoweringStage::Monomorphized)?
            .contains(&function))
}

/// Query implementation of
/// [crate::db::LoweringGroup::priv_function_with_body_feedback_set_of_representative].
pub fn priv_function_with_body_feedback_set_of_representative<'db>(
    db: &'db dyn LoweringGroup,
    function: ConcreteSCCRepresentative<'db>,
    stage: LoweringStage,
) -> Maybe<OrderedHashSet<ConcreteFunctionWithBodyId<'db>>> {
    Ok(calc_feedback_set(
        ConcreteFunctionWithBodyNode {
            function_id: function.0,
            db,
            dependency_type: DependencyType::Cost,
            stage,
        }
        .into(),
    ))
}
