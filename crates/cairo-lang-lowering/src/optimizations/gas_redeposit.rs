#[cfg(test)]
#[path = "gas_redeposit_test.rs"]
mod test;

use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::FlagId;
use cairo_lang_semantic::corelib;
use itertools::{Itertools, zip_eq};

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::db::LoweringGroup;
use crate::ids::{ConcreteFunctionWithBodyId, LocationId, SemanticFunctionIdEx};
use crate::implicits::FunctionImplicitsTrait;
use crate::{BlockId, FlatLowered, MatchInfo, Statement, StatementCall, VarRemapping, VarUsage};

/// Adds redeposit gas actions.
///
/// The algorithm is as follows:
/// Check if the function will have the `GasBuiltin` implicit after the lower_implicits stage.
/// If so, after every block that ends with match, add a call to `redeposit_gas` in every arm
/// that is followed by a convergence point or a return.
/// Note that assuming `reorganize_blocks` stage is applied before this stage, every `goto`
/// statement is a convergence point.
///
/// Note that for implementation simplicity this stage must be applied before `LowerImplicits`
/// stage.
pub fn gas_redeposit(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) {
    if lowered.blocks.is_empty() {
        return;
    }
    if matches!(db.get_flag(FlagId::new(db, "add_withdraw_gas")),
        Some(flag) if matches!(*flag, Flag::AddWithdrawGas(false)))
    {
        return;
    }
    let gb_ty = corelib::get_core_ty_by_name(db, "GasBuiltin".into(), vec![]);
    // Checking if the implicits of this function past lowering includes `GasBuiltin`.
    if let Ok(implicits) = db.function_with_body_implicits(function_id) {
        if !implicits.into_iter().contains(&gb_ty) {
            return;
        }
    }
    assert!(
        lowered.parameters.iter().all(|p| lowered.variables[*p].ty != gb_ty),
        "`GasRedeposit` stage must be called before `LowerImplicits` stage"
    );

    let ctx = GasRedepositContext { fixes: vec![] };
    let mut analysis = BackAnalysis::new(lowered, ctx);
    analysis.get_root_info();

    let redeposit_gas = corelib::get_function_id(
        db,
        corelib::core_submodule(db, "gas"),
        "redeposit_gas".into(),
        vec![],
    )
    .lowered(db);
    for (block_id, location) in analysis.analyzer.fixes {
        let block = &mut lowered.blocks[block_id];

        // The `redeposit_gas` function is added at the beginning of the block as it result in
        // smaller code when the GasBuiltin is revoked during the block.
        block.statements.insert(
            0,
            Statement::Call(StatementCall {
                function: redeposit_gas,
                inputs: vec![],
                with_coupon: false,
                outputs: vec![],
                location,
            }),
        );
    }
}

pub struct GasRedepositContext {
    /// The list of blocks where we need to insert redeposit_gas.
    fixes: Vec<(BlockId, LocationId)>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum RedepositState {
    /// Gas might be burned if we don't redeposit.
    Required,
    /// Rediposit was already added, no need to add another one.
    Redeposited,
}

impl Analyzer<'_> for GasRedepositContext {
    type Info = RedepositState;

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        _target_block_id: BlockId,
        _remapping: &VarRemapping,
    ) {
        // A goto is a convergence point, gas will get burned unless we redeposit it before the
        // convergence.
        *info = RedepositState::Required
    }

    fn merge_match(
        &mut self,
        _st: StatementLocation,
        match_info: &MatchInfo,
        infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info {
        for (info, arm) in zip_eq(infos, match_info.arms()) {
            if info == RedepositState::Required {
                self.fixes.push((arm.block_id, *match_info.location()));
            }
        }

        // `redeposit_gas` was added, no need to add it until the next convergence point.
        RedepositState::Redeposited
    }

    fn info_from_return(&mut self, _: StatementLocation, _vars: &[VarUsage]) -> Self::Info {
        // If the function has multiple returns with different gas costs, gas will get burned unless
        // we redeposit it.
        RedepositState::Required
    }
}
