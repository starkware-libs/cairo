#[cfg(test)]
#[path = "gas_redeposit_test.rs"]
mod test;

use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::FlagId;
use cairo_lang_semantic::corelib;
use itertools::Itertools;

use crate::db::LoweringGroup;
use crate::ids::{ConcreteFunctionWithBodyId, SemanticFunctionIdEx};
use crate::implicits::FunctionImplicitsTrait;
use crate::{BlockId, FlatBlockEnd, FlatLowered, Statement, StatementCall};

/// Adds redeposit gas actions.
///
/// The algorithm is as follows:
/// Check if the function has the `GasBuiltin` implicit.
/// If it does, add a `redeposit_gas` call at the beginning of every branch in the code.
/// If it doesn't, do nothing.
pub fn gas_redeposit(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) {
    if lowered.blocks.is_empty() {
        return;
    }
    if !matches!(
        db.get_flag(FlagId::new(db.upcast(), "add_redeposit_gas")),
        Some(flag) if matches!(*flag, Flag::AddRedepositGas(true))
    ) {
        return;
    }
    let gb_ty = corelib::get_core_ty_by_name(db.upcast(), "GasBuiltin".into(), vec![]);
    if !db
        .function_with_body_implicits(function_id)
        .map(|implicits| implicits.into_iter().contains(&gb_ty))
        .unwrap_or_default()
    {
        return;
    }
    assert!(
        lowered.parameters.iter().all(|p| lowered.variables[*p].ty != gb_ty),
        "`GasRedeposit` stage must be called before `LowerImplicits` stage"
    );

    let redeposit_gas = corelib::get_function_id(
        db.upcast(),
        corelib::core_submodule(db.upcast(), "gas"),
        "redeposit_gas".into(),
        vec![],
    )
    .lowered(db);
    let mut stack = vec![BlockId::root()];
    let mut visited = vec![false; lowered.blocks.len()];
    let mut redeposit_commands = vec![];
    while let Some(block_id) = stack.pop() {
        if visited[block_id.0] {
            continue;
        }
        visited[block_id.0] = true;
        let block = &lowered.blocks[block_id];
        match &block.end {
            FlatBlockEnd::Goto(block_id, _) => {
                stack.push(*block_id);
            }
            FlatBlockEnd::Match { info } => {
                let location = info.location().with_auto_generation_note(db, "withdraw_gas");
                for arm in info.arms() {
                    stack.push(arm.block_id);
                    redeposit_commands.push((arm.block_id, location));
                }
            }
            &FlatBlockEnd::Return(..) | FlatBlockEnd::Panic(_) => {}
            FlatBlockEnd::NotSet => unreachable!("Block end not set"),
        }
    }
    for (block_id, location) in redeposit_commands {
        let block = &mut lowered.blocks[block_id];
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
