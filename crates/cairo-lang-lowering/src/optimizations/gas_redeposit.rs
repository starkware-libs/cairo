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
/// Check if the function will have the `GasBuiltin` implicit after the lower_implicits stage.
/// If so, after every block that ends with match, add a call to `redeposit_gas` af the beginning of
/// the next block that ends with a `return` or a `goto`.
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

    let redeposit_gas = corelib::get_function_id(
        db,
        corelib::core_submodule(db, "gas"),
        "redeposit_gas".into(),
        vec![],
    )
    .lowered(db);

    let maybe_add_redeposit_gas = |statements: &mut Vec<Statement>, opt_location| {
        if let Some(location) = opt_location {
            // The `redeposit_gas` function is added at the beginning of the block as it result in
            // smaller code when the GasBuiltin is revoked during the block.
            statements.insert(
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
    };

    let mut stack = vec![(BlockId::root(), None)];
    let mut visited = vec![false; lowered.blocks.len()];
    let mut redeposit_commands = vec![];
    while let Some((block_id, opt_location)) = stack.pop() {
        if visited[block_id.0] {
            continue;
        }
        visited[block_id.0] = true;
        let block = &mut lowered.blocks[block_id];
        let statements = &mut block.statements;
        match &block.end {
            FlatBlockEnd::Goto(block_id, _) => {
                maybe_add_redeposit_gas(statements, opt_location);
                stack.push((*block_id, None));
            }
            FlatBlockEnd::Match { info } => {
                let location = info.location().with_auto_generation_note(db, "withdraw_gas");
                for arm in info.arms() {
                    stack.push((arm.block_id, Some(location)));
                    redeposit_commands.push((arm.block_id, location));
                }
            }
            FlatBlockEnd::Return(..) => {
                maybe_add_redeposit_gas(statements, opt_location);
            }
            FlatBlockEnd::Panic(_) => {}
            FlatBlockEnd::NotSet => unreachable!("Block end not set"),
        }
    }
}
