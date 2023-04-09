use cairo_lang_defs::diagnostic_utils::StableLocationOption;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::corelib::{
    core_submodule, core_withdraw_gas, get_function_id, get_ty_by_name,
};
use id_arena::Arena;

use crate::db::LoweringGroup;
use crate::ids::{ConcreteFunctionWithBodyId, FunctionLongId, SemanticFunctionIdEx};
use crate::lower::context::{VarRequest, VariableAllocator};
use crate::{
    FlatBlockEnd, FlatLowered, MatchExternInfo, MatchInfo, Statement, StatementCall, Variable,
};

/// Main function for the replace_withdraw_gas lowering phase. Replaces `withdraw_gas` calls with
/// `withdraw_gas_all` calls where necessary.
pub fn replace_withdraw_gas(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) -> Maybe<()> {
    let lowered_vars = &mut lowered.variables;
    for block in lowered.blocks.iter_mut() {
        let info = match &block.end {
            FlatBlockEnd::Match { info: MatchInfo::Extern(info) } => {
                match db.lookup_intern_lowering_function(info.function) {
                    FunctionLongId::AutoWithdrawGas => info.clone(),
                    _ => break,
                }
            }
            _ => break,
        };

        if needs_withdraw_gas_all(db, function) {
            replace_block_to_withdraw_gas_all(db, function, lowered_vars, info, block)?;
        } else {
            replace_block_to_withdraw_gas(db, info, block);
        }
    }

    Ok(())
}

// TODO(yuval): currently always returns true. Change to a smarter logic.
/// returns whether the `FunctionLongId::AutoWithdrawGas` call should be replaced with a call to
/// `withdraw_gas_all`.
fn needs_withdraw_gas_all(_db: &dyn LoweringGroup, _function: ConcreteFunctionWithBodyId) -> bool {
    true
}

/// Replaces a block ending with a match-extern of `withdraw_gas` to call `withdraw_gas_all`.
fn replace_block_to_withdraw_gas_all(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
    lowered_vars: &mut Arena<Variable>,
    info: MatchExternInfo,
    block: &mut crate::FlatBlock,
) -> Maybe<()> {
    let gas_module = core_submodule(db.upcast(), "gas");

    // Add variable of type BuiltinCosts.
    let mut variables = VariableAllocator::new(
        db,
        function.function_with_body_id(db).base_semantic_function(db),
        lowered_vars.clone(),
    )?;
    let builtin_costs_var = variables.new_var(VarRequest {
        ty: get_ty_by_name(db.upcast(), gas_module, "BuiltinCosts".into(), Vec::new()),
        location: StableLocationOption::None,
    });
    *lowered_vars = variables.variables;

    // Add a statement call to `get_builtin_costs`.
    block.statements.push(Statement::Call(StatementCall {
        function: get_function_id(db.upcast(), gas_module, "get_builtin_costs".into(), vec![])
            .lowered(db),
        inputs: vec![],
        outputs: vec![builtin_costs_var],
        location: StableLocationOption::None,
    }));

    // Modify block end to call `withdraw_gas_all`.
    let mut inputs = info.inputs;
    inputs.push(builtin_costs_var);
    block.end = FlatBlockEnd::Match {
        info: MatchInfo::Extern(MatchExternInfo {
            function: get_function_id(db.upcast(), gas_module, "withdraw_gas_all".into(), vec![])
                .lowered(db),
            inputs,
            ..info
        }),
    };

    Ok(())
}

/// Replaces a block ending with a match-extern of `withdraw_gas` to call `withdraw_gas_all`.
fn replace_block_to_withdraw_gas(
    db: &dyn LoweringGroup,
    info: MatchExternInfo,
    block: &mut crate::FlatBlock,
) {
    block.end = FlatBlockEnd::Match {
        info: MatchInfo::Extern(MatchExternInfo {
            function: core_withdraw_gas(db.upcast()).lowered(db),
            ..info
        }),
    }
}
