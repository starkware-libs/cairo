#[cfg(test)]
#[path = "gas_redeposit_test.rs"]
mod test;

use cairo_lang_filesystem::flag::FlagsGroup;
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_semantic::{ConcreteVariant, corelib};
use itertools::Itertools;
use salsa::Database;

use crate::analysis::core::StatementLocation;
use crate::analysis::{DataflowAnalyzer, DataflowBackAnalysis, Direction, Edge};
use crate::ids::{ConcreteFunctionWithBodyId, LocationId, SemanticFunctionIdEx};
use crate::implicits::FunctionImplicitsTrait;
use crate::panic::PanicSignatureInfo;
use crate::{
    BlockEnd, BlockId, Lowered, Statement, StatementCall, StatementEnumConstruct, VarUsage,
    VariableId,
};

/// Adds redeposit gas actions.
///
/// The algorithm is as follows:
/// Checks if the function will have the `GasBuiltin` implicit after the lower_implicits stage.
/// If so, after every block that ends with match, add a call to `redeposit_gas` in every arm
/// that is followed by a convergence point or a return.
/// Note that assuming `reorganize_blocks` stage is applied before this stage, every `goto`
/// statement is a convergence point.
///
/// Note that for implementation simplicity this stage must be applied before `LowerImplicits`
/// stage.
pub fn gas_redeposit<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
    lowered: &mut Lowered<'db>,
) {
    if lowered.blocks.is_empty() {
        return;
    }
    if !db.flag_add_withdraw_gas() {
        return;
    }
    let gb_ty = corelib::get_core_ty_by_name(db, SmolStrId::from(db, "GasBuiltin"), vec![]);
    // Checking if the implicits of this function past lowering includes `GasBuiltin`.
    if let Ok(implicits) = db.function_with_body_implicits(function_id)
        && !implicits.into_iter().contains(&gb_ty)
    {
        return;
    }
    assert!(
        lowered.parameters.iter().all(|p| lowered.variables[*p].ty != gb_ty),
        "`GasRedeposit` stage must be called before `LowerImplicits` stage"
    );

    let panic_sig = PanicSignatureInfo::new(db, &function_id.signature(db).unwrap());
    if panic_sig.always_panic {
        return;
    }
    let mut ctx = GasRedepositContext { err_variant: panic_sig.err_variant, fixes: vec![] };
    DataflowBackAnalysis::new(lowered, &mut ctx).run();

    let redeposit_gas = corelib::get_function_id(
        db,
        corelib::core_submodule(db, SmolStrId::from(db, "gas")),
        SmolStrId::from(db, "redeposit_gas"),
        vec![],
    )
    .lowered(db);
    for (block_id, location) in ctx.fixes {
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
                is_specialization_base_call: false,
            }),
        );
    }
}

pub struct GasRedepositContext<'db> {
    /// The panic error variant.
    pub err_variant: ConcreteVariant<'db>,
    /// Locations where we need to insert redeposit_gas.
    pub fixes: Vec<(BlockId, LocationId<'db>)>,
}

/// Redeposit state for a point in the program.
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum RedepositState {
    /// Gas might be burned if we don't redeposit.
    Required,
    /// Redeposit is not necessary. This may occur if it has already been handled
    /// or if the flow is terminating due to a panic.
    Unnecessary,
    /// The flow returns the given variable, redeposit is required unless the return var is of the
    /// error variant.
    Return(VariableId),
}

impl<'db, 'a> DataflowAnalyzer<'db, 'a> for GasRedepositContext<'db> {
    type Info = RedepositState;
    const DIRECTION: Direction = Direction::Backward;

    fn initial_info(&mut self, _block_id: BlockId, block_end: &'a BlockEnd<'db>) -> Self::Info {
        // If the function has multiple returns with different gas costs, gas will get burned unless
        // we redeposit it.
        // If however, this return corresponds to a panic, we don't redeposit due to code size
        // concerns.
        match block_end {
            BlockEnd::Return(vars, _) => match vars.last() {
                Some(VarUsage { var_id, location: _ }) => RedepositState::Return(*var_id),
                None => RedepositState::Required,
            },
            _ => RedepositState::Unnecessary,
        }
    }

    fn merge(
        &mut self,
        _statement_location: StatementLocation,
        location: &'a LocationId<'db>,
        infos: impl Iterator<Item = (BlockId, Self::Info)>,
    ) -> Self::Info {
        for (src, state) in infos {
            match state {
                RedepositState::Return(_) | RedepositState::Required => {
                    self.fixes.push((src, *location));
                }
                RedepositState::Unnecessary => {}
            }
        }

        // `redeposit_gas` was added, no need to add it until the next convergence point.
        RedepositState::Unnecessary
    }

    fn transfer_stmt(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        stmt: &'a Statement<'db>,
    ) {
        let RedepositState::Return(var_id) = *info else {
            return;
        };

        let Statement::EnumConstruct(StatementEnumConstruct { variant, input: _, output }) = stmt
        else {
            return;
        };

        if *output == var_id && *variant == self.err_variant {
            *info = RedepositState::Unnecessary;
        }
    }

    fn transfer_edge(&mut self, info: &Self::Info, edge: &Edge<'db, 'a>) -> Self::Info {
        match edge {
            Edge::Goto { .. } => {
                // A goto is a convergence point, gas will get burned unless it is redeposited
                // before the convergence.
                RedepositState::Required
            }
            _ => *info,
        }
    }
}
