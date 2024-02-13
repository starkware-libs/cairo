#[cfg(test)]
#[path = "return_optimization_test.rs"]
mod test;

use cairo_lang_semantic as semantic;
use cairo_lang_utils::extract_matches;
use itertools::Itertools;
use semantic::MatchArmSelector;

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::db::LoweringGroup;
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchArm, MatchEnumInfo, MatchInfo, Statement,
    StatementEnumConstruct, StatementStructConstruct, StatementStructDestructure, VarRemapping,
    VarUsage, VariableId,
};

/// Adds early returns when applicable.
///
/// This optimization does backward analysis from return statement and keeps track of
/// each returned value (see `ValueInfo`), whenever all the returned values are available at a block
/// end and there was no side effects later, the end is replaced with a return statement.
pub fn return_optimization(db: &dyn LoweringGroup, lowered: &mut FlatLowered) {
    if lowered.blocks.is_empty() {
        return;
    }
    let ctx = ReturnOptimizerContext { db, lowered, fixes: vec![] };
    let mut analysis =
        BackAnalysis { lowered: &*lowered, block_info: Default::default(), analyzer: ctx };
    analysis.get_root_info();
    let ctx = analysis.analyzer;

    for FixInfo { block_id, return_vars } in ctx.fixes.into_iter() {
        let block = &mut lowered.blocks[block_id];
        block.end = FlatBlockEnd::Return(
            return_vars
                .iter()
                .map(|var_info| *extract_matches!(var_info, ValueInfo::Var))
                .collect_vec(),
        )
    }
}

pub struct ReturnOptimizerContext<'a> {
    db: &'a dyn LoweringGroup,
    lowered: &'a FlatLowered,

    /// The list of fixes that should be applied.
    fixes: Vec<FixInfo>,
}
impl ReturnOptimizerContext<'_> {
    /// Given a VarUsage, returns the ValueInfo that corresponds to it.
    fn get_var_info(&self, var_usage: &VarUsage) -> ValueInfo {
        let var_ty = &self.lowered.variables[var_usage.var_id].ty;
        if self.is_droppable(var_usage.var_id) && self.db.single_value_type(*var_ty).unwrap() {
            ValueInfo::Interchangeable(*var_ty)
        } else {
            ValueInfo::Var(*var_usage)
        }
    }

    /// Returns true if the variable is droppable
    fn is_droppable(&self, var_id: VariableId) -> bool {
        self.lowered.variables[var_id].droppable.is_ok()
    }
}

/// Information about a fix that should be applied to the lowering.
pub struct FixInfo {
    /// a block id of a block that can be fixed to return `return_vars`.
    block_id: BlockId,
    /// The variables that should be returned by the block.
    return_vars: Vec<ValueInfo>,
}

/// Information about the value that should be returned from the function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueInfo {
    /// The value is available through the given var usage.
    Var(VarUsage),
    /// The can be replaced with other values of the same type.
    Interchangeable(semantic::TypeId),
    /// The value is the result of a StructConstruct statement.
    StructConstruct {
        /// The type of the struct.
        ty: semantic::TypeId,
        /// The inputs to the StructConstruct statement.
        var_infos: Vec<ValueInfo>,
    },
    /// The value is the result of an EnumConstruct statement.
    EnumConstruct {
        /// The input to the EnumConstruct.
        var_info: Box<ValueInfo>,
        /// The constructed variant.
        variant: semantic::ConcreteVariant,
    },
}

/// The result of applying an operation to a ValueInfo.
enum OpResult {
    /// The input of the operation was consumed.
    InputConsumed,
    /// One of the value is produced operation and therefore it is invalid before the operation.
    ValueInvalidated,
    /// The operation did not change the value info.
    NoChange,
}

impl ValueInfo {
    /// Applies the given function to the value info.
    fn apply<F>(&mut self, f: &F)
    where
        F: Fn(&VarUsage) -> ValueInfo,
    {
        match self {
            ValueInfo::Var(var_usage) => *self = f(var_usage),
            ValueInfo::StructConstruct { ty: _, ref mut var_infos } => {
                for var_info in var_infos.iter_mut() {
                    var_info.apply(f);
                }
            }
            ValueInfo::EnumConstruct { ref mut var_info, .. } => {
                var_info.apply(f);
            }
            ValueInfo::Interchangeable(_) => {}
        }
    }

    /// Updates the value to the state before the StructDeconstruct statement.
    /// Returns OpResult.
    fn apply_deconstruct(
        &mut self,
        ctx: &ReturnOptimizerContext<'_>,
        stmt: &StatementStructDestructure,
    ) -> OpResult {
        match self {
            ValueInfo::Var(var_usage) => {
                if stmt.outputs.contains(&var_usage.var_id) {
                    OpResult::ValueInvalidated
                } else {
                    OpResult::NoChange
                }
            }
            ValueInfo::StructConstruct { ty, var_infos } => {
                let mut cancels_out = ty == &ctx.lowered.variables[stmt.input.var_id].ty
                    && var_infos.len() == stmt.outputs.len();
                for (var_info, output) in var_infos.iter().zip(stmt.outputs.iter()) {
                    if !cancels_out {
                        break;
                    }

                    match var_info {
                        ValueInfo::Var(var_usage) if &var_usage.var_id == output => {}
                        ValueInfo::Interchangeable(ty)
                            if &ctx.lowered.variables[*output].ty == ty => {}
                        _ => cancels_out = false,
                    }
                }

                if cancels_out {
                    // If the StructDeconstruct cancels out the StructConstruct, then we don't need
                    // to `apply_deconstruct` to the innner var infos.
                    *self = ValueInfo::Var(stmt.input);
                    return OpResult::InputConsumed;
                }

                let mut input_consumed = false;
                for var_info in var_infos.iter_mut() {
                    match var_info.apply_deconstruct(ctx, stmt) {
                        OpResult::InputConsumed => {
                            input_consumed = true;
                        }
                        OpResult::ValueInvalidated => {
                            // If one of the values is invalidated the optimization is no longer
                            // applicable.
                            return OpResult::ValueInvalidated;
                        }
                        OpResult::NoChange => {}
                    }
                }

                match input_consumed {
                    true => OpResult::InputConsumed,
                    false => OpResult::NoChange,
                }
            }
            ValueInfo::EnumConstruct { ref mut var_info, .. } => {
                var_info.apply_deconstruct(ctx, stmt)
            }
            ValueInfo::Interchangeable(_) => OpResult::NoChange,
        }
    }

    /// Updates the value to the expected value before the match arm.
    /// Returns OpResult.
    fn apply_match_arm(&mut self, input: &ValueInfo, arm: &MatchArm) -> OpResult {
        match self {
            ValueInfo::Var(var_usage) => {
                if arm.var_ids == [var_usage.var_id] {
                    OpResult::ValueInvalidated
                } else {
                    OpResult::NoChange
                }
            }
            ValueInfo::StructConstruct { ty: _, ref mut var_infos } => {
                let mut input_consumed = false;
                for var_info in var_infos.iter_mut() {
                    match var_info.apply_match_arm(input, arm) {
                        OpResult::InputConsumed => {
                            input_consumed = true;
                        }
                        OpResult::ValueInvalidated => return OpResult::ValueInvalidated,
                        OpResult::NoChange => {}
                    }
                }

                if input_consumed {
                    return OpResult::InputConsumed;
                }
                OpResult::NoChange
            }
            ValueInfo::EnumConstruct { ref mut var_info, variant } => {
                let MatchArmSelector::VariantId(arm_variant) = &arm.arm_selector else {
                    panic!("Enum construct should not appear in value match");
                };

                if *variant == *arm_variant {
                    let cancels_out = match **var_info {
                        ValueInfo::Interchangeable(_) => true,
                        ValueInfo::Var(var_usage) if arm.var_ids == [var_usage.var_id] => true,
                        _ => false,
                    };

                    if cancels_out {
                        // If the arm recreates the relevant enum variant, then the arm
                        // assuming the other arms also cancel out.
                        *self = input.clone();
                        return OpResult::InputConsumed;
                    }
                }

                var_info.apply_match_arm(input, arm)
            }
            ValueInfo::Interchangeable(_) => OpResult::NoChange,
        }
    }
}

/// Information about the current state of the analyzer.
/// Used to track the value that should be returned from the function at the current
/// analysis point or None if it is unknown.
/// If early_return_possible() returns true, the function can return early as the return value is
/// already known.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AnalyzerInfo {
    opt_returned_vars: Option<Vec<ValueInfo>>,
}

impl AnalyzerInfo {
    /// Applies the given function to the returned_vars
    fn apply<F>(&mut self, f: &F)
    where
        F: Fn(&VarUsage) -> ValueInfo,
    {
        let Some(ref mut returned_vars) = self.opt_returned_vars else {
            return;
        };

        for var_info in returned_vars.iter_mut() {
            var_info.apply(f)
        }
    }

    /// Replaces occurrences of `var_id` with `var_info`.
    fn replace(&mut self, var_id: VariableId, var_info: ValueInfo) {
        self.apply(&|var_usage| {
            if var_usage.var_id == var_id { var_info.clone() } else { ValueInfo::Var(*var_usage) }
        });
    }

    /// Invalidates the state of the analyzer, identifying early return is no longer possible.
    fn invalidate(&mut self) {
        self.opt_returned_vars = None;
    }

    /// Updates the info to the state before the StructDeconstruct statement.
    fn apply_deconstruct(
        &mut self,
        ctx: &ReturnOptimizerContext<'_>,
        stmt: &StatementStructDestructure,
    ) {
        let Some(ref mut returned_vars) = self.opt_returned_vars else { return };

        let mut input_consumed = false;
        for var_info in returned_vars.iter_mut() {
            match var_info.apply_deconstruct(ctx, stmt) {
                OpResult::InputConsumed => {
                    input_consumed = true;
                }
                OpResult::ValueInvalidated => {
                    self.invalidate();
                    return;
                }
                OpResult::NoChange => {}
            };
        }

        if !(input_consumed || ctx.is_droppable(stmt.input.var_id)) {
            self.invalidate();
        }
    }

    /// Updates the info to the state before match arm.
    fn apply_match_arm(&mut self, is_droppable: bool, input: &ValueInfo, arm: &MatchArm) {
        let Some(ref mut returned_vars) = self.opt_returned_vars else { return };

        let mut input_consumed = false;
        for var_info in returned_vars.iter_mut() {
            match var_info.apply_match_arm(input, arm) {
                OpResult::InputConsumed => {
                    input_consumed = true;
                }
                OpResult::ValueInvalidated => {
                    self.invalidate();
                    return;
                }
                OpResult::NoChange => {}
            };
        }

        if !(input_consumed || is_droppable) {
            self.invalidate();
        }
    }

    /// Returns true if the an early return is possible according to 'self'.
    fn early_return_possible(&self) -> bool {
        let Some(ref returned_vars) = self.opt_returned_vars else { return false };

        returned_vars.iter().all(|var_info| match var_info {
            ValueInfo::Var(_) => true,
            ValueInfo::StructConstruct { .. } => false,
            ValueInfo::EnumConstruct { .. } => false,
            ValueInfo::Interchangeable(_) => false,
        })
    }
}

impl<'a> Analyzer<'a> for ReturnOptimizerContext<'_> {
    type Info = AnalyzerInfo;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        stmt: &'a Statement,
    ) {
        match stmt {
            Statement::StructConstruct(StatementStructConstruct { inputs, output }) => {
                // Note that the ValueInfo::StructConstruct can only be removed by
                // a StructDeconstruct statement that produces its non-interchangeable inputs so
                // allowing undroppable inputs is ok here.
                info.replace(
                    *output,
                    ValueInfo::StructConstruct {
                        ty: self.lowered.variables[*output].ty,
                        var_infos: inputs.iter().map(|input| self.get_var_info(input)).collect(),
                    },
                );
            }

            Statement::StructDestructure(stmt) => info.apply_deconstruct(self, stmt),
            Statement::EnumConstruct(StatementEnumConstruct { variant, input, output }) => {
                info.replace(
                    *output,
                    ValueInfo::EnumConstruct {
                        var_info: Box::new(self.get_var_info(input)),
                        variant: variant.clone(),
                    },
                );
            }
            _ => info.invalidate(),
        }
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        (block_id, _statement_idx): StatementLocation,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        info.apply(&|var_usage| {
            if let Some(usage) = remapping.get(&var_usage.var_id) {
                ValueInfo::Var(*usage)
            } else {
                ValueInfo::Var(*var_usage)
            }
        });

        if info.early_return_possible() {
            self.fixes
                .push(FixInfo { block_id, return_vars: info.opt_returned_vars.clone().unwrap() });
        }
    }

    fn merge_match(
        &mut self,
        (block_id, _statement_idx): StatementLocation,
        match_info: &'a MatchInfo,
        infos: &[Self::Info],
    ) -> Self::Info {
        let MatchInfo::Enum(MatchEnumInfo { input, arms, .. }) = match_info else {
            return AnalyzerInfo { opt_returned_vars: None };
        };
        if arms.is_empty() {
            return AnalyzerInfo { opt_returned_vars: None };
        }

        let input_info = self.get_var_info(input);
        let mut opt_last_info = None;
        for (arm, info) in arms.iter().zip(infos) {
            let mut curr_info = info.clone();
            curr_info.apply_match_arm(self.is_droppable(input.var_id), &input_info, arm);

            if !curr_info.early_return_possible() {
                return AnalyzerInfo { opt_returned_vars: None };
            }

            if let Some(prev_info) = &opt_last_info {
                if prev_info != &curr_info {
                    return AnalyzerInfo { opt_returned_vars: None };
                }
            } else {
                opt_last_info = Some(curr_info);
            }
        }

        let last_info = opt_last_info.unwrap();
        let return_vars = last_info.opt_returned_vars.clone().unwrap();
        self.fixes.push(FixInfo { block_id, return_vars });
        last_info
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        vars: &'a [VarUsage],
    ) -> Self::Info {
        // Note that `self.get_var_info` is not used here because ValueInfo::Interchangeable is
        // supported only inside other ValueInfo variants.
        AnalyzerInfo {
            opt_returned_vars: Some(
                vars.iter().map(|var_usage| ValueInfo::Var(*var_usage)).collect(),
            ),
        }
    }
}
