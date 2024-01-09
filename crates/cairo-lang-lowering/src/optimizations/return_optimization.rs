#[cfg(test)]
#[path = "return_optimization_test.rs"]
mod test;

use cairo_lang_semantic as semantic;
use itertools::Itertools;

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::db::LoweringGroup;
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchArm, MatchEnumInfo, MatchInfo, Statement,
    StatementEnumConstruct, StatementStructConstruct, StatementStructDestructure, VarRemapping,
    VarUsage, VariableId,
};

/// Moves return earlier when applicable.
/// Currently there are two cases:
/// 1. EnumConstruct statements where all the arms reconstruct the enum and return it.
/// 2. Goto that remaps the last variable and then returns it.
pub fn return_optimization(db: &dyn LoweringGroup, lowered: &mut FlatLowered) {
    if !lowered.blocks.is_empty() {
        let ctx = ReturnOptimizerContext {
            lowered,
            unit_ty: semantic::corelib::unit_ty(db.upcast()),
            fixes: vec![],
        };
        let mut analysis =
            BackAnalysis { lowered: &*lowered, block_info: Default::default(), analyzer: ctx };
        analysis.get_root_info();
        let ctx = analysis.analyzer;

        for FixInfo { block_id, return_vars } in ctx.fixes.into_iter() {
            let block = &mut lowered.blocks[block_id];
            block.end = FlatBlockEnd::Return(
                return_vars
                    .iter()
                    .map(|var_info| match var_info {
                        ValueInfo::Var(var_usage) => *var_usage,
                        ValueInfo::Unit
                        | ValueInfo::StructConstruct { .. }
                        | ValueInfo::EnumConstruct { .. } => {
                            panic!("Value info is not final.")
                        }
                    })
                    .collect_vec(),
            )
        }
    }
}
#[allow(dead_code)]
pub struct ReturnOptimizerContext<'a> {
    lowered: &'a FlatLowered,
    unit_ty: semantic::TypeId,

    /// The list of fixes that should be applied.
    fixes: Vec<FixInfo>,
}
impl ReturnOptimizerContext<'_> {
    fn get_var_info(&self, var_usage: &VarUsage) -> ValueInfo {
        if self.lowered.variables[var_usage.var_id].ty == self.unit_ty {
            ValueInfo::Unit
        } else {
            ValueInfo::Var(*var_usage)
        }
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
    /// The value is avaialble through the given var usage.
    Var(VarUsage),
    /// The value is a unit.
    Unit,
    /// The value is the result of a StructConstruct statement.
    StructConstruct {
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

impl ValueInfo {
    /// Applies the given function to the value info.
    fn apply<F>(&mut self, f: &F)
    where
        F: Fn(&VarUsage) -> ValueInfo,
    {
        match self {
            ValueInfo::Var(var_usage) => *self = f(var_usage),
            ValueInfo::StructConstruct { ref mut var_infos } => {
                for var_info in var_infos.iter_mut() {
                    var_info.apply(f);
                }
            }
            ValueInfo::EnumConstruct { ref mut var_info, .. } => {
                var_info.apply(f);
            }
            ValueInfo::Unit => {}
        }
    }

    /// Updates the value to the state before the StructDeconstruct statement.
    /// Returns an error if the value info is invalid before the statement.
    fn apply_deconstruct(&mut self, stmt: &StatementStructDestructure) -> Result<(), ()> {
        match self {
            ValueInfo::Var(var_usage) => {
                if stmt.outputs.contains(&var_usage.var_id) {
                    Err(())
                } else {
                    Ok(())
                }
            }
            ValueInfo::StructConstruct { var_infos } => {
                let mut cancels_out = var_infos.len() == stmt.outputs.len();

                let mut has_errors = false;
                for (var_info, output) in var_infos.iter_mut().zip(stmt.outputs.iter()) {
                    has_errors |= var_info.apply_deconstruct(stmt).is_err();
                    if let ValueInfo::Var(var_usage) = var_info {
                        if &var_usage.var_id != output {
                            cancels_out = false;
                        }
                    }
                }

                if cancels_out {
                    // If the StructDeconstruct cancels out the StructConstruct, then its ok
                    // for StructConstruct inputs to be invalidated, otherwise we sould return an
                    // error.
                    *self = ValueInfo::Var(stmt.input);
                } else if has_errors {
                    return Err(());
                }

                Ok(())
            }
            ValueInfo::EnumConstruct { ref mut var_info, .. } => var_info.apply_deconstruct(stmt),
            ValueInfo::Unit => Ok(()),
        }
    }

    /// Updates the value to the expected value before the match arm.
    /// Returns an error if the value info is invalid before the statement.
    fn apply_match_arm(&mut self, input: &ValueInfo, arm: &MatchArm) -> Result<(), ()> {
        match self {
            ValueInfo::Var(var_usage) => {
                if arm.var_ids == [var_usage.var_id] {
                    Err(())
                } else {
                    Ok(())
                }
            }
            ValueInfo::StructConstruct { ref mut var_infos } => {
                for var_info in var_infos.iter_mut() {
                    var_info.apply_match_arm(input, arm)?;
                }

                Ok(())
            }
            ValueInfo::EnumConstruct { ref mut var_info, variant } => {
                if *variant == arm.variant_id {
                    let cancels_out = match **var_info {
                        ValueInfo::Unit => true,
                        ValueInfo::Var(var_usage) if arm.var_ids == [var_usage.var_id] => true,
                        _ => false,
                    };

                    if cancels_out {
                        // If the arm recreates the relevent enum variant, then the arm
                        // assuming the other arms also cancel out.
                        *self = input.clone();
                        return Ok(());
                    }
                }

                var_info.apply_match_arm(input, arm)
            }
            ValueInfo::Unit => Ok(()),
        }
    }
}

/// Information about the current state of the analyzer.
/// Used to track the value that should be returned from the function at the current
/// analysis point or None if it is unknown.
/// If is_final() returns true, the function can return early as the return value is alread
/// known.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AnalyzerInfo {
    opt_returned_vars: Option<Vec<ValueInfo>>,
}

impl AnalyzerInfo {
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

    // Replaces occurrences of `var_id` with `var_info`.
    fn replace(&mut self, var_id: VariableId, var_info: ValueInfo) {
        self.apply(&|var_usage| {
            if var_usage.var_id == var_id { var_info.clone() } else { ValueInfo::Var(*var_usage) }
        });
    }

    fn invalidate(&mut self) {
        self.opt_returned_vars = None;
    }

    fn apply_deconstruct(&mut self, stmt: &StatementStructDestructure) {
        let Some(ref mut returned_vars) = self.opt_returned_vars else { return };

        for var_info in returned_vars.iter_mut() {
            if var_info.apply_deconstruct(stmt).is_err() {
                self.invalidate();
                return;
            }
        }
    }

    fn apply_match_arm(&mut self, input: &ValueInfo, arm: &MatchArm) {
        let Some(ref mut returned_vars) = self.opt_returned_vars else { return };

        for var_info in returned_vars.iter_mut() {
            if var_info.apply_match_arm(input, arm).is_err() {
                self.invalidate();
                return;
            }
        }
    }

    fn is_final(&self) -> bool {
        let Some(ref returned_vars) = self.opt_returned_vars else { return false };

        returned_vars.iter().all(|var_info| match var_info {
            ValueInfo::Var(_) => true,
            ValueInfo::StructConstruct { .. } => false,
            ValueInfo::EnumConstruct { .. } => false,
            // TODO(ilya): Support unit type here.
            ValueInfo::Unit => false,
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
                info.replace(
                    *output,
                    ValueInfo::StructConstruct {
                        var_infos: inputs.iter().map(|input| ValueInfo::Var(*input)).collect(),
                    },
                );
            }

            Statement::StructDestructure(stmt) => info.apply_deconstruct(stmt),
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

        if info.is_final() {
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

        let input = self.get_var_info(input);
        let mut opt_prev_info = None;
        for (arm, info) in arms.iter().zip(infos) {
            let mut curr_info = info.clone();
            curr_info.apply_match_arm(&input, arm);

            if !curr_info.is_final() {
                return AnalyzerInfo { opt_returned_vars: None };
            }

            if let Some(prev_info) = &opt_prev_info {
                if prev_info != &curr_info {
                    return AnalyzerInfo { opt_returned_vars: None };
                }
            } else {
                opt_prev_info = Some(curr_info);
            }
        }

        self.fixes.push(FixInfo {
            block_id,
            return_vars: opt_prev_info.unwrap().opt_returned_vars.unwrap(),
        });

        AnalyzerInfo { opt_returned_vars: None }
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        vars: &'a [VarUsage],
    ) -> Self::Info {
        // TODO(ilya): Support unit type here.
        AnalyzerInfo {
            opt_returned_vars: Some(
                vars.iter().map(|var_usage| ValueInfo::Var(*var_usage)).collect(),
            ),
        }
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        _data: &VarUsage,
    ) -> Self::Info {
        AnalyzerInfo { opt_returned_vars: None }
    }
}
