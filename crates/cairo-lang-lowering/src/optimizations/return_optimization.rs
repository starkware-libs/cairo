#[cfg(test)]
#[path = "return_optimization_test.rs"]
mod test;

use cairo_lang_semantic as semantic;

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::db::LoweringGroup;
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchEnumInfo, MatchInfo, Statement,
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
            block.end = FlatBlockEnd::Return(return_vars)
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

/// Information about a fix that should be applied to the lowering.
pub struct FixInfo {
    /// a block id of a block that can be fixed to return `return_vars`.
    block_id: BlockId,
    /// The variables that should be returned by the block.
    return_vars: Vec<VarUsage>,
}

/// The pattern that was detected in the backwards analysis.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pattern<'a> {
    /// No relevant pattern was found
    None,
    /// Found a return statement with the given returned
    Return {
        /// The return value of the user, this is the same as the last item in `returned_vars' up
        /// to remappings.
        user_return: VarUsage,
        /// The returned variables of the return statement (excluding the last one).
        returned_vars: &'a [VarUsage],
    },
    /// There is a StructConstruct followed by a EnumConstruct followed by a return.
    StructConstruct {
        /// The input to the StructConstruct statement.
        input_vars: Vec<VarUsage>,
        /// The constructed variant.
        variant: &'a semantic::ConcreteVariant,
        /// The returned variables of the return that follows the EnumConstruct.
        returned_vars: &'a [VarUsage],
    },
    /// Found an EnumConstruct whose output is returned by the function.
    EnumConstruct {
        /// The input to the EnumConstruct is either a unit type or `opt_input_var.unwrap()`.
        opt_input_var: Option<VariableId>,
        /// The constructed variant.
        variant: &'a semantic::ConcreteVariant,
        /// The returned variables of the return that follows the EnumConstruct.
        returned_vars: &'a [VarUsage],
    },
}

impl<'a> Analyzer<'a> for ReturnOptimizerContext<'_> {
    type Info = Pattern<'a>;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        stmt: &'a Statement,
    ) {
        match stmt {
            Statement::StructConstruct(StatementStructConstruct { inputs, output }) => {
                match info {
                    Pattern::EnumConstruct { opt_input_var, variant, returned_vars } => {
                        if Some(*output) == *opt_input_var {
                            *info = Pattern::StructConstruct {
                                input_vars: inputs.to_vec(),
                                variant,
                                returned_vars,
                            };
                        }
                    }
                    Pattern::Return { user_return, returned_vars } => {
                        if user_return.var_id == *output
                            || returned_vars.iter().any(|var_usage| &var_usage.var_id == output)
                        {
                            // If the output of the StructConstruct is returned we can't apply the
                            // optimization.
                            *info = Pattern::None;
                        }
                    }
                    _ => {}
                }

                // Keep the pattern across StructConstruct statement, except for the cases above.
                return;
            }

            Statement::StructDestructure(StatementStructDestructure { input, outputs }) => {
                if let Pattern::StructConstruct { input_vars, variant, returned_vars } = info {
                    if itertools::equal(
                        outputs.iter(),
                        input_vars.iter().map(|input| &input.var_id),
                    ) {
                        // We have StructConstruct and StructDestructure that cancel out.
                        // So the resulting pattern is equivalent EnumConstruct with `input`.`
                        *info = Pattern::EnumConstruct {
                            opt_input_var: Some(input.var_id),
                            variant,
                            returned_vars,
                        };
                        return;
                    }
                }
            }
            Statement::EnumConstruct(StatementEnumConstruct { variant, input, output }) => {
                if let Pattern::Return { user_return, returned_vars } = info {
                    if &user_return.var_id != output {
                        *info = Pattern::None;
                        return;
                    }

                    // If the input is a unit type accept any variable in its place,
                    // otherwise save the expected input variable.
                    let input_requirement =
                        if self.lowered.variables[input.var_id].ty == self.unit_ty {
                            None
                        } else {
                            Some(input.var_id)
                        };

                    *info = Pattern::EnumConstruct {
                        opt_input_var: input_requirement,
                        variant,
                        returned_vars,
                    };
                    return;
                }
            }
            _ => {}
        }
        *info = Pattern::None;
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        (block_id, _statement_idx): StatementLocation,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        if remapping.is_empty() {
            return;
        }

        if let Pattern::Return { user_return, returned_vars } = info {
            if returned_vars.iter().any(|var_usage| remapping.contains_key(&var_usage.var_id)) {
                // If any of the returned variables is remapped we can't apply the optimization.
                *info = Pattern::None;
                return;
            }

            if let Some(remapped_return) = remapping.get(&user_return.var_id) {
                // In case user_return is remapped we can move the return earlier and avoid the
                // remapping.
                let mut return_vars = returned_vars.to_vec();
                return_vars.pop();
                return_vars.push(*remapped_return);
                self.fixes.push(FixInfo { block_id, return_vars });

                *info = Pattern::Return { user_return: *remapped_return, returned_vars };
            }
        } else if let Pattern::StructConstruct { input_vars, variant, returned_vars } = info {
            *info = Pattern::StructConstruct {
                input_vars: input_vars
                    .iter()
                    .map(|var_usage| *remapping.get(&var_usage.var_id).unwrap_or(var_usage))
                    .collect(),
                variant,
                returned_vars,
            };
        } else {
            *info = Pattern::None;
        };
    }

    fn merge_match(
        &mut self,
        (block_id, _statement_idx): StatementLocation,
        match_info: &'a MatchInfo,
        infos: &[Self::Info],
    ) -> Self::Info {
        let MatchInfo::Enum(MatchEnumInfo { input, arms, .. }) = match_info else {
            return Pattern::None;
        };

        let [Pattern::EnumConstruct { returned_vars, .. }, ..] = infos else {
            return Pattern::None;
        };
        let mut return_vars = returned_vars.to_vec();
        for (arm, info) in arms.iter().zip(infos) {
            let Pattern::EnumConstruct { opt_input_var, variant, returned_vars } = info else {
                return Pattern::None;
            };

            if &&(arm.variant_id) != variant {
                return Pattern::None;
            }

            if let Some(var_id) = opt_input_var {
                if [*var_id] != arm.var_ids.as_slice() {
                    return Pattern::None;
                }
            }

            // If any of the returned vars is different we can't apply the optimization.
            if return_vars.len() != returned_vars.len()
                || return_vars
                    .iter()
                    .zip(returned_vars.iter())
                    .any(|(usage_a, uasge_b)| usage_a.var_id != uasge_b.var_id)
            {
                return Pattern::None;
            }
        }
        return_vars.push(*input);

        self.fixes.push(FixInfo { block_id, return_vars });
        Pattern::None
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        vars: &'a [VarUsage],
    ) -> Self::Info {
        let (user_return, returned_vars) = vars.split_last().unwrap();
        Pattern::Return { user_return: *user_return, returned_vars }
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        _data: &VarUsage,
    ) -> Self::Info {
        Pattern::None
    }
}
