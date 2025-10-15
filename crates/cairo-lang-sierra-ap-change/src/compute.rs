use cairo_lang_sierra::algorithm::topological_order::reverse_topological_ordering;
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::ids::{ConcreteTypeId, FunctionId};
use cairo_lang_sierra::program::{Program, Statement, StatementIdx};
use cairo_lang_sierra_type_size::{ProgramRegistryInfo, TypeSizeMap};
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::ap_change_info::ApChangeInfo;
use crate::core_libfunc_ap_change::{self, InvocationApChangeInfoProvider};
use crate::{ApChange, ApChangeError};

/// Helper to implement the `InvocationApChangeInfoProvider` for the equation generation.
struct InvocationApChangeInfoProviderForEqGen<'a, TokenUsages: Fn(CostTokenType) -> usize> {
    /// Registry for providing the sizes of the types.
    type_sizes: &'a TypeSizeMap,
    /// Closure providing the token usages for the invocation.
    token_usages: TokenUsages,
}

impl<TokenUsages: Fn(CostTokenType) -> usize> InvocationApChangeInfoProvider
    for InvocationApChangeInfoProviderForEqGen<'_, TokenUsages>
{
    fn type_size(&self, ty: &ConcreteTypeId) -> usize {
        self.type_sizes[ty].into_or_panic()
    }

    fn token_usages(&self, token_type: CostTokenType) -> usize {
        (self.token_usages)(token_type)
    }
}

/// A base to start ap tracking from.
#[derive(Clone, Debug)]
enum ApTrackingBase {
    FunctionStart(FunctionId),
    #[expect(dead_code)]
    EnableStatement(StatementIdx),
}

/// The information for ap tracking of a statement.
#[derive(Clone, Debug)]
struct ApTrackingInfo {
    /// The base tracking from.
    base: ApTrackingBase,
    /// The ap-change from the base.
    ap_change: usize,
}

/// Helper for calculating the ap-changes of a program.
struct ApChangeCalcHelper<'a, TokenUsages: Fn(StatementIdx, CostTokenType) -> usize> {
    /// The program.
    program: &'a Program,
    /// The program info containing registry and type sizes.
    program_info: &'a ProgramRegistryInfo,
    /// Closure providing the token usages for the invocation.
    token_usages: TokenUsages,
    /// The branches of the statement.
    branches: Vec<Vec<(ApChange, StatementIdx)>>,
    /// The ap_change of functions with known ap changes.
    function_ap_change: OrderedHashMap<FunctionId, usize>,
    /// The statement info per statement.
    infos: Vec<StatementInfo>,
    /// The variables for ap alignment.
    variable_values: OrderedHashMap<StatementIdx, usize>,
}
#[derive(Clone, Default)]
struct StatementInfo {
    /// The size of allocated locals until the statement.
    locals_size: usize,
    /// The lower bound of an ap-change to the furthest return from the statement.
    known_ap_change_to_return: Option<usize>,
    /// The ap tracking information per statement.
    tracking_info: Option<ApTrackingInfo>,
    /// The effective ap change from the statement's base.
    effective_ap_change_from_base: Option<usize>,
}
impl<'a, TokenUsages: Fn(StatementIdx, CostTokenType) -> usize>
    ApChangeCalcHelper<'a, TokenUsages>
{
    /// Creates a new helper.
    fn new(
        program: &'a Program,
        program_info: &'a ProgramRegistryInfo,
        token_usages: TokenUsages,
    ) -> Result<Self, ApChangeError> {
        Ok(Self {
            program,
            program_info,
            token_usages,
            function_ap_change: Default::default(),
            infos: vec![StatementInfo::default(); program.statements.len()],
            variable_values: Default::default(),
            branches: vec![vec![]; program.statements.len()],
        })
    }

    /// Calculates the locals size and function ap changes.
    fn calc_locals_and_function_ap_changes(&mut self) -> Result<(), ApChangeError> {
        let rev_ordering = self.known_ap_change_reverse_topological_order()?;
        for idx in rev_ordering.iter().rev() {
            self.calc_locals_for_statement(*idx);
        }
        for idx in rev_ordering {
            self.calc_known_ap_change_for_statement(idx)?;
        }
        self.function_ap_change = self
            .program
            .funcs
            .iter()
            .filter_map(|f| {
                self.infos[f.entry_point.0]
                    .known_ap_change_to_return
                    .map(|ap_change| (f.id.clone(), ap_change))
            })
            .collect();
        Ok(())
    }

    /// Calculates the locals size for a statement.
    fn calc_locals_for_statement(&mut self, idx: StatementIdx) {
        for (ap_change, target) in &self.branches[idx.0] {
            match ap_change {
                ApChange::AtLocalsFinalization(x) => {
                    self.infos[target.0].locals_size = self.infos[idx.0].locals_size + x;
                }
                ApChange::Unknown | ApChange::FinalizeLocals => {}
                ApChange::FromMetadata
                | ApChange::FunctionCall(_)
                | ApChange::EnableApTracking
                | ApChange::Known(_)
                | ApChange::DisableApTracking => {
                    self.infos[target.0].locals_size = self.infos[idx.0].locals_size;
                }
            }
        }
    }

    /// Calculates the lower bound of an ap-change to the furthest return per statement.
    /// If it is unknown does not set it.
    fn calc_known_ap_change_for_statement(
        &mut self,
        idx: StatementIdx,
    ) -> Result<(), ApChangeError> {
        let mut max_change = 0;
        for (ap_change, target) in &self.branches[idx.0] {
            let Some(target_ap_change) = self.infos[target.0].known_ap_change_to_return else {
                return Ok(());
            };
            if let Some(ap_change) = self.branch_ap_change(idx, ap_change, |id| {
                self.infos[self.func_entry_point(id).ok()?.0].known_ap_change_to_return
            }) {
                max_change = max_change.max(target_ap_change + ap_change);
            } else {
                return Ok(());
            };
        }
        self.infos[idx.0].known_ap_change_to_return = Some(max_change);
        Ok(())
    }

    /// Returns the topological ordering of the program statements for fully known ap-changes.
    fn known_ap_change_reverse_topological_order(
        &self,
    ) -> Result<Vec<StatementIdx>, ApChangeError> {
        reverse_topological_ordering(
            false,
            (0..self.program.statements.len()).map(StatementIdx),
            self.program.statements.len(),
            |idx| {
                let mut res = vec![];
                for (ap_change, target) in &self.branches[idx.0] {
                    res.push(*target);
                    if let ApChange::FunctionCall(id) = ap_change {
                        res.push(self.func_entry_point(id)?);
                    }
                }
                Ok(res)
            },
            |_| unreachable!("Cycle isn't an error."),
        )
    }

    /// Returns the topological ordering of the program statements where tracked ap changes give the
    /// ordering.
    fn tracked_ap_change_reverse_topological_order(
        &self,
    ) -> Result<Vec<StatementIdx>, ApChangeError> {
        reverse_topological_ordering(
            false,
            (0..self.program.statements.len()).map(StatementIdx),
            self.program.statements.len(),
            |idx| {
                Ok(self.branches[idx.0].iter().flat_map(|(ap_change, target)| match ap_change {
                    ApChange::Unknown => None,
                    ApChange::FunctionCall(id) => {
                        self.function_ap_change.contains_key(id).then_some(*target)
                    }
                    ApChange::Known(_)
                    | ApChange::DisableApTracking
                    | ApChange::FromMetadata
                    | ApChange::AtLocalsFinalization(_)
                    | ApChange::FinalizeLocals
                    | ApChange::EnableApTracking => Some(*target),
                }))
            },
            |_| unreachable!("Cycle isn't an error."),
        )
    }

    /// Calculates the tracking information for a statement.
    fn calc_tracking_info_for_statement(&mut self, idx: StatementIdx) {
        for (ap_change, target) in &self.branches[idx.0] {
            if matches!(ap_change, ApChange::EnableApTracking) {
                self.infos[target.0].tracking_info = Some(ApTrackingInfo {
                    base: ApTrackingBase::EnableStatement(idx),
                    ap_change: 0,
                });
                continue;
            }
            let Some(mut base_info) = self.infos[idx.0].tracking_info.clone() else {
                continue;
            };
            if let Some(ap_change) =
                self.branch_ap_change(idx, ap_change, |id| self.function_ap_change.get(id).cloned())
            {
                base_info.ap_change += ap_change;
            } else {
                continue;
            }
            let target_info = &mut self.infos[target.0].tracking_info;
            *target_info = Some(match target_info.take() {
                Some(mut e) => {
                    e.ap_change = e.ap_change.max(base_info.ap_change);
                    e
                }
                None => base_info,
            });
        }
    }

    /// Calculates the effective ap change for a statement, and the variables for ap alignment.
    fn calc_effective_ap_change_and_variables_per_statement(&mut self, idx: StatementIdx) {
        let Some(base_info) = self.infos[idx.0].tracking_info.clone() else {
            return;
        };
        if matches!(self.program.get_statement(&idx), Some(Statement::Return(_))) {
            if let ApTrackingBase::FunctionStart(id) = base_info.base
                && let Some(func_change) = self.function_ap_change.get(&id)
            {
                self.infos[idx.0].effective_ap_change_from_base = Some(*func_change);
            }
            return;
        }
        let mut source_ap_change = None;
        let mut paths_ap_change = vec![];
        for (ap_change, target) in &self.branches[idx.0] {
            if matches!(ap_change, ApChange::EnableApTracking) {
                continue;
            }
            let Some(change) = self
                .branch_ap_change(idx, ap_change, |id| self.function_ap_change.get(id).cloned())
            else {
                source_ap_change = Some(base_info.ap_change);
                continue;
            };
            let Some(target_ap_change) = self.infos[target.0].effective_ap_change_from_base else {
                continue;
            };
            let calc_ap_change = target_ap_change - change;
            paths_ap_change.push((target, calc_ap_change));
            if let Some(source_ap_change) = &mut source_ap_change {
                *source_ap_change = (*source_ap_change).min(calc_ap_change);
            } else {
                source_ap_change = Some(calc_ap_change);
            }
        }
        if let Some(source_ap_change) = source_ap_change {
            self.infos[idx.0].effective_ap_change_from_base = Some(source_ap_change);
            for (target, path_ap_change) in paths_ap_change {
                if path_ap_change != source_ap_change {
                    self.variable_values.insert(*target, path_ap_change - source_ap_change);
                }
            }
        }
    }

    /// Gets the actual ap-change of a branch.
    fn branch_ap_change(
        &self,
        idx: StatementIdx,
        ap_change: &ApChange,
        func_ap_change: impl Fn(&FunctionId) -> Option<usize>,
    ) -> Option<usize> {
        match ap_change {
            ApChange::Unknown | ApChange::DisableApTracking => None,
            ApChange::Known(x) => Some(*x),
            ApChange::FromMetadata
            | ApChange::AtLocalsFinalization(_)
            | ApChange::EnableApTracking => Some(0),
            ApChange::FinalizeLocals => Some(self.infos[idx.0].locals_size),
            ApChange::FunctionCall(id) => func_ap_change(id).map(|x| 2 + x),
        }
    }

    /// Calculates the branches for all statements.
    fn calc_branches(&mut self) -> Result<(), ApChangeError> {
        for idx in (0..self.program.statements.len()).map(StatementIdx) {
            let Statement::Invocation(invocation) = self.program.get_statement(&idx).unwrap()
            else {
                continue;
            };
            let libfunc = self.program_info.registry.get_libfunc(&invocation.libfunc_id)?;
            self.branches[idx.0] = core_libfunc_ap_change::core_libfunc_ap_change(
                libfunc,
                &InvocationApChangeInfoProviderForEqGen {
                    type_sizes: &self.program_info.type_sizes,
                    token_usages: |token_type| (self.token_usages)(idx, token_type),
                },
            )
            .into_iter()
            .zip(&invocation.branches)
            .map(|(ap_change, branch_info)| (ap_change, idx.next(&branch_info.target)))
            .collect();
        }
        Ok(())
    }

    /// Returns the entry point of a function.
    fn func_entry_point(&self, id: &FunctionId) -> Result<StatementIdx, ApChangeError> {
        Ok(self.program_info.registry.get_function(id)?.entry_point)
    }
}

/// Calculates ap change information for a given program.
pub fn calc_ap_changes<TokenUsages: Fn(StatementIdx, CostTokenType) -> usize>(
    program: &Program,
    program_info: &ProgramRegistryInfo,
    token_usages: TokenUsages,
) -> Result<ApChangeInfo, ApChangeError> {
    let mut helper = ApChangeCalcHelper::new(program, program_info, token_usages)?;
    helper.calc_branches()?;
    helper.calc_locals_and_function_ap_changes()?;
    let ap_tracked_reverse_topological_ordering =
        helper.tracked_ap_change_reverse_topological_order()?;
    // Setting tracking info for function entry points.
    for f in &program.funcs {
        helper.infos[f.entry_point.0].tracking_info = Some(ApTrackingInfo {
            base: ApTrackingBase::FunctionStart(f.id.clone()),
            ap_change: 0,
        });
    }
    for idx in ap_tracked_reverse_topological_ordering.iter().rev() {
        helper.calc_tracking_info_for_statement(*idx);
    }
    for idx in ap_tracked_reverse_topological_ordering {
        helper.calc_effective_ap_change_and_variables_per_statement(idx);
    }
    Ok(ApChangeInfo {
        variable_values: helper.variable_values,
        function_ap_change: helper.function_ap_change,
    })
}
