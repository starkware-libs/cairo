#[cfg(test)]
#[path = "lifetime_test.rs"]
mod test;

use std::fmt::Debug;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::{BlockId, VariableId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::{zip_eq, Itertools};
use lowering::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use lowering::borrow_check::demand::DemandReporter;
use lowering::borrow_check::Demand;
use lowering::{FlatLowered, VarUsage};

/// Represents the location where a drop statement for a variable should be added.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DropLocation {
    BeginningOfBlock(BlockId),
    PostStatement(StatementLocation),
}

/// Represents a location where a variable is being used.
/// Contains the statement location, and the index of the argument within this statement.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct UseLocation {
    /// The statement where the variable is used.
    pub statement_location: StatementLocation,
    /// The index of the argument within the statement.
    pub idx: usize,
}

impl Debug for UseLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}, {})", self.statement_location, self.idx)
    }
}

/// Represents a Sierra variable by its corresponding lowering [VariableId].
///
/// For example, uninitialized local variables do not have a representation as lowering
/// [VariableId], since they are created in the sierra-generation phase.
/// Instead, we refer to it as [SierraGenVar::UninitializedLocal] by the actual local variable
/// (not the uninitialized version).
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub enum SierraGenVar {
    /// Represents a regular variable.
    LoweringVar(VariableId),
    /// Represents an uninitialized local variable, by the corresponding local variable.
    UninitializedLocal(VariableId),
}

impl From<VariableId> for SierraGenVar {
    fn from(var: VariableId) -> Self {
        SierraGenVar::LoweringVar(var)
    }
}

impl From<VarUsage> for SierraGenVar {
    fn from(var_usage: VarUsage) -> Self {
        SierraGenVar::LoweringVar(var_usage.var_id)
    }
}

impl Debug for SierraGenVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LoweringVar(var) => write!(f, "v{}", var.index()),
            Self::UninitializedLocal(var) => write!(f, "UninitializedLocal(v{})", var.index()),
        }
    }
}

/// Information returned by [find_variable_lifetime] regarding the lifetime of variables.
#[derive(Default)]
pub struct VariableLifetimeResult {
    /// A set of [UseLocation] where a variable is used, but not required after.
    ///
    /// Note that a variable may be mentioned twice. For example, when it's last used within two
    /// branches.
    ///
    /// StatementLocation may point to a nonexisting statement after the end of the block -
    /// this means that the last use was in `block.end`.
    pub last_use: OrderedHashSet<UseLocation>,
    /// A map from [DropLocation] to the list of variables that should be dropped at this location.
    pub drops: OrderedHashMap<DropLocation, Vec<SierraGenVar>>,
}
impl VariableLifetimeResult {
    /// Registers where a drop statement should appear.
    fn add_drop(&mut self, var_id: SierraGenVar, drop_location: DropLocation) {
        if let Some(vars) = self.drops.get_mut(&drop_location) {
            vars.push(var_id);
        } else {
            self.drops.insert(drop_location, vec![var_id]);
        }
    }
}

/// Given the lowering of a function, returns lifetime information for all the variables.
/// See [VariableLifetimeResult].
pub fn find_variable_lifetime(
    lowered_function: &FlatLowered,
    local_vars: &OrderedHashSet<VariableId>,
) -> Maybe<VariableLifetimeResult> {
    lowered_function.blocks.has_root()?;
    let context = VariableLifetimeContext { local_vars, res: VariableLifetimeResult::default() };
    let mut analysis = BackAnalysis {
        lowered: lowered_function,
        block_info: Default::default(),
        analyzer: context,
    };

    let mut root_demands = analysis.get_root_info();

    // Introduce the parameters before starting the analysis.
    root_demands.variables_introduced(
        &mut analysis.analyzer,
        &lowered_function.parameters,
        DropLocation::BeginningOfBlock(BlockId::root()),
    );

    for var in root_demands.vars.keys() {
        match var {
            SierraGenVar::LoweringVar(var_id) => {
                panic!("v{} is used before it is introduced.", var_id.index())
            }
            SierraGenVar::UninitializedLocal(_) => {}
        }
    }
    Ok(analysis.analyzer.res)
}

/// Context information for [find_variable_lifetime] and its helper functions.
struct VariableLifetimeContext<'a> {
    local_vars: &'a OrderedHashSet<VariableId>,
    res: VariableLifetimeResult,
}

pub type SierraDemand = Demand<SierraGenVar, UseLocation>;

impl<'a> DemandReporter<SierraGenVar> for VariableLifetimeContext<'a> {
    type IntroducePosition = DropLocation;
    type UsePosition = UseLocation;

    fn drop(&mut self, position: DropLocation, var: SierraGenVar) {
        self.res.add_drop(var, position)
    }

    fn last_use(&mut self, use_location: UseLocation, _var: SierraGenVar) {
        self.res.last_use.insert(use_location);
    }
}

impl<'a> Analyzer<'_> for VariableLifetimeContext<'a> {
    type Info = SierraDemand;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &lowering::Statement,
    ) {
        self.introduce(
            info,
            &stmt.outputs(),
            statement_location,
            DropLocation::PostStatement(statement_location),
        );
        info.variables_used(
            self,
            stmt.inputs()
                .iter()
                .enumerate()
                .map(|(idx, var_id)| (var_id, UseLocation { statement_location, idx })),
        );
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        _target_block_id: BlockId,
        remapping: &lowering::VarRemapping,
    ) {
        info.apply_remapping(
            self,
            remapping.iter().enumerate().map(|(idx, (dst, src))| {
                (dst, (&src.var_id, UseLocation { statement_location, idx }))
            }),
        );
        for (idx, (dst, _src)) in remapping.iter().enumerate() {
            if self.local_vars.contains(dst) {
                assert!(
                    info.vars
                        .insert(
                            SierraGenVar::UninitializedLocal(*dst),
                            UseLocation { statement_location, idx }
                        )
                        .is_none(),
                    "Variable introduced multiple times."
                );
            }
        }
    }

    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_info: &lowering::MatchInfo,
        infos: &[Self::Info],
    ) -> Self::Info {
        let arm_demands = zip_eq(match_info.arms(), infos)
            .map(|(arm, demand)| {
                let mut demand = demand.clone();
                self.introduce(
                    &mut demand,
                    &arm.var_ids,
                    statement_location,
                    DropLocation::BeginningOfBlock(arm.block_id),
                );
                (demand, DropLocation::BeginningOfBlock(arm.block_id))
            })
            .collect_vec();
        let mut demand = SierraDemand::merge_demands(&arm_demands, self);
        demand.variables_used(
            self,
            match_info
                .inputs()
                .iter()
                .enumerate()
                .map(|(idx, var_id)| (var_id, UseLocation { statement_location, idx })),
        );
        demand
    }

    fn info_from_return(
        &mut self,
        statement_location: StatementLocation,
        vars: &[VarUsage],
    ) -> Self::Info {
        let mut info = SierraDemand::default();
        info.variables_used(
            self,
            vars.iter().enumerate().map(|(idx, VarUsage { var_id, .. })| {
                (var_id, UseLocation { statement_location, idx })
            }),
        );
        info
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        _var: &VarUsage,
    ) -> Self::Info {
        unreachable!("Panics should have been stripped in a previous phase.")
    }
}
impl<'a> VariableLifetimeContext<'a> {
    /// A wrapper for info.variables_introduced that adds demand for uninitialized locals.
    /// Note that this function is not called for the parameters of the analyzed function.
    fn introduce(
        &mut self,
        info: &mut SierraDemand,
        vars: &[VariableId],
        statement_location: StatementLocation,
        drop_location: DropLocation,
    ) {
        info.variables_introduced(self, vars, drop_location);
        for (idx, var_id) in vars.iter().enumerate() {
            if self.local_vars.contains(var_id) {
                assert!(
                    info.vars
                        .insert(
                            SierraGenVar::UninitializedLocal(*var_id),
                            UseLocation { statement_location, idx }
                        )
                        .is_none(),
                    "Variable introduced multiple times."
                );
            }
        }
    }
}
