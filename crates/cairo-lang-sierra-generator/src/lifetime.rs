#[cfg(test)]
#[path = "lifetime_test.rs"]
mod test;

use std::fmt::Debug;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::{BlockId, VariableId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::Itertools;
use lowering::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use lowering::borrow_check::demand::DemandReporter;
use lowering::borrow_check::Demand;
use lowering::FlatLowered;

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
    let context = VariableLifetimeContext { local_vars, res: VariableLifetimeResult::default() };
    let mut analysis =
        BackAnalysis { lowered: lowered_function, cache: Default::default(), analyzer: context };
    lowered_function.blocks.has_root()?;
    let root_demands = analysis.get_root_info();
    for var in root_demands.vars {
        assert!(matches!(var, SierraGenVar::UninitializedLocal(_)), "Unexpected variable.");
    }
    Ok(analysis.analyzer.res)
}

/// Context information for [find_variable_lifetime] and its helper functions.
struct VariableLifetimeContext<'a> {
    local_vars: &'a OrderedHashSet<VariableId>,
    res: VariableLifetimeResult,
}

pub type SierraDemand = Demand<SierraGenVar>;

impl<'a> DemandReporter<SierraGenVar> for VariableLifetimeContext<'a> {
    type IntroducePosition = DropLocation;
    type UsePosition = StatementLocation;

    fn drop(&mut self, position: DropLocation, var: SierraGenVar) {
        self.res.add_drop(var, position)
    }

    fn dup(&mut self, _position: StatementLocation, _var: SierraGenVar) {}

    fn last_use(
        &mut self,
        statement_location: StatementLocation,
        var_index: usize,
        _var: SierraGenVar,
    ) {
        self.res.last_use.insert(UseLocation { statement_location, idx: var_index });
    }

    fn unused_mapped_var(&mut self, _var: SierraGenVar) {
        panic!("Unnecessary remapping should have already been removed.")
    }
}

impl<'a> Analyzer for VariableLifetimeContext<'a> {
    type Info = SierraDemand;

    fn visit_block_start(
        &mut self,
        info: &mut Self::Info,
        block_id: BlockId,
        block: &lowering::FlatBlock,
    ) {
        self.introduce(info, &block.inputs, DropLocation::BeginningOfBlock(block_id));
    }

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &lowering::Statement,
    ) {
        self.introduce(info, &stmt.outputs(), DropLocation::PostStatement(statement_location));
        info.variables_used(self, &stmt.inputs(), statement_location);
    }

    fn visit_remapping(
        &mut self,
        info: &mut Self::Info,
        _block_id: BlockId,
        _target_block_id: BlockId,
        remapping: &lowering::VarRemapping,
    ) {
        info.apply_remapping(self, remapping.iter().map(|(dst, src)| (*dst, *src)));
        for (dst, _src) in remapping.iter() {
            if self.local_vars.contains(dst) {
                assert!(
                    info.vars.insert(SierraGenVar::UninitializedLocal(*dst)),
                    "Variable introduced multiple times."
                );
            }
        }
    }

    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_info: &lowering::MatchInfo,
        arms: &[(BlockId, Self::Info)],
    ) -> Self::Info {
        let arm_demands = arms
            .iter()
            .map(|(block_id, demand)| (demand.clone(), DropLocation::BeginningOfBlock(*block_id)))
            .collect_vec();
        let mut demand = SierraDemand::merge_demands(&arm_demands, self);
        demand.variables_used(self, &match_info.inputs(), statement_location);
        demand
    }

    fn info_from_return(
        &mut self,
        statement_location: StatementLocation,
        vars: &[VariableId],
    ) -> Self::Info {
        let mut info = SierraDemand::default();
        info.variables_used(self, vars, statement_location);
        info
    }
}
impl<'a> VariableLifetimeContext<'a> {
    fn introduce(&mut self, info: &mut SierraDemand, vars: &[VariableId], location: DropLocation) {
        info.variables_introduced(self, vars, location);
        for var_id in vars {
            if self.local_vars.contains(var_id) {
                assert!(
                    info.vars.insert(SierraGenVar::UninitializedLocal(*var_id)),
                    "Variable introduced multiple times."
                );
            }
        }
    }
}
