#[cfg(test)]
#[path = "early_unsafe_panic_test.rs"]
mod test;

use std::collections::HashSet;

use cairo_lang_defs::ids::ExternFunctionId;
use cairo_lang_filesystem::flag::flag_unsafe_panic;
use cairo_lang_semantic::helper::ModuleHelper;
use itertools::zip_eq;

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::db::LoweringGroup;
use crate::ids::{LocationId, SemanticFunctionIdEx};
use crate::{
    BlockEnd, BlockId, Lowered, MatchExternInfo, MatchInfo, Statement, StatementCall, VarUsage,
};

/// Adds an early unsafe_panic when we detect that `return` is unreachable from a certain point in
/// the code. This step is needed to avoid to avoid issues with undroppable references in sierra to
/// casm.
///
/// This step might replace a match on an empty enum with a call to unsafe_panic and we rely on the
/// 'trim_unreachable' optimization to clean that up.
pub fn early_unsafe_panic(db: &dyn LoweringGroup, lowered: &mut Lowered) {
    if !flag_unsafe_panic(db) || lowered.blocks.is_empty() {
        return;
    }

    let core = ModuleHelper::core(db);
    let libfuncs_with_sideffect = HashSet::from_iter([
        core.submodule("debug").extern_function_id("print"),
        core.submodule("internal").extern_function_id("trace"),
    ]);

    let ctx = UnsafePanicContext { db, fixes: vec![], libfuncs_with_sideffect };
    let mut analysis = BackAnalysis::new(lowered, ctx);
    let fixes = if let ReachableSideEffects::Unreachable(location) = analysis.get_root_info() {
        vec![((BlockId::root(), 0), location)]
    } else {
        analysis.analyzer.fixes
    };

    let panic_func_id = core.submodule("panics").function_id("unsafe_panic", vec![]).lowered(db);
    for ((block_id, statement_idx), location) in fixes {
        let block = &mut lowered.blocks[block_id];
        block.statements.truncate(statement_idx);

        block.end = BlockEnd::Match {
            info: MatchInfo::Extern(MatchExternInfo {
                arms: vec![],
                location,
                function: panic_func_id,
                inputs: vec![],
            }),
        }
    }
}

pub struct UnsafePanicContext<'a> {
    db: &'a dyn LoweringGroup,

    /// The list of blocks where we can insert unsafe_panic.
    fixes: Vec<(StatementLocation, LocationId)>,

    /// libfuncs with side effects that we need to ignore.
    libfuncs_with_sideffect: HashSet<ExternFunctionId>,
}

impl UnsafePanicContext<'_> {
    /// Returns true if the statement has side effects.
    pub fn has_side_effects(&self, stmt: &Statement) -> bool {
        if let Statement::Call(StatementCall { function, .. }) = stmt {
            let Some((extern_fn, _gargs)) = function.get_extern(self.db) else {
                return false;
            };

            if self.libfuncs_with_sideffect.contains(&extern_fn) {
                return true;
            }
        }

        false
    }
}

/// Can this state lead to a return or a statement with side effect.
#[derive(Clone, Default, PartialEq, Debug)]
pub enum ReachableSideEffects {
    /// Some return statement or statement with side effect is reachable.
    #[default]
    Reachable,
    /// No return statement or statement with side effect is reachable.
    /// holds the location of the closest match with no returning arms.
    Unreachable(LocationId),
}

impl<'a> Analyzer<'a> for UnsafePanicContext<'_> {
    type Info = ReachableSideEffects;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &Statement,
    ) {
        if self.has_side_effects(stmt) {
            if let ReachableSideEffects::Unreachable(locations) = *info {
                self.fixes.push((statement_location, locations));
                *info = ReachableSideEffects::Reachable
            }
        }
    }

    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_info: &'a MatchInfo,
        infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info {
        let mut res = ReachableSideEffects::Unreachable(*match_info.location());
        for (arm, info) in zip_eq(match_info.arms(), infos) {
            match info {
                ReachableSideEffects::Reachable => {
                    res = ReachableSideEffects::Reachable;
                }
                ReachableSideEffects::Unreachable(l) => self.fixes.push(((arm.block_id, 0), l)),
            }
        }

        if let ReachableSideEffects::Unreachable(location) = res {
            self.fixes.push((statement_location, location));
        }

        res
    }

    fn info_from_return(&mut self, _: StatementLocation, _vars: &'a [VarUsage]) -> Self::Info {
        ReachableSideEffects::Reachable
    }
}
