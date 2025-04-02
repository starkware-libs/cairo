#[cfg(test)]
#[path = "early_unsafe_panic_test.rs"]
mod test;

use std::collections::HashSet;

use cairo_lang_filesystem::flag::flag_unsafe_panic;
use cairo_lang_semantic::corelib::{core_submodule, get_function_id};
use cairo_lang_utils::Intern;
use itertools::zip_eq;

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::db::LoweringGroup;
use crate::ids::{FunctionId, FunctionLongId, LocationId};
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchExternInfo, MatchInfo, Statement, StatementCall,
    VarUsage,
};

/// Adds an early unsafe_panic when we detect that `return` is unreachable from a certain point in
/// the code. This step is needed to avoid to avoid issues with undroppable references in sierra to
/// casm.
///
/// This step might replace a match on an empty enum with a call to unsafe_panic and we rely on the
/// 'trim_unreachable' optimization to clean that up.
pub fn early_unsafe_panic(db: &dyn LoweringGroup, lowered: &mut FlatLowered) {
    if !flag_unsafe_panic(db.upcast()) || lowered.blocks.is_empty() {
        return;
    }

    let semantic_db = db.upcast();

    let debug_submodule = core_submodule(semantic_db, "debug");

    let libfuncs_with_sideffect = HashSet::from_iter(
        [get_function_id(semantic_db, debug_submodule, "print".into(), vec![])]
            .map(|semantic_func_id| FunctionLongId::Semantic(semantic_func_id).intern(db)),
    );

    let ctx = UnsafePanicContext { fixes: vec![], libfuncs_with_sideffect };
    let mut analysis = BackAnalysis::new(lowered, ctx);
    let fixes = if let ReachableSideEffects::Unreachable(location) = analysis.get_root_info() {
        vec![((BlockId::root(), 0), location)]
    } else {
        analysis.analyzer.fixes
    };

    let semantic_db = db.upcast();
    let panics = core_submodule(semantic_db, "panics");
    let panic_func_id = FunctionLongId::Semantic(get_function_id(
        semantic_db,
        panics,
        "unsafe_panic".into(),
        vec![],
    ))
    .intern(db);

    for ((block_id, statement_idx), location) in fixes {
        let block = &mut lowered.blocks[block_id];
        block.statements.truncate(statement_idx);

        block.end = FlatBlockEnd::Match {
            info: MatchInfo::Extern(MatchExternInfo {
                arms: vec![],
                location,
                function: panic_func_id,
                inputs: vec![],
            }),
        }
    }
}

pub struct UnsafePanicContext {
    /// The list of blocks where we can insert unsafe_panic.
    fixes: Vec<(StatementLocation, LocationId)>,

    /// libfuncs with side effects that we need to ignore.
    libfuncs_with_sideffect: HashSet<FunctionId>,
}

/// Can this state lead to any return.
#[derive(Clone, Default, PartialEq, Debug)]
pub enum ReachableSideEffects {
    /// Some return statement or statement with side effect is reachable.
    #[default]
    Reachable,
    /// No return statement or statement with side effect is reachable.
    /// holds the location of the closest match with no returning arms.
    Unreachable(LocationId),
}

impl<'a> Analyzer<'a> for UnsafePanicContext {
    type Info = ReachableSideEffects;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &Statement,
    ) {
        match stmt {
            Statement::Call(StatementCall { function, .. })
                if self.libfuncs_with_sideffect.contains(function) =>
            {
                // if we call a function with side effect, we can assume that the return is
                // reachable.
                if let ReachableSideEffects::Unreachable(locations) = *info {

                    self.fixes.push((statement_location, locations));
                    *info = ReachableSideEffects::Reachable
                }
            }
            _ => {}
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
