#[cfg(test)]
#[path = "early_unsafe_panic_test.rs"]
mod test;

use cairo_lang_filesystem::flag::flag_unsafe_panic;
use cairo_lang_semantic::corelib::{core_submodule, get_function_id};
use cairo_lang_utils::Intern;
use itertools::zip_eq;

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::db::LoweringGroup;
use crate::ids::{FunctionLongId, LocationId};
use crate::{BlockId, FlatBlockEnd, FlatLowered, MatchExternInfo, MatchInfo, VarUsage};

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

    let ctx = UnsafePanicContext { fixes: vec![] };
    let mut analysis = BackAnalysis::new(lowered, ctx);
    let fixes = if let ReturnState::Unreachable(location) = analysis.get_root_info() {
        vec![(BlockId::root(), location)]
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

    for (block_id, location) in fixes {
        let block = &mut lowered.blocks[block_id];
        block.statements.clear();

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
    fixes: Vec<(BlockId, LocationId)>,
}

/// Can this state lead to any return.
#[derive(Clone, Default, PartialEq, Debug)]
pub enum ReturnState {
    /// Some return statement is reachable.
    #[default]
    Reachable,
    /// No return statement is reachable.
    /// holds the location of the closest match with no returning arms.
    Unreachable(LocationId),
}

impl<'a> Analyzer<'a> for UnsafePanicContext {
    type Info = ReturnState;

    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_info: &'a MatchInfo,
        infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info {
        let mut res = ReturnState::Unreachable(*match_info.location());
        for (arm, info) in zip_eq(match_info.arms(), infos) {
            match info {
                ReturnState::Reachable => {
                    res = ReturnState::Reachable;
                }
                ReturnState::Unreachable(l) => self.fixes.push((arm.block_id, l)),
            }
        }

        if let ReturnState::Unreachable(location) = res {
            self.fixes.push((statement_location.0, location));
        }

        res
    }

    fn info_from_return(&mut self, _: StatementLocation, _vars: &'a [VarUsage]) -> Self::Info {
        ReturnState::Reachable
    }
}
