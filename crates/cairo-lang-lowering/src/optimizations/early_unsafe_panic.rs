#[cfg(test)]
#[path = "early_unsafe_panic_test.rs"]
mod test;

use cairo_lang_filesystem::flag::flag_unsafe_panic;
use cairo_lang_semantic::corelib::get_core_function_id;
use cairo_lang_utils::Intern;
use itertools::zip_eq;

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::db::LoweringGroup;
use crate::ids::FunctionLongId;
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
    let fixes = if analysis.get_root_info() == ReturnState::Unreachable {
        vec![BlockId::root()]
    } else {
        analysis.analyzer.fixes
    };

    let panic_func_id =
        FunctionLongId::Semantic(get_core_function_id(db.upcast(), "unsafe_panic".into(), vec![]))
            .intern(db);

    let Some((_, var)) = lowered.variables.iter().next() else {
        return;
    };

    let location = var.location;
    for block_id in fixes {
        let block = &mut lowered.blocks[block_id];
        block.statements.truncate(0);

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
    fixes: Vec<BlockId>,
}

/// Can this state lead to any return.
#[derive(Clone, Default, PartialEq, Debug)]
pub enum ReturnState {
    /// Some return statement is reachable.
    #[default]
    Reachable,
    /// No return statement is reachable.
    /// This most likely means all flows from this state lead to a match on a never type.
    Unreachable,
}

impl<'a> Analyzer<'a> for UnsafePanicContext {
    type Info = ReturnState;

    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_info: &'a MatchInfo,
        infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info {
        let mut res = ReturnState::Unreachable;
        for (arm, info) in zip_eq(match_info.arms(), infos) {
            match info {
                ReturnState::Reachable => {
                    res = ReturnState::Reachable;
                }
                ReturnState::Unreachable => self.fixes.push(arm.block_id),
            }
        }

        if res == ReturnState::Unreachable {
            self.fixes.push(statement_location.0);
        }

        res
    }

    fn info_from_return(&mut self, _: StatementLocation, _vars: &'a [VarUsage]) -> Self::Info {
        ReturnState::Reachable
    }
}
