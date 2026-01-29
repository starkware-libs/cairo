#[cfg(test)]
#[path = "early_unsafe_panic_test.rs"]
mod test;

use std::collections::HashSet;

use cairo_lang_defs::ids::ExternFunctionId;
use cairo_lang_filesystem::flag::FlagsGroup;
use cairo_lang_semantic::helper::ModuleHelper;
use salsa::Database;

use crate::analysis::core::StatementLocation;
use crate::analysis::{DataflowAnalyzer, DataflowBackAnalysis, Direction, Edge};
use crate::ids::{LocationId, SemanticFunctionIdEx};
use crate::{
    Block, BlockEnd, BlockId, Lowered, MatchExternInfo, MatchInfo, Statement, StatementCall,
};

/// Adds an early unsafe_panic when we detect that `return` is unreachable from a certain point in
/// the code. This step is needed to avoid issues with undroppable references in Sierra to CASM.
///
/// This step might replace a match on an empty enum with a call to unsafe_panic and we rely on the
/// 'trim_unreachable' optimization to clean that up.
pub fn early_unsafe_panic<'db>(db: &'db dyn Database, lowered: &mut Lowered<'db>) {
    if !db.flag_unsafe_panic() || lowered.blocks.is_empty() {
        return;
    }

    let core = ModuleHelper::core(db);
    let libfuncs_with_sideffect = HashSet::from_iter([
        core.submodule("debug").extern_function_id("print"),
        core.submodule("internal").extern_function_id("trace"),
    ]);

    let mut ctx = UnsafePanicContext { db, libfuncs_with_sideffect, fixes: Vec::new() };
    let root_info = DataflowBackAnalysis::new(lowered, &mut ctx).run();

    // If the root block is completely unreachable (no path to return), replace entire function
    // with unsafe_panic from the start.
    let fixes = if let ReachableSideEffects::Unreachable(location) = root_info {
        vec![((BlockId::root(), 0), location)]
    } else {
        ctx.fixes
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

pub struct UnsafePanicContext<'db> {
    db: &'db dyn Database,

    /// The list of blocks where we can insert unsafe_panic.
    fixes: Vec<(StatementLocation, LocationId<'db>)>,

    /// libfuncs with side effects that we need to ignore.
    libfuncs_with_sideffect: HashSet<ExternFunctionId<'db>>,
}

impl<'db> UnsafePanicContext<'db> {
    /// Returns true if the statement has side effects.
    pub fn has_side_effects(&self, stmt: &Statement<'db>) -> bool {
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
pub enum ReachableSideEffects<'db> {
    /// Some return statement or statement with side effect is reachable.
    #[default]
    Reachable,
    /// No return statement or statement with side effect is reachable.
    /// holds the location of the closest match with no returning arms.
    Unreachable(LocationId<'db>),
}

impl<'db, 'a> DataflowAnalyzer<'db, 'a> for UnsafePanicContext<'db> {
    type Info = ReachableSideEffects<'db>;
    const DIRECTION: Direction = Direction::Backward;

    fn transfer_block(&mut self, info: &mut Self::Info, block_id: BlockId, block: &'a Block<'db>) {
        if let BlockEnd::Match { info: match_info } = &block.end
            && let ReachableSideEffects::Unreachable(_) = info
        {
            self.fixes.push(((block_id, block.statements.len()), *match_info.location()));
        }
        if ReachableSideEffects::Reachable == *info {
            return;
        }
        for (i, stmt) in block.statements.iter().enumerate() {
            if self.has_side_effects(stmt)
                && let ReachableSideEffects::Unreachable(locations) = *info
            {
                self.fixes.push(((block_id, i), locations));
                *info = ReachableSideEffects::Reachable;
                break;
            }
        }
    }

    fn merge(
        &mut self,
        lowered: &Lowered<'db>,
        statement_location: StatementLocation,
        info1: Self::Info,
        info2: Self::Info,
    ) -> Self::Info {
        match (info1, info2) {
            (ReachableSideEffects::Reachable, _) | (_, ReachableSideEffects::Reachable) => {
                ReachableSideEffects::Reachable
            }
            // Both are unreachable.
            (ReachableSideEffects::Unreachable(_), ReachableSideEffects::Unreachable(_)) => {
                ReachableSideEffects::Unreachable(
                    lowered.blocks[statement_location.0].end.location().unwrap(),
                )
            }
        }
    }

    fn transfer_edge(&mut self, info: &Self::Info, edge: &Edge<'db, 'a>) -> Self::Info {
        if let Edge::MatchArm { arm, .. } = edge
            && let ReachableSideEffects::Unreachable(l) = info
        {
            self.fixes.push(((arm.block_id, 0), *l));
        }
        info.clone()
    }

    fn initial_info(&mut self, _block_id: BlockId, block_end: &'a BlockEnd<'db>) -> Self::Info {
        match block_end {
            BlockEnd::Match { info } => ReachableSideEffects::Unreachable(*info.location()),
            _ => ReachableSideEffects::Reachable,
        }
    }
}
