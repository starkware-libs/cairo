#[cfg(test)]
#[path = "early_unsafe_panic_test.rs"]
mod test;

use std::collections::HashSet;

use cairo_lang_defs::ids::ExternFunctionId;
use cairo_lang_filesystem::flag::FlagsGroup;
use cairo_lang_semantic::helper::ModuleHelper;
use salsa::Database;

use crate::analysis::core::StatementLocation;
use crate::analysis::{DataflowAnalyzer, DataflowBackAnalysis, Direction};
use crate::ids::{LocationId, SemanticFunctionIdEx};
use crate::{BlockEnd, BlockId, Lowered, MatchExternInfo, MatchInfo, Statement, StatementCall};

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
    let analysis = DataflowBackAnalysis::new(lowered, &mut ctx);
    let result = analysis.run();

    // If the entry point itself is unreachable, add a fix for it.
    if let Reachability::Unreachable(location) = result {
        ctx.fixes.push(((BlockId::root(), 0), location));
    }

    let panic_func_id = core.submodule("panics").function_id("unsafe_panic", vec![]).lowered(db);
    for ((block_id, statement_idx), location) in ctx.fixes {
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

    /// libfuncs with side effects that we need to ignore.
    libfuncs_with_sideffect: HashSet<ExternFunctionId<'db>>,

    /// Locations where we need to insert unsafe_panic.
    pub fixes: Vec<(StatementLocation, LocationId<'db>)>,
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

/// Reachability state for a point in the program.
#[derive(Clone, Copy, Default, PartialEq, Debug)]
pub enum Reachability<'db> {
    /// Some return statement or statement with side effect is reachable.
    #[default]
    Reachable,
    /// No return statement or statement with side effect is reachable.
    /// Holds the location of the closest match with no returning arms.
    Unreachable(LocationId<'db>),
}

impl<'db, 'a> DataflowAnalyzer<'db, 'a> for UnsafePanicContext<'db> {
    type Info = Reachability<'db>;
    const DIRECTION: Direction = Direction::Backward;

    fn initial_info(&mut self, _block_id: BlockId, _block_end: &'a BlockEnd<'db>) -> Self::Info {
        Reachability::default()
    }

    fn merge(
        &mut self,
        statement_location: StatementLocation,
        location: &'a LocationId<'db>,
        infos: impl Iterator<Item = (BlockId, Self::Info)>,
    ) -> Self::Info {
        let mut result = Reachability::default();
        let mut all_unreachable = true;

        for (src, reachability) in infos {
            match reachability {
                Reachability::Reachable => {
                    all_unreachable = false;
                }
                Reachability::Unreachable(loc) => {
                    // Fix at the entry of this unreachable branch.
                    self.fixes.push(((src, 0), loc));
                }
            }
        }

        // All branches are unreachable (or there are no branches, e.g., match on `never`).
        // Use the match location as the source of unreachability.
        if all_unreachable {
            result = Reachability::Unreachable(*location);
            self.fixes.push((statement_location, *location));
        }

        result
    }

    fn transfer_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &'a Statement<'db>,
    ) {
        if self.has_side_effects(stmt)
            && let Reachability::Unreachable(loc) = *info
        {
            self.fixes.push((statement_location, loc));
            *info = Reachability::Reachable;
        }
    }
}
