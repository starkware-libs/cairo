use cairo_lang_semantic::TypeId;
use cairo_lang_utils::casts::IntoOrPanic;
use salsa::Database;

use crate::db::LoweringGroup;
use crate::{BlockEnd, Lowered, Statement, VarUsage, VariableId};

/// Trait for calculating the weight of a lowered function, for the purpose of inlining.
pub trait InlineWeight<'db> {
    /// The weight of calling the function.
    fn calling_weight(&self, lowered: &Lowered<'db>) -> isize;
    /// The weight of a statement in the lowered function.
    fn statement_weight(&self, statement: &Statement<'db>) -> isize;
    /// The weight of the block end in the lowered function.
    fn block_end_weight(&self, block_end: &BlockEnd<'db>) -> isize;
    /// The weight of the entire lowered function.
    fn lowered_weight(&self, lowered: &Lowered<'db>) -> isize {
        self.calling_weight(lowered)
            + lowered
                .blocks
                .iter()
                .map(|(_, block)| {
                    block
                        .statements
                        .iter()
                        .map(|statement| self.statement_weight(statement))
                        .sum::<isize>()
                        + self.block_end_weight(&block.end)
                })
                .sum::<isize>()
    }
}

/// A simple inline weight that gives a weight of 1 to each statement and block end.
pub struct SimpleInlineWeight;
impl<'db> InlineWeight<'db> for SimpleInlineWeight {
    fn calling_weight(&self, _lowered: &Lowered<'db>) -> isize {
        0
    }

    fn statement_weight(&self, _statement: &Statement<'db>) -> isize {
        1
    }

    fn block_end_weight(&self, _block_end: &BlockEnd<'db>) -> isize {
        1
    }
}

/// Try to approximate the weight of a lowered function by counting the number of CASM statements it
/// will add to the code.
pub struct ApproxCasmInlineWeight<'db> {
    db: &'db dyn Database,
    lowered: &'db Lowered<'db>,
}
impl<'db> ApproxCasmInlineWeight<'db> {
    /// Create a new `ApproxCasmInlineWeight` for the given lowered function.
    pub fn new(db: &'db dyn Database, lowered: &'db Lowered<'db>) -> Self {
        Self { db, lowered }
    }
    /// Calculate the total size of the given types.
    fn tys_total_size(&self, tys: impl IntoIterator<Item = TypeId<'db>>) -> usize {
        tys.into_iter().map(|ty| self.db.type_size(ty)).sum()
    }
    /// Calculate the total size of the given variables.
    fn vars_size<'b, I: IntoIterator<Item = &'db VariableId>>(&self, vars: I) -> usize {
        self.tys_total_size(vars.into_iter().map(|v| self.lowered.variables[*v].ty))
    }
    /// Calculate the total size of the given inputs.
    fn inputs_size<'b, I: IntoIterator<Item = &'db VarUsage<'db>>>(&self, vars: I) -> usize {
        self.vars_size(vars.into_iter().map(|v| &v.var_id))
    }
}

impl<'db> InlineWeight<'db> for ApproxCasmInlineWeight<'db> {
    fn calling_weight(&self, _lowered: &Lowered<'db>) -> isize {
        0
    }
    fn statement_weight(&self, statement: &Statement<'db>) -> isize {
        match statement {
            // TODO(orizi): Add analysis of existing compilation to provide proper approximation for
            // libfunc sizes.

            // Current approximation is only based on assuming all libfunc require preparation for
            // their arguments.
            Statement::Call(statement_call) => self.inputs_size(&statement_call.inputs),
            _ => 0,
        }
        .into_or_panic()
    }

    fn block_end_weight(&self, block_end: &BlockEnd<'db>) -> isize {
        match block_end {
            // Returns are removed when the function is inlined.
            BlockEnd::Return(..) => 0,
            // Goto requires the size of the variables in the mappings, as these are likely to be
            // stored for merge.
            BlockEnd::Goto(_, r) => self.vars_size(r.keys()),
            // The required store for the branch parameter, as well as the branch aligns.
            BlockEnd::Match { info } => info.arms().len() + self.inputs_size(info.inputs()),
            BlockEnd::Panic(_) | BlockEnd::NotSet => unreachable!(),
        }
        .into_or_panic()
    }
}
