use cairo_lang_semantic::TypeId;
use cairo_lang_utils::casts::IntoOrPanic;

use crate::db::LoweringGroup;
use crate::{FlatBlockEnd, FlatLowered, Statement, VarUsage, VariableId};

/// Trait for calculating the weight of a lowered function, for the purpose of inlining.
pub trait InlineWeight {
    /// The weight of calling the function.
    fn calling_weight(&self, lowered: &FlatLowered) -> isize;
    /// The weight of a statement in the lowered function.
    fn statement_weight(&self, statement: &Statement) -> isize;
    /// The weight of the block end in the lowered function.
    fn block_end_weight(&self, block_end: &FlatBlockEnd) -> isize;
    /// The weight of the entire lowered function.
    fn lowered_weight(&self, lowered: &FlatLowered) -> isize {
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
impl InlineWeight for SimpleInlineWeight {
    fn calling_weight(&self, _lowered: &FlatLowered) -> isize {
        0
    }

    fn statement_weight(&self, _statement: &Statement) -> isize {
        1
    }

    fn block_end_weight(&self, _block_end: &FlatBlockEnd) -> isize {
        1
    }
}

/// Try to approximate the weight of a lowered function by counting the number of casm statements it
/// will add to the code.
pub struct ApproxCasmInlineWeight<'a> {
    db: &'a dyn LoweringGroup,
    lowered: &'a FlatLowered,
}
impl<'a> ApproxCasmInlineWeight<'a> {
    /// Create a new `ApproxCasmInlineWeight` for the given lowered function.
    pub fn new(db: &'a dyn LoweringGroup, lowered: &'a FlatLowered) -> Self {
        Self { db, lowered }
    }
    /// Calculate the total size of the given types.
    fn tys_total_size(&self, tys: impl IntoIterator<Item = TypeId>) -> usize {
        tys.into_iter().map(|ty| self.db.type_size(ty)).sum()
    }
    /// Calculate the total size of the given variables.
    fn vars_size<'b, I: IntoIterator<Item = &'b VariableId>>(&self, vars: I) -> usize {
        self.tys_total_size(vars.into_iter().map(|v| self.lowered.variables[*v].ty))
    }
    /// Calculate the total size of the given inputs.
    fn inputs_size<'b, I: IntoIterator<Item = &'b VarUsage>>(&self, vars: I) -> usize {
        self.vars_size(vars.into_iter().map(|v| &v.var_id))
    }
}

impl<'a> InlineWeight for ApproxCasmInlineWeight<'a> {
    fn calling_weight(&self, _lowered: &FlatLowered) -> isize {
        0
    }
    fn statement_weight(&self, statement: &Statement) -> isize {
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

    fn block_end_weight(&self, block_end: &FlatBlockEnd) -> isize {
        match block_end {
            // Return are removed when the function is inlined.
            FlatBlockEnd::Return(..) => 0,
            // Goto requires the size of the variables in the mappings, as these are likely to be
            // stored for merge.
            FlatBlockEnd::Goto(_, r) => self.vars_size(r.keys()),
            // The required store for the branch parameter, as well as the branch aligns.
            FlatBlockEnd::Match { info } => info.arms().len() + self.inputs_size(info.inputs()),
            FlatBlockEnd::Panic(_) | FlatBlockEnd::NotSet => unreachable!(),
        }
        .into_or_panic()
    }
}
