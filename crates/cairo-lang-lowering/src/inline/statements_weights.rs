use cairo_lang_utils::casts::IntoOrPanic;

use crate::db::LoweringGroup;
use crate::{FlatBlockEnd, FlatLowered, Statement};

/// Trait for calculating the weight of a lowered function, for the purpose of inlining.
pub trait InlineWeight {
    /// The weight of calling the function.
    fn calling_weight(&self, db: &dyn LoweringGroup, lowered: &FlatLowered) -> isize;
    /// The weight of a statement in the lowered function.
    fn statement_weight(&self, db: &dyn LoweringGroup, statement: &Statement) -> isize;
    /// The weight of the block end in the lowered function.
    fn block_end_weight(&self, db: &dyn LoweringGroup, block_end: &FlatBlockEnd) -> isize;
    /// The weight of the entire lowered function.
    fn lowered_weight(&self, db: &dyn LoweringGroup, lowered: &FlatLowered) -> isize {
        self.calling_weight(db, lowered)
            + lowered
                .blocks
                .iter()
                .map(|(_, block)| {
                    block
                        .statements
                        .iter()
                        .map(|statement| self.statement_weight(db, statement))
                        .sum::<isize>()
                        + self.block_end_weight(db, &block.end)
                })
                .sum::<isize>()
    }
}

/// A simple inline weight that gives a weight of 1 to each statement and block end.
pub struct SimpleInlineWeight;
impl InlineWeight for SimpleInlineWeight {
    fn calling_weight(&self, _db: &dyn LoweringGroup, _lowered: &FlatLowered) -> isize {
        0
    }

    fn statement_weight(&self, _db: &dyn LoweringGroup, _statement: &Statement) -> isize {
        1
    }

    fn block_end_weight(&self, _db: &dyn LoweringGroup, _block_end: &FlatBlockEnd) -> isize {
        1
    }
}

/// Try to approximate the weight of a lowered function by counting the number of casm statements it
/// will add to the code.
pub struct ApproxCasmInlineWeight;
impl InlineWeight for ApproxCasmInlineWeight {
    fn calling_weight(&self, _db: &dyn LoweringGroup, lowered: &FlatLowered) -> isize {
        -lowered.parameters.len().into_or_panic::<isize>()
    }
    fn statement_weight(&self, db: &dyn LoweringGroup, statement: &Statement) -> isize {
        match statement {
            // TODO(TomerStarkware): give a better approximation for the weight of libfuncs.

            // for user function the weight is equal to the number of values pushed to the stack
            // (we approximate the size of the inputs to be 1) plus 1 for the call itself.
            // for libfuncs we give a weight equal to the number of inputs, since we don't know the
            // libfunc's body.
            Statement::Call(statement_call) => (statement_call.inputs.len()
                + if statement_call.function.body(db).unwrap().is_some() { 1 } else { 0 })
            .into_or_panic(),
            _ => 0,
        }
    }

    fn block_end_weight(&self, _db: &dyn LoweringGroup, block_end: &FlatBlockEnd) -> isize {
        match block_end {
            // Return are removed when the function is inlined.
            FlatBlockEnd::Return(_) => 0,
            FlatBlockEnd::NotSet => 0,
            FlatBlockEnd::Panic(_) => 0,
            FlatBlockEnd::Goto(_, _) => 1,
            FlatBlockEnd::Match { info } => (1 + info.arms().len()).into_or_panic(),
        }
    }

    /// The weight of a lowered function is the sum of the weights of its blocks minus the number of
    /// parameters since they are not pushed on the stack when the function is inlined.
    fn lowered_weight(&self, db: &dyn LoweringGroup, lowered: &FlatLowered) -> isize {
        lowered
            .blocks
            .iter()
            .map(|(_, block)| {
                block
                    .statements
                    .iter()
                    .map(|statement| self.statement_weight(db, statement))
                    .sum::<isize>()
                    + self.block_end_weight(db, &block.end)
            })
            .sum::<isize>()
    }
}
