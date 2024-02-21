use cairo_lang_diagnostics::Maybe;
use cairo_lang_utils::casts::IntoOrPanic;

use crate::db::LoweringGroup;
use crate::{FlatBlockEnd, FlatLowered, Statement};

/// Trait for calculating the weight of a lowered function, for the purpose of inlining.
pub trait InlineWeight {
    // The weight of a statement in the lowered function.
    fn statement_weight(&self, db: &dyn LoweringGroup, statement: &Statement) -> Maybe<i32>;
    // The weight of the block end in the lowered function.
    fn block_end_weight(&self, db: &dyn LoweringGroup, block_end: &FlatBlockEnd) -> Maybe<i32>;
    // The weight of the entire lowered function.
    fn lowered_weight(&self, db: &dyn LoweringGroup, lowered: &FlatLowered) -> Maybe<i32>;
}

/// A simple inline weight that gives a weight of 1 to each statement and block end.
pub struct SimpleInlineWeight;
impl InlineWeight for SimpleInlineWeight {
    fn statement_weight(&self, _db: &dyn LoweringGroup, _statement: &Statement) -> Maybe<i32> {
        Ok(1)
    }

    fn block_end_weight(&self, _db: &dyn LoweringGroup, _block_end: &FlatBlockEnd) -> Maybe<i32> {
        Ok(1)
    }

    fn lowered_weight(&self, db: &dyn LoweringGroup, lowered: &FlatLowered) -> Maybe<i32> {
        lowered
            .blocks
            .iter()
            .map(|(_, block)| {
                block
                    .statements
                    .iter()
                    .map(|statement| self.statement_weight(db, statement))
                    .sum::<Maybe<i32>>()
                    .and_then(|sum| Ok(sum + self.block_end_weight(db, &block.end)?))
            })
            .sum::<Maybe<i32>>()
    }
}

/// Try to approximate the weight of a lowered function by counting the number of casm statements it
/// will add to the code.
pub struct ApproxCasmInlineWeight;
impl InlineWeight for ApproxCasmInlineWeight {
    fn statement_weight(&self, db: &dyn LoweringGroup, statement: &Statement) -> Maybe<i32> {
        match statement {
            // Todo(Tomerstarkware): give a better approximation for the weight of libfuncs.
            Statement::Call(statement_call) => Ok((statement_call.inputs.len()
                + if statement_call.function.body(db)?.is_some() { 1 } else { 3 })
            .into_or_panic()),
            _ => Ok(0),
        }
    }

    fn block_end_weight(&self, _db: &dyn LoweringGroup, block_end: &FlatBlockEnd) -> Maybe<i32> {
        match block_end {
            // Return are removed when the function is inlined.
            FlatBlockEnd::Return(outputs) => Ok(-(outputs.len().into_or_panic::<i32>())),
            FlatBlockEnd::NotSet => Ok(0),
            FlatBlockEnd::Panic(_) => Ok(0),
            FlatBlockEnd::Goto(_, _) => Ok(1),
            FlatBlockEnd::Match { info } => Ok((1 + info.arms().len()).into_or_panic()),
        }
    }

    /// The weight of a lowered function is the sum of the weights of its blocks minus the number of
    /// parameters since they are not pushed on the stack when the function is inlined.
    fn lowered_weight(&self, db: &dyn LoweringGroup, lowered: &FlatLowered) -> Maybe<i32> {
        Ok(lowered
            .blocks
            .iter()
            .map(|(_, block)| {
                block
                    .statements
                    .iter()
                    .map(|statement| self.statement_weight(db, statement))
                    .sum::<Maybe<i32>>()
                    .and_then(|sum| Ok(sum + self.block_end_weight(db, &block.end)?))
            })
            .sum::<Maybe<i32>>()?
            - lowered.parameters.len().into_or_panic::<i32>())
    }
}
