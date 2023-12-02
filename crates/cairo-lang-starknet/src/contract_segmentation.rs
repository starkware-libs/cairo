#[cfg(test)]
#[path = "contract_segmentation_test.rs"]
mod test;

use cairo_lang_sierra::program::{BranchTarget, Program, Statement, StatementIdx};
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum SegmentationError {
    #[error("Expected a function start at index 0.")]
    NoFunctionStartAtZero,
    #[error("Jump outside of function boundaries.")]
    JumpOutsideFunction(StatementIdx),
}

/// Returns a vector of vectors, where each inner vector represents a function in the program,
/// and contains the starts (as statement indices) of the segments in the function.
#[allow(dead_code)]
fn find_segments(program: &Program) -> Result<Vec<Vec<usize>>, SegmentationError> {
    // Get the set of function entry points.
    let function_statement_ids: UnorderedHashSet<usize> =
        program.funcs.iter().map(|func| func.entry_point.0).collect();

    if !function_statement_ids.contains(&0) {
        return Err(SegmentationError::NoFunctionStartAtZero);
    }

    let mut segment_starts = Vec::<Vec<usize>>::default();
    let mut current_function = FunctionInfo::new(0);

    // Iterate over all statements and collect the segments' starts.
    for (idx, statement) in program.statements.iter().enumerate() {
        // Check if this is the beginning of a new function.
        if function_statement_ids.contains(&idx) && idx != 0 {
            segment_starts.push(current_function.finalize(idx)?);
            current_function = FunctionInfo::new(idx);
        }
        current_function.visit_statement(idx, statement)?;
    }

    segment_starts.push(current_function.finalize(program.statements.len())?);

    Ok(segment_starts)
}

/// Helper struct for [find_segments].
/// Represents a single function and its segments.
struct FunctionInfo {
    entry_point: usize,
    segment_starts: UnorderedHashSet<usize>,
    /// The maximal StatementIdx which we saw a jump to in the function.
    max_jump_in_function: usize,
    /// The statement that performed the jump to max_jump_in_function.
    max_jump_in_function_src: usize,
}
impl FunctionInfo {
    /// Creates a new FunctionInfo, for a function with the given entry point.
    fn new(entry_point: usize) -> Self {
        Self {
            entry_point,
            segment_starts: [entry_point].into_iter().collect(),
            max_jump_in_function: entry_point,
            max_jump_in_function_src: entry_point,
        }
    }

    /// Finalizes the current function handling.
    ///
    /// `function_end` is the statement index following the last statement in the function.
    ///
    /// Returns the segment starts for the function.
    fn finalize(self, function_end: usize) -> Result<Vec<usize>, SegmentationError> {
        // Check that we did not see a jump after the function's end.
        if self.max_jump_in_function >= function_end {
            return Err(SegmentationError::JumpOutsideFunction(StatementIdx(
                self.max_jump_in_function_src,
            )));
        }
        Ok(self.segment_starts.sorted())
    }

    /// Visits a statement inside the function and update the [FunctionInfo] accordingly.
    fn visit_statement(
        &mut self,
        idx: usize,
        statement: &Statement,
    ) -> Result<(), SegmentationError> {
        match statement {
            Statement::Invocation(invocation) => {
                for branch in invocation.branches.iter() {
                    let next_statement_idx = match &branch.target {
                        BranchTarget::Fallthrough => {
                            if invocation.branches.len() > 1 {
                                self.segment_starts.insert(idx + 1);
                            }
                            idx + 1
                        }
                        BranchTarget::Statement(target_statement) => {
                            self.segment_starts.insert(target_statement.0);
                            target_statement.0
                        }
                    };
                    if next_statement_idx < self.entry_point {
                        return Err(SegmentationError::JumpOutsideFunction(StatementIdx(idx)));
                    }
                    if next_statement_idx > self.max_jump_in_function {
                        self.max_jump_in_function = next_statement_idx;
                        self.max_jump_in_function_src = idx;
                    }
                }
            }
            Statement::Return(_) => {}
        }
        Ok(())
    }
}
