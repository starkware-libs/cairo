#[cfg(test)]
#[path = "contract_segmentation_test.rs"]
mod test;

use cairo_lang_sierra::program::{BranchTarget, Program, Statement, StatementIdx};
use cairo_lang_sierra_to_casm::compiler::CairoProgram;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// NestedIntList is either a list of NestedIntList or an integer.
/// E.g., `[0, [1, 2], [3, [4]]]`.
///
/// Used to represents the lengths of the segments in a contract, which are in a form of a tree.
///
/// For example, the contract may be segmented by functions, where each function is segmented by
/// its branches. It is also possible to have the inner segmentation only for some of the functions,
/// while others are kept as non-segmented leaves in the tree.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum NestedIntList {
    Leaf(usize),
    Node(Vec<NestedIntList>),
}

#[derive(Error, Debug, Eq, PartialEq)]
pub enum SegmentationError {
    #[error("Expected a function start at index 0.")]
    NoFunctionStartAtZero,
    #[error("Jump outside of function boundaries.")]
    JumpOutsideFunction(StatementIdx),
}

/// Computes the bytecode_segment_length for the given contract.
pub fn compute_bytecode_segment_lengths(
    program: &Program,
    cairo_program: &CairoProgram,
    bytecode_size: usize,
) -> Result<NestedIntList, SegmentationError> {
    if bytecode_size == 0 {
        return Ok(NestedIntList::Leaf(0));
    }
    let segment_start_statements = find_segments(program)?;
    let segment_start_offsets = statement_ids_to_offsets(cairo_program, &segment_start_statements);
    Ok(NestedIntList::Node(
        get_segment_lengths(&segment_start_offsets, bytecode_size)
            .iter()
            .map(|segment| {
                NestedIntList::Node(
                    segment.iter().map(|length| NestedIntList::Leaf(*length)).collect(),
                )
            })
            .collect(),
    ))
}

/// Returns a vector of vectors, where each inner vector represents a function in the program,
/// and contains the starts (as statement indices) of the segments in the function.
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

/// Converts the result of [find_segments] from statement ids to bytecode offsets.
fn statement_ids_to_offsets(
    cairo_program: &CairoProgram,
    segment_starts_statements: &[Vec<usize>],
) -> Vec<Vec<usize>> {
    let statement_to_offset = |statement_id: usize| {
        cairo_program
            .debug_info
            .sierra_statement_info
            .get(statement_id)
            .unwrap_or_else(|| panic!("Missing bytecode offset for statement id {statement_id}."))
            .code_offset
    };
    segment_starts_statements
        .iter()
        .map(|segment_starts| {
            segment_starts.iter().map(|start| statement_to_offset(*start)).collect()
        })
        .collect()
}

/// Returns a vector of vectors, where each inner vector represents a function in the program,
/// and contains the lengths of the segments in the function.
fn get_segment_lengths(
    segment_starts_offsets: &[Vec<usize>],
    bytecode_len: usize,
) -> Vec<Vec<usize>> {
    // Compute the lengths of the segments from their start points.
    let mut segment_lengths = vec![];
    for i in 0..segment_starts_offsets.len() {
        let mut function_segment_lengths = vec![];
        for j in 1..segment_starts_offsets[i].len() {
            let segment_size = segment_starts_offsets[i][j] - segment_starts_offsets[i][j - 1];
            if segment_size > 0 {
                function_segment_lengths.push(segment_size);
            }
        }

        // Handle the last segment.
        let last_offset = segment_starts_offsets[i]
            .last()
            .expect("Segmentation error: Function has no segments.");
        let next_offset = if i < segment_starts_offsets.len() - 1 {
            segment_starts_offsets[i + 1][0]
        } else {
            bytecode_len
        };
        let segment_size = next_offset - last_offset;
        if segment_size > 0 {
            function_segment_lengths.push(segment_size);
        }

        segment_lengths.push(function_segment_lengths);
    }
    segment_lengths
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
