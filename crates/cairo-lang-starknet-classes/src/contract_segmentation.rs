#[cfg(test)]
#[path = "contract_segmentation_test.rs"]
mod test;

use cairo_lang_sierra::program::{Program, Statement, StatementIdx};
use cairo_lang_sierra_to_casm::compiler::CairoProgram;
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
    bytecode_len: usize,
) -> Result<NestedIntList, SegmentationError> {
    if bytecode_len == 0 {
        return Ok(NestedIntList::Leaf(0));
    }
    let functions_segment_start_statements = find_functions_segments(program)?;
    let mut segment_start_offsets =
        functions_statement_ids_to_offsets(cairo_program, &functions_segment_start_statements);
    segment_start_offsets.extend(consts_segments_offsets(cairo_program, bytecode_len));

    Ok(NestedIntList::Node(
        get_segment_lengths(&segment_start_offsets, bytecode_len)
            .iter()
            .map(|length| NestedIntList::Leaf(*length))
            .collect(),
    ))
}

/// Returns a vector that contains the starts (as statement indices) of the functions.
fn find_functions_segments(program: &Program) -> Result<Vec<usize>, SegmentationError> {
    // Get the set of function entry points.
    let mut function_statement_ids: Vec<usize> =
        program.funcs.iter().map(|func| func.entry_point.0).collect();
    function_statement_ids.sort();
    if function_statement_ids.first() != Some(&0) {
        return Err(SegmentationError::NoFunctionStartAtZero);
    }

    // Sanity check: go over the statements and check that there are no jump outside of functions.
    let mut current_function = FunctionInfo::new(0);
    let mut next_function_idx = 1;

    // Iterate over all statements and collect the segments' starts.
    for (idx, statement) in program.statements.iter().enumerate() {
        // Check if this is the beginning of a new function.
        if function_statement_ids.get(next_function_idx) == Some(&idx) {
            current_function.finalize(idx)?;
            current_function = FunctionInfo::new(idx);
            next_function_idx += 1;
        }
        current_function.visit_statement(idx, statement)?;
    }

    current_function.finalize(program.statements.len())?;

    Ok(function_statement_ids)
}

/// Converts the result of [find_functions_segments] from statement ids to bytecode offsets.
fn functions_statement_ids_to_offsets(
    cairo_program: &CairoProgram,
    segment_starts_statements: &[usize],
) -> Vec<usize> {
    let statement_to_offset = |statement_id: usize| {
        cairo_program
            .debug_info
            .sierra_statement_info
            .get(statement_id)
            .unwrap_or_else(|| panic!("Missing bytecode offset for statement id {statement_id}."))
            .start_offset
    };
    segment_starts_statements.iter().map(|start| statement_to_offset(*start)).collect()
}

/// Returns a vector that contains the lengths of the segments.
fn get_segment_lengths(segment_starts_offsets: &[usize], bytecode_len: usize) -> Vec<usize> {
    // Compute the lengths of the segments from their start points.
    let mut segment_lengths = vec![];
    for i in 1..segment_starts_offsets.len() {
        let segment_size = segment_starts_offsets[i] - segment_starts_offsets[i - 1];
        if segment_size > 0 {
            segment_lengths.push(segment_size);
        }
    }

    // Handle the last segment.
    let last_offset =
        segment_starts_offsets.last().expect("Segmentation error: No function found.");
    let segment_size = bytecode_len - last_offset;
    if segment_size > 0 {
        segment_lengths.push(segment_size);
    }

    segment_lengths
}

/// Helper struct for [find_functions_segments].
/// Represents a single function and its segments.
struct FunctionInfo {
    entry_point: usize,
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
            max_jump_in_function: entry_point,
            max_jump_in_function_src: entry_point,
        }
    }

    /// Finalizes the current function handling.
    ///
    /// `function_end` is the statement index following the last statement in the function.
    ///
    /// Returns the segment starts for the function.
    fn finalize(self, function_end: usize) -> Result<(), SegmentationError> {
        // Check that we did not see a jump after the function's end.
        if self.max_jump_in_function >= function_end {
            return Err(SegmentationError::JumpOutsideFunction(StatementIdx(
                self.max_jump_in_function_src,
            )));
        }
        Ok(())
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
                    let next_statement_idx = StatementIdx(idx).next(&branch.target).0;
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

/// Returns the offsets of the const segments.
fn consts_segments_offsets(cairo_program: &CairoProgram, bytecode_len: usize) -> Vec<usize> {
    let const_segments_start_offset = bytecode_len - cairo_program.consts_info.total_segments_size;
    cairo_program
        .consts_info
        .segments
        .values()
        .map(|segment| const_segments_start_offset + segment.segment_offset)
        .collect()
}
