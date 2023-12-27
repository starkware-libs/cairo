#[cfg(test)]
#[path = "contract_segmentation_test.rs"]
mod test;

use cairo_lang_sierra::program::{Program, StatementIdx};
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
            .map(|length| NestedIntList::Leaf(*length))
            .collect(),
    ))
}

/// Returns a vector that contains the starts (as statement indices) of the functions.
fn find_segments(program: &Program) -> Result<Vec<usize>, SegmentationError> {
    // Get the set of function entry points.
    let mut function_statement_ids: Vec<usize> =
        program.funcs.iter().map(|func| func.entry_point.0).collect();
    function_statement_ids.sort();
    if function_statement_ids.first() != Some(&0) {
        return Err(SegmentationError::NoFunctionStartAtZero);
    }
    Ok(function_statement_ids)
}

/// Converts the result of [find_segments] from statement ids to bytecode offsets.
fn statement_ids_to_offsets(
    cairo_program: &CairoProgram,
    segment_starts_statements: &[usize],
) -> Vec<usize> {
    let statement_to_offset = |statement_id: usize| {
        cairo_program
            .debug_info
            .sierra_statement_info
            .get(statement_id)
            .unwrap_or_else(|| panic!("Missing bytecode offset for statement id {statement_id}."))
            .code_offset
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
        segment_starts_offsets.last().expect("Segmentation error: Function has no segments."); // TODO: fix error.
    let segment_size = bytecode_len - last_offset;
    if segment_size > 0 {
        segment_lengths.push(segment_size);
    }

    segment_lengths
}
