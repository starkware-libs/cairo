//! Utilities for running deeply recursive code without overflowing the stack.

/// The remaining stack size below which a new stack segment is allocated.
const RED_ZONE: usize = 256 * 1024;
/// The size of a newly allocated stack segment.
const STACK_SEGMENT_SIZE: usize = 4 * 1024 * 1024;

/// Runs `f`, on a newly allocated stack segment if the remaining stack is below a red zone.
///
/// Call at the entry of recursive functions whose depth depends on user input, so deeply nested
/// inputs do not overflow the stack.
pub fn with_sufficient_stack<R>(f: impl FnOnce() -> R) -> R {
    stacker::maybe_grow(RED_ZONE, STACK_SEGMENT_SIZE, f)
}
