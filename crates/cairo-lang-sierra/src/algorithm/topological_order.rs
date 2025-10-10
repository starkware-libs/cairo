use crate::program::StatementIdx;

/// The status of a node during the topological ordering finding algorithm.
#[derive(Clone, Debug)]
enum TopologicalOrderStatus {
    /// The computation for that statement did not start.
    NotStarted,
    /// The computation is in progress.
    InProgress,
    /// The computation was completed, and all the children were visited.
    Done,
}

/// Returns the reverse topological ordering.
///
/// `detect_cycles` - if true, the function will return an error if a cycle is detected, else will
///   not detect the cycle, and ordering within cycles won't be topological.
/// `roots` - the roots of the graph.
/// `node_count` - the number of nodes in the graph.
/// `get_children` - a function that returns the children of a node.
/// `out_of_bounds_err` - a function that returns an error for a node is out of bounds.
/// `cycle_err` - a function that returns an error for a node that is part of a cycle.
/// Note: Will only work properly if the nodes are in the range [0, node_count).
pub fn reverse_topological_ordering<
    E,
    Children: IntoIterator<Item = StatementIdx>,
    GetChildren: Fn(StatementIdx) -> Result<Children, E>,
>(
    detect_cycles: bool,
    roots: impl Iterator<Item = StatementIdx>,
    node_count: usize,
    get_children: GetChildren,
    cycle_err: impl Fn(StatementIdx) -> E,
) -> Result<Vec<StatementIdx>, E> {
    let mut ordering = vec![];
    let mut status = vec![TopologicalOrderStatus::NotStarted; node_count];
    for root in roots {
        calculate_reverse_topological_ordering(
            detect_cycles,
            &mut ordering,
            &mut status,
            root,
            &get_children,
            &cycle_err,
        )?;
    }
    Ok(ordering)
}

/// Calculates the reverse topological ordering starting from `root`. For more info see
/// `reverse_topological_ordering`.
fn calculate_reverse_topological_ordering<
    E,
    Children: IntoIterator<Item = StatementIdx>,
    GetChildren: Fn(StatementIdx) -> Result<Children, E>,
>(
    detect_cycles: bool,
    ordering: &mut Vec<StatementIdx>,
    status: &mut [TopologicalOrderStatus],
    root: StatementIdx,
    get_children: &GetChildren,
    cycle_err: &impl Fn(StatementIdx) -> E,
) -> Result<(), E> {
    // A stack of statements to visit.
    let mut stack = vec![root];

    while let Some(idx) = stack.pop() {
        match status[idx.0] {
            TopologicalOrderStatus::NotStarted => {
                // Mark the statement as `InProgress`.
                status[idx.0] = TopologicalOrderStatus::InProgress;

                // Push the statement back to the stack, so that after visiting all
                // of its children, we would add it to the ordering.
                // Add the missing children on top of it.
                stack.push(idx);
                for child in get_children(idx)? {
                    match status[child.0] {
                        TopologicalOrderStatus::InProgress if detect_cycles => {
                            return Err(cycle_err(child));
                        }
                        TopologicalOrderStatus::NotStarted => stack.push(child),
                        TopologicalOrderStatus::Done | TopologicalOrderStatus::InProgress => {}
                    }
                }
            }
            TopologicalOrderStatus::InProgress => {
                // Mark the statement as `Done`.
                status[idx.0] = TopologicalOrderStatus::Done;

                // Add the element to the ordering after visiting all its children.
                // This gives us reverse topological ordering.
                ordering.push(idx);
            }
            TopologicalOrderStatus::Done => {}
        }
    }

    Ok(())
}
