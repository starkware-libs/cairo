use itertools::Itertools;

use crate::lower::flow_control::graph::{
    BindVar, FlowControlGraphBuilder, FlowControlNode, FlowControlVar, NodeId, PatternVarId,
};

/// The pattern-matching function below takes a list of patterns, and depending on the item at
/// question, construct a filtered list of patterns that are relevant to the item.
/// This struct represents the indices of those filtered patterns.
///
/// For example, consider the following match:
/// ```plain
/// match (x, y) {
///     (_, C) => { ... }
///     (B, _) => { ... }
/// }
/// ```
/// When we look at the first item, `x`, we have two patterns: `_` and `B`.
/// If `x=A`, then the filtered list is `[0]` (only the first pattern is accepted).
/// If `x=B`, then the filtered list is `[0, 1]` (both patterns are accepted).
/// For the latter, it is important to return both `0` and `1`, because the arm that will be chosen
/// depends on the value of `y` (which will be handled by the calling pattern matching function).
#[derive(Clone, Default, Hash, Eq, PartialEq)]
pub struct FilteredPatterns {
    /// The indices of the patterns that are accepted by the filter, together with binding
    /// information.
    filter: Vec<IndexAndBindings>,
}

impl FilteredPatterns {
    /// Adds a new pattern to the filter.
    pub fn add(&mut self, idx: usize) {
        self.filter.push(IndexAndBindings { index: idx, bindings: Bindings::default() });
    }

    /// Returns a [FilteredPatterns] that accepts all patterns (no filtering).
    pub fn all(n_patterns: usize) -> Self {
        Self {
            filter: (0..n_patterns)
                .map(|idx| IndexAndBindings { index: idx, bindings: Bindings::default() })
                .collect_vec(),
        }
    }

    /// Assuming `self` is a [FilteredPatterns] that applies to a *subset* of a list of patterns
    /// (defined by `outer_filter`), this function returns the lifted [FilteredPatterns] -
    /// the corresponding [FilteredPatterns] that applies to the *original* list of patterns.
    ///
    /// For example, assume that `foo` gets 3 patterns: `A`, `B`, `C`, and it calls `bar` with the
    /// last two patterns (`B` and `C`, at indices `1` and `2`).
    /// Suppose that `bar` filters this list to only `C`.
    /// `bar` returns the filter `[1]` since it uses its own indexing.
    /// `foo` needs to lift it to `[2]` to return to its caller using `foo`'s indexing.
    pub fn lift(self, outer_filter: &FilteredPatterns) -> Self {
        Self {
            filter: self
                .filter
                .into_iter()
                .map(|index_and_bindings| index_and_bindings.lift(outer_filter))
                .collect_vec(),
        }
    }

    /// Returns the first pattern accepted by the filter.
    pub fn first(self) -> Option<IndexAndBindings> {
        self.filter.into_iter().next()
    }

    /// Adds bindings to the filter. The length of the vector must be equal to the number of
    /// patterns in the original list of patterns.
    ///
    /// For example, if the original list of patterns is `[x, _]`, then the bindings vector should
    /// be `[Bindings::single(..., x), Bindings::default()]`.
    pub fn add_bindings(self, bindings: Vec<Bindings>) -> Self {
        Self {
            filter: self
                .filter
                .into_iter()
                .map(|index_and_bindings| {
                    let index = index_and_bindings.index;
                    index_and_bindings.extend(&bindings[index])
                })
                .collect_vec(),
        }
    }

    /// Returns an iterator over the indices of the patterns accepted by the filter.
    pub fn indices<'a>(&'a self) -> impl Iterator<Item = usize> + 'a {
        self.filter.iter().map(|index_and_bindings| index_and_bindings.index)
    }
}

/// Represents an item in the filter.
#[derive(Clone, Hash, Eq, PartialEq)]
pub struct IndexAndBindings {
    /// The index of the pattern in the list of patterns.
    index: usize,
    /// The bindings that should be applied if the pattern is chosen.
    bindings: Bindings,
}
impl IndexAndBindings {
    /// Returns the index of the pattern in the original list.
    pub fn index(&self) -> usize {
        self.index
    }

    /// Lifts the index of the pattern to the outer level.
    /// See [FilteredPatterns::lift] for more details.
    fn lift(self, outer_filter: &FilteredPatterns) -> IndexAndBindings {
        IndexAndBindings {
            index: outer_filter.filter[self.index].index,
            bindings: self.bindings.union(&outer_filter.filter[self.index].bindings),
        }
    }

    /// Create a node that binds the [FlowControlVar]s in [Self::bindings] to the [PatternVarId]s
    /// and continues to the given `node`.
    pub fn wrap_node<'db>(
        self,
        graph: &mut FlowControlGraphBuilder<'db>,
        mut node: NodeId,
    ) -> NodeId {
        for (input, output) in self.bindings.bindings {
            node = graph.add_node(FlowControlNode::BindVar(BindVar { input, output, next: node }));
        }
        node
    }

    /// Extends `self` with the given [Bindings].
    fn extend(self, bindings: &Bindings) -> Self {
        Self { index: self.index, bindings: self.bindings.union(bindings) }
    }
}

#[derive(Clone, Default, Hash, Eq, PartialEq)]
pub struct Bindings {
    /// The bindings that should be applied if the pattern is chosen.
    bindings: Vec<(FlowControlVar, PatternVarId)>,
}
impl Bindings {
    /// Returns [Bindings] of a single variable.
    pub fn single(input: FlowControlVar, output: PatternVarId) -> Self {
        Self { bindings: vec![(input, output)] }
    }

    /// Returns the union of two [Bindings].
    ///
    /// Assumption: the two [Bindings] are disjoint.
    pub fn union(mut self, bindings: &Self) -> Self {
        self.bindings.extend(bindings.bindings.iter().cloned());
        self
    }
}
