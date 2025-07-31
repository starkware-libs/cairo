/// The pattern-matching function below take a list of patterns, and depending on the item at
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
#[derive(Clone, Hash, Eq, PartialEq)]
pub struct FilteredPatterns {
    /// The indices of the patterns that are accepted by the filter.
    filter: Vec<usize>,
}

impl FilteredPatterns {
    pub fn empty() -> Self {
        Self { filter: vec![] }
    }

    pub fn add(&mut self, idx: usize) {
        self.filter.push(idx);
    }

    /// Returns the first pattern accepted by the filter.
    pub fn first(self) -> Option<usize> {
        self.filter.into_iter().next()
    }
}
