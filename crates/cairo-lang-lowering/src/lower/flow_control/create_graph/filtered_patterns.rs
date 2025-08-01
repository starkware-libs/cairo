use itertools::Itertools;

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

    /// Returns a [FilteredPatterns] that accepts all patterns (no filtering).
    pub fn all(n_patterns: usize) -> Self {
        Self { filter: (0..n_patterns).collect_vec() }
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
            filter: self.filter.into_iter().map(|index| outer_filter.filter[index]).collect_vec(),
        }
    }

    /// Returns the first pattern accepted by the filter.
    pub fn first(self) -> Option<usize> {
        self.filter.into_iter().next()
    }
}
