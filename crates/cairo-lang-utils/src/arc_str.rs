use std::fmt::Debug;
use std::ops::{Deref, RangeBounds};
use std::sync::Arc;

use smol_str::SmolStr;

/// A string slice that holds a `Arc<str>`, with start and end indices defining a slice of the
/// string, allowing cheap sub-slicing and duplication.
#[derive(Clone)]
pub struct ArcStrSlice {
    source: Arc<str>,
    start: usize,
    end: usize,
}

impl ArcStrSlice {
    /// Creates a new `ArcStrSlice` from an `Arc<str>` and a range that specifies which part of the
    /// `Arc<str>` to reference.
    ///
    /// # Panics
    /// Panics if the range is out of bounds or if the specified indices are not on character
    /// boundaries.
    pub fn new<R: RangeBounds<usize>>(source: Arc<str>, range: R) -> Self {
        let len = source.len();
        let start = match range.start_bound() {
            std::ops::Bound::Included(&n) => n,
            std::ops::Bound::Excluded(&n) => n + 1,
            std::ops::Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            std::ops::Bound::Included(&n) => n + 1,
            std::ops::Bound::Excluded(&n) => n,
            std::ops::Bound::Unbounded => len,
        };
        // This assertion checks the validity of the range.
        assert!(start <= end && end <= len, "Invalid range bounds");
        // This ensures the boundaries split valid UTF-8 code point boundaries.
        assert!(source.is_char_boundary(start), "Start index is not on a char boundary");
        assert!(source.is_char_boundary(end), "End index is not on a char boundary");

        Self { source, start, end }
    }

    /// Creates a new `ArcStrSlice` that references the entire given `Arc<str>`.
    pub fn full(source: Arc<str>) -> Self {
        let len = source.len();
        Self { source, start: 0, end: len }
    }

    /// Returns the length of this slice.
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Returns `true` if the slice has a length of 0, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Returns the string slice.
    pub fn as_str(&self) -> &str {
        &self.source[self.start..self.end]
    }

    /// Returns a new `ArcStrSlice` based on a sub-range of the current slice.
    ///
    /// # Panics
    /// Panics if the sub-range is out of bounds or if the specified indices are not on character
    /// boundaries.
    pub fn slice<R: RangeBounds<usize>>(&self, range: R) -> ArcStrSlice {
        let current_len = self.len();
        let start = match range.start_bound() {
            std::ops::Bound::Included(&n) => n,
            std::ops::Bound::Excluded(&n) => n + 1,
            std::ops::Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            std::ops::Bound::Included(&n) => n + 1,
            std::ops::Bound::Excluded(&n) => n,
            std::ops::Bound::Unbounded => current_len,
        };

        assert!(start <= end && end <= current_len, "Invalid range bounds");

        let absolute_start = self.start + start;
        let absolute_end = self.start + end;

        assert!(
            self.source.is_char_boundary(absolute_start),
            "Start index is not on a char boundary"
        );
        assert!(self.source.is_char_boundary(absolute_end), "End index is not on a char boundary");

        Self { source: self.source.clone(), start: absolute_start, end: absolute_end }
    }
}

impl Debug for ArcStrSlice {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.as_str().fmt(f)
    }
}

impl Deref for ArcStrSlice {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl AsRef<str> for ArcStrSlice {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl std::fmt::Display for ArcStrSlice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl PartialEq for ArcStrSlice {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<str> for ArcStrSlice {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<&str> for ArcStrSlice {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<String> for ArcStrSlice {
    fn eq(&self, other: &String) -> bool {
        self.as_str() == other
    }
}

impl Eq for ArcStrSlice {}

impl std::hash::Hash for ArcStrSlice {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl PartialOrd for ArcStrSlice {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ArcStrSlice {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl From<ArcStrSlice> for SmolStr {
    fn from(value: ArcStrSlice) -> Self {
        SmolStr::new(value)
    }
}

impl From<ArcStrSlice> for String {
    fn from(value: ArcStrSlice) -> Self {
        String::from(value.as_str())
    }
}

impl Extend<ArcStrSlice> for Vec<String> {
    fn extend<T: IntoIterator<Item = ArcStrSlice>>(&mut self, iter: T) {
        self.extend(iter.into_iter().map(|arc_str| arc_str.to_string()));
    }
}

impl Extend<ArcStrSlice> for String {
    fn extend<T: IntoIterator<Item = ArcStrSlice>>(&mut self, iter: T) {
        for arc_str in iter.into_iter() {
            self.push_str(&arc_str);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_usage() {
        let source = Arc::from("Hello, world!");
        let slice = ArcStrSlice::new(source, 7..12);
        assert_eq!(slice.as_str(), "world");
        assert_eq!(slice.len(), 5);
    }

    #[test]
    fn test_full_slice() {
        let source = Arc::from("Hello, world!");
        let slice = ArcStrSlice::full(source);
        assert_eq!(slice.as_str(), "Hello, world!");
        assert_eq!(slice.len(), 13);
    }

    #[test]
    fn test_slice_of_slice() {
        let source = Arc::from("Hello, world!");
        let slice1 = ArcStrSlice::new(source, 7..);
        let slice2 = slice1.slice(..5);
        assert_eq!(slice2.as_str(), "world");
    }

    #[test]
    fn test_deref() {
        let source = Arc::from("Hello, world!");
        let slice = ArcStrSlice::new(source, 7..12);
        assert_eq!(slice.len(), 5); // Uses Deref to call str::len()
        assert!(slice.starts_with("wo")); // Uses Deref to call str::starts_with()
    }

    #[test]
    fn test_equality() {
        let source = Arc::from("Hello, world!");
        let slice = ArcStrSlice::new(source, 7..12);
        assert_eq!(slice, "world");
        assert_eq!(slice, String::from("world"));
    }
}
