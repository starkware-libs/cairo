/// A trait for fallible indexing operations with different index types.
///
/// Unlike [`IndexView`] and [`Index`] which panic on out-of-bounds access, `SliceIndex`
/// returns an `Option`, providing safe indexing operations. This trait enables containers
/// to support multiple index types (e.g., `Range<usize>`, `RangeInclusive<usize>`,
/// or `usize`) through a unified interface.
///
/// [`IndexView`]: crate::ops::IndexView
/// [`Index`]: crate::ops::Index
///
/// # Examples
///
/// The following example shows how `ByteSpan` implements `SliceIndex` for both `Range<usize>`
/// and `RangeInclusive<usize>`, enabling safe slicing operations that return `None` when
/// out of bounds.
///
/// ```
/// use core::byte_array::{ByteSpan, ByteSpanTrait};
///
/// let ba: ByteArray = "hello";
/// let span = ba.span();
///
/// // Using Range<usize>.
/// let slice = span.get(1..4).unwrap();
/// assert_eq!(slice.to_byte_array(), "ell");
///
/// // Using RangeInclusive<usize>.
/// let slice = span.get(1..=3).unwrap();
/// assert_eq!(slice.to_byte_array(), "ell");
///
/// // Out of bounds returns None.
/// assert!(span.get(10..20).is_none());
/// ```
// TODO(giladchase): add examples for `usize` once supported.
#[unstable(feature: "slice-index")]
pub trait SliceIndex<C, I> {
    /// The returned type after indexing.
    type Output;

    /// Returns the output at this index, if in bounds.
    fn get(self: @C, index: I) -> Option<Self::Output>;
}
