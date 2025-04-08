//! `Box<T>` is a smart pointer that allows for:
//!
//! * Storing values of arbitrary size while maintaining a fixed-size pointer
//! * Enabling recursive types that would otherwise have infinite size
//! * Moving large data structures efficiently by passing pointers instead of copying values
//!
//! # Examples
//!
//! Creating a new box with [`BoxTrait::new`]:
//!
//! ```
//! let boxed = BoxTrait::new(42);
//! let unboxed = boxed.unbox();
//! ```
//!
//! Working with larger structures:
//!
//! ```
//! let large_array = array![1, 2, 3, 4, 5];
//! let boxed_array = BoxTrait::new(large_array);
//! ```
//!
//! Creating a recursive data structure:
//!
//!```
//! #[derive(Copy, Drop, Debug)]
//! enum BinaryTree {
//!     Leaf: u32,
//!     Node: (u32, Box<BinaryTree>, Box<BinaryTree>)
//! }
//!
//! let leaf = BinaryTree::Leaf(1);
//! let node = BinaryTree::Node((2, BoxTrait::new(leaf), BoxTrait::new(leaf)));
//! println!("{:?}", node);
//!```
//!
//! NOTE: A `Box<T>` is a smart pointer type that provides a way to store a value of type `T` in
//! Cairo VM's _boxed_ segment, leaving only a pointer in the execution segment.

/// A `Box` is a type that points to a wrapped value.
/// It allows for cheap moving around of the value, as its size is small, and may wrap a large size.
pub extern type Box<T>;

impl BoxCopy<T, +Copy<T>> of Copy<Box<T>>;
impl BoxDrop<T, +Drop<T>> of Drop<Box<T>>;


// These functions are only exposed in the corelib through the trait below since calling them
// directly with tuples panics due to auto unpacking of the tuple.
// TODO(Gil): Expose in the core lib when the described behaviour is fixed.
extern fn into_box<T>(value: T) -> Box<T> nopanic;
extern fn unbox<T>(box: Box<T>) -> T nopanic;
extern fn box_forward_snapshot<T>(value: @Box<T>) -> Box<@T> nopanic;

/// Basic trait for the `Box` type.
#[generate_trait]
pub impl BoxImpl<T> of BoxTrait<T> {
    /// Creates a new `Box` with the given value.
    ///
    /// Allocates space in the boxed segment for the provided value
    /// and returns a `Box<T>` that points to it.
    /// # Examples
    ///
    /// ```
    /// let x = 42;
    /// let boxed_x = BoxTrait::new(x);
    /// ```
    #[inline]
    #[must_use]
    fn new(value: T) -> Box<T> nopanic {
        into_box(value)
    }

    /// Unboxes the given `Box` and returns the wrapped value.
    ///
    /// # Examples
    ///
    /// ```
    /// let boxed = BoxTrait::new(42);
    /// assert!(boxed.unbox() == 42);
    /// ```
    #[inline]
    #[must_use]
    fn unbox(self: Box<T>) -> T nopanic {
        unbox(self)
    }

    /// Converts the given snapshot of a `Box` into a `Box` of a snapshot.
    /// Useful for structures that aren't copyable.
    ///
    /// # Examples
    ///
    /// ```
    /// let snap_boxed_arr = @BoxTraits::new(array![1, 2, 3]);
    /// let boxed_snap_arr = snap_boxed_arr.as_snapshot();
    /// let snap_arr = boxed_snap_arr.unbox();
    /// ```
    #[must_use]
    fn as_snapshot(self: @Box<T>) -> Box<@T> nopanic {
        box_forward_snapshot(self)
    }
}

impl BoxDeref<T> of crate::ops::Deref<Box<T>> {
    /// The target type after dereferencing.
    type Target = T;
    /// Takes a `Box<T>`, dereferences it and returns a value of type `T`.
    ///
    /// # Examples
    ///
    /// ```
    /// let boxed_value: Box<u32> = BoxTrait::new(1);
    /// let value: u32 = boxed_value.deref();
    /// assert!(value == 1);
    /// ```
    fn deref(self: Box<T>) -> T {
        self.unbox()
    }
}

impl BoxDebug<T, impl TDebug: crate::fmt::Debug<T>> of crate::fmt::Debug<Box<T>> {
    /// Formats a `Box` type, allowing to print `Box` instances for debugging purposes.
    /// Boxed values are prefixed with `&` when formatted.
    ///
    /// # Examples
    ///
    /// ```
    /// let boxed_value: Box<u32> = BoxTrait::new(1);
    /// println!("{:?}", boxed_value); // Result will be `&1`
    /// ```
    fn fmt(self: @Box<T>, ref f: crate::fmt::Formatter) -> Result<(), crate::fmt::Error> {
        write!(f, "&")?;
        TDebug::fmt(self.as_snapshot().unbox(), ref f)
    }
}
