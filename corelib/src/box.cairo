//! A type that points to a wrapped value, using a dedicated memory segment.
//!
//! `Box<T>` is a smart pointer that allows to store values of arbitrary size
//! while keeping a fixed-size pointer during operations.
//!
//! # Examples
//!
//! Creating a new box:
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

/// A `Box` is a type that points to a wrapped value.
/// It allows for cheap moving around of the value, as its size is small, and may wrap a large size.
#[derive(Copy, Drop)]
pub extern type Box<T>;

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
    /// Takes a `Box<T>`, deferences it and returns a value of type `T`.
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

/// `Debug` trait implementation for the `Box` type.
impl BoxDebug<T, impl TDebug: crate::fmt::Debug<T>> of crate::fmt::Debug<Box<T>> {
    /// Formats a `Box` type, allowing to print `Box` instances for debugging purposes.
    ///
    /// # Examples
    ///
    /// ```
    /// let boxed_value: Box<u32> = BoxTrait::new(1);
    /// println!("{:?}", boxed_value);
    /// ```
    fn fmt(self: @Box<T>, ref f: crate::fmt::Formatter) -> Result<(), crate::fmt::Error> {
        write!(f, "&")?;
        TDebug::fmt(self.as_snapshot().unbox(), ref f)
    }
}
