/// A Box is a type that points to a wrapped value.
///
/// Allows for cheap moving around of the value, as its size is small, and may wrap a large size.
#[derive(Copy, Drop)]
pub extern type Box<T>;

// These functions are only exposed in the corelib through the trait below since calling them
// directly with tuples panics due to auto unpacking of the tuple.
// TODO(Gil): Expose in the core lib when the described behaviour is fixed.
extern fn into_box<T>(value: T) -> Box<T> nopanic;
extern fn unbox<T>(box: Box<T>) -> T nopanic;
extern fn box_forward_snapshot<T>(value: @Box<T>) -> Box<@T> nopanic;

/// Basic trait for the Box type.
#[generate_trait]
pub impl BoxImpl<T> of BoxTrait<T> {
    /// Creates a new Box with the given value.
    ///
    /// Example:
    /// ```
    /// let x = 42;
    /// let boxed_x = BoxTrait::new(x);
    /// ```
    #[inline(always)]
    #[must_use]
    fn new(value: T) -> Box<T> nopanic {
        into_box(value)
    }
    /// Unboxes the given Box.
    ///
    /// Example:
    /// ```
    /// let boxed = BoxTrait::new(42);
    /// assert_eq!(boxed.unbox(), 42);
    /// ```
    #[inline(always)]
    #[must_use]
    fn unbox(self: Box<T>) -> T nopanic {
        unbox(self)
    }
    /// Converts the given snapshot of a Box into a Box of a snapshot.
    ///
    /// Useful for structures that aren't copyable.
    /// Example:
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

impl BoxDeref<T> of core::ops::Deref<Box<T>> {
    type Target = T;
    fn deref(self: Box<T>) -> T {
        self.unbox()
    }
}

impl BoxDebug<T, impl TDebug: core::fmt::Debug<T>> of core::fmt::Debug<Box<T>> {
    fn fmt(self: @Box<T>, ref f: core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        write!(f, "&")?;
        TDebug::fmt(self.as_snapshot().unbox(), ref f)
    }
}
