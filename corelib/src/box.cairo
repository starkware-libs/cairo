#[derive(Copy, Drop)]
pub extern type Box<T>;

// These functions are only exposed in the corelib through the trait below since calling them
// directly with tuples panics due to auto unpacking of the tuple.
// TODO(Gil): Expose in the core lib when the described behaviour is fixed.
extern fn into_box<T>(value: T) -> Box<T> nopanic;
extern fn unbox<T>(box: Box<T>) -> T nopanic;
extern fn box_forward_snapshot<T>(value: @Box<T>) -> Box<@T> nopanic;

#[generate_trait]
pub impl BoxImpl<T> of BoxTrait<T> {
    #[inline(always)]
    fn new(value: T) -> Box<T> nopanic {
        into_box(value)
    }
    #[inline(always)]
    fn unbox(self: Box<T>) -> T nopanic {
        unbox(self)
    }
    fn as_snapshot(self: @Box<T>) -> Box<@T> nopanic {
        box_forward_snapshot(self)
    }
}

impl BoxDebug<T, impl TDebug: core::fmt::Debug<T>> of core::fmt::Debug<Box<T>> {
    fn fmt(self: @Box<T>, ref f: core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        write!(f, "BoxTrait::new(")?;
        TDebug::fmt(self.as_snapshot().unbox(), ref f)?;
        write!(f, ")")
    }
}
