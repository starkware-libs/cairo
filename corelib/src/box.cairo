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
