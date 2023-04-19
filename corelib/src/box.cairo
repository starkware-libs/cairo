extern type Box<T>;
impl BoxTCopy<T, Copy<T>> of Copy<Box<T>>;
impl BoxTDrop<T, Drop<T>> of Drop<Box<T>>;

// These functions are only exposed in the corelib through the trait below since calling them
// directly with tuples panics due to auto unpacking of the tuple.
// TODO(Gil): Expose in the core lib when the described behaviour is fixed.
extern fn into_box<T>(value: T) -> Box<T> nopanic;
extern fn unbox<T>(box: Box<T>) -> T nopanic;

trait BoxTrait<T> {
    fn new(value: T) -> Box<T> nopanic;
    fn unbox(self: Box<T>) -> T nopanic;
}

impl BoxImpl<T> of BoxTrait<T> {
    #[inline(always)]
    fn new(value: T) -> Box<T> nopanic {
        into_box(value)
    }
    #[inline(always)]
    fn unbox(self: Box<T>) -> T nopanic {
        unbox(self)
    }
}
