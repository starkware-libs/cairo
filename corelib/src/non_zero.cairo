extern type NonZero<T>;
impl NonZeroTCopy<T, impl TCopy: Copy::<T>> of Copy::<NonZero::<T>>;
impl NonZeroTDrop<T, impl TDrop: Drop::<T>> of Drop::<NonZero::<T>>;
enum IsZeroResult<T> {
    Zero: (),
    NonZero: NonZero<T>,
}
extern fn unwrap_non_zero<T>(a: NonZero<T>) -> T nopanic;

trait NonZeroTrait<T> {
    fn unwrap_nz(self: NonZero<T>) -> T nopanic;
}
impl NonZeroImpl<T> of NonZeroTrait::<T> {
    fn unwrap_nz(self: NonZero<T>) -> T nopanic {
        unwrap_non_zero(self)
    }
}

impl IsZeroResultIntoBool<T, impl TDrop: Drop::<T>> of Into::<IsZeroResult<T>, bool> {
    fn into(self: IsZeroResult<T>) -> bool {
        match self {
            IsZeroResult::Zero(()) => true,
            IsZeroResult::NonZero(_) => false,
        }
    }
}
