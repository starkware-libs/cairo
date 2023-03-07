extern type Box<T>;
impl BoxTCopy<T, impl TCopy: Copy::<T>> of Copy::<Box::<T>>;
impl BoxTDrop<T, impl TDrop: Drop::<T>> of Drop::<Box::<T>>;

extern fn into_box<T>(value: T) -> Box<T> nopanic;
extern fn unbox<T>(box: Box<T>) -> T nopanic;
