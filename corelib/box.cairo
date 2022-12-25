extern type Box<T>;
impl BoxFeltCopy of Copy::<Box::<felt>>;
impl BoxFeltDrop of Drop::<Box::<felt>>;

extern fn into_box<T>(value: T) -> Box::<T> nopanic;
extern fn unbox<T>(box: Box::<T>) -> T nopanic;
