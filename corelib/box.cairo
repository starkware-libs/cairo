extern type Box<T>;
impl BoxFeltCopy of Copy::<Box::<felt>>;
impl BoxFeltDrop of Drop::<Box::<felt>>;

extern func into_box<T>(value: T) -> Box::<T> nopanic;
extern func unbox<T>(box: Box::<T>) -> T nopanic;
