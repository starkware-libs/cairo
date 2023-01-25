extern type Array<T>;
extern fn array_new<T>() -> Array::<T> nopanic;
extern fn array_append<T>(ref arr: Array::<T>, value: T) nopanic;
extern fn array_pop_front<T>(ref arr: Array::<T>) -> Option::<T> nopanic;
#[panic_with('Array out of bounds', array_at)]
extern fn array_get<T>(
    ref arr: Array::<T>, index: u128
) -> Option::<T> implicits(RangeCheck) nopanic;
extern fn array_len<T>(ref arr: Array::<T>) -> u128 nopanic;
