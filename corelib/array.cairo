extern type Array<T>;
extern func array_new<T>() -> Array::<T> nopanic;
extern func array_append<T>(ref arr: Array::<T>, value: T) nopanic;
extern func array_at<T>(
    ref arr: Array::<T>, index: u128
) -> Option::<T> implicits(RangeCheck) nopanic;
extern func array_len<T>(ref arr: Array::<T>) -> u128 nopanic;
