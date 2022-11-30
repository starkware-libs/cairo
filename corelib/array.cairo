extern type Array<T>;
extern func array_new<T>() -> Array::<T> nopanic;
extern func array_append<T>(ref arr: Array::<T>, value: T) nopanic;
extern func array_at<T>(
    ref arr: Array::<T>,
    index: uint128
) -> Option::<T> implicits(rc: RangeCheck) nopanic;
