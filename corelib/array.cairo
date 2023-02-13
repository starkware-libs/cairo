extern type Array<T>;
extern fn array_new<T>() -> Array::<T> nopanic;
extern fn array_append<T>(ref arr: Array::<T>, value: T) nopanic;
extern fn array_pop_front<T>(ref arr: Array::<T>) -> Option::<T> nopanic;
#[panic_with('Array out of bounds', array_at)]
extern fn array_get<T>(
    ref arr: Array::<T>, index: usize
) -> Option::<T> implicits(RangeCheck) nopanic;
extern fn array_len<T>(ref arr: Array::<T>) -> usize nopanic;

trait ArrayTrait<T> {
    fn new() -> Array::<T>;
    fn append(ref self: Array::<T>, value: T);
    fn pop_front(ref self: Array::<T>) -> Option::<T>;
    fn get(ref self: Array::<T>, index: usize) -> Option::<T>;
    fn at(ref self: Array::<T>, index: usize) -> T;
    fn len(ref self: Array::<T>) -> usize;
    fn is_empty(ref self: Array::<T>) -> bool;
}
impl ArrayImpl<T> of ArrayTrait::<T> {
    #[inline(always)]
    fn new() -> Array::<T> {
        array_new()
    }
    #[inline(always)]
    fn append(ref self: Array::<T>, value: T) {
        array_append(ref self, value)
    }
    #[inline(always)]
    fn pop_front(ref self: Array::<T>) -> Option::<T> {
        array_pop_front(ref self)
    }
    #[inline(always)]
    fn get(ref self: Array::<T>, index: usize) -> Option::<T> {
        array_get(ref self, index)
    }
    fn at(ref self: Array::<T>, index: usize) -> T {
        array_at(ref self, index)
    }
    #[inline(always)]
    fn len(ref self: Array::<T>) -> usize {
        array_len(ref self)
    }
    #[inline(always)]
    fn is_empty(ref self: Array::<T>) -> bool {
        self.len() == 0_usize
    }
}

// Impls for common generic types
impl ArrayFeltDrop of Drop::<Array::<felt>>;
impl ArrayFeltCopy of Copy::<Array::<felt>>;
impl ArrayU8Drop of Drop::<Array::<u8>>;
impl ArrayU8Copy of Copy::<Array::<u8>>;
impl ArrayU32Drop of Drop::<Array::<u32>>;
impl ArrayU32Copy of Copy::<Array::<u32>>;
impl ArrayU64Drop of Drop::<Array::<u64>>;
impl ArrayU64Copy of Copy::<Array::<u64>>;
impl ArrayU128Drop of Drop::<Array::<u128>>;
impl ArrayU128Copy of Copy::<Array::<u128>>;
impl ArrayU256Drop of Drop::<Array::<u256>>;
impl ArrayU256Copy of Copy::<Array::<u256>>;
