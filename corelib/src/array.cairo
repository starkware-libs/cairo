extern type Array<T>;
extern fn array_new<T>() -> Array::<T> nopanic;
extern fn array_append<T>(ref arr: Array::<T>, value: T) nopanic;
extern fn array_pop_front<T>(ref arr: Array::<T>) -> Option::<T> nopanic;
#[panic_with('Index out of bounds', array_at)]
extern fn array_get<T>(
    arr: @Array::<T>, index: usize
) -> Option::<@T> implicits(RangeCheck) nopanic;
extern fn array_len<T>(arr: @Array::<T>) -> usize nopanic;

trait ArrayTrait<T> {
    fn new() -> Array::<T>;
    fn append(ref self: Array::<T>, value: T);
    fn pop_front(ref self: Array::<T>) -> Option::<T>;
    fn get(self: @Array::<T>, index: usize) -> Option::<@T>;
    fn at(self: @Array::<T>, index: usize) -> @T;
    fn len(self: @Array::<T>) -> usize;
    fn is_empty(self: @Array::<T>) -> bool;
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
    fn get(self: @Array::<T>, index: usize) -> Option::<@T> {
        array_get(self, index)
    }
    fn at(self: @Array::<T>, index: usize) -> @T {
        array_at(self, index)
    }
    #[inline(always)]
    fn len(self: @Array::<T>) -> usize {
        array_len(self)
    }
    #[inline(always)]
    fn is_empty(self: @Array::<T>) -> bool {
        self.len() == 0_usize
    }
}

// Impls for common generic types
impl ArrayFeltDrop of Drop::<Array::<felt>>;
impl ArrayU8Drop of Drop::<Array::<u8>>;
impl ArrayU32Drop of Drop::<Array::<u32>>;
impl ArrayU64Drop of Drop::<Array::<u64>>;
impl ArrayU128Drop of Drop::<Array::<u128>>;
impl ArrayU256Drop of Drop::<Array::<u256>>;
