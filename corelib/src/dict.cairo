use traits::Index;

extern type Felt252Dict<T>;
extern type SquashedFelt252Dict<T>;
impl SquashedFelt252DictDrop<T, impl TDrop: Drop<T>> of Drop<SquashedFelt252Dict<T>>;

extern fn felt252_dict_new<T>() -> Felt252Dict<T> implicits(SegmentArena) nopanic;
extern fn felt252_dict_write<T>(ref dict: Felt252Dict<T>, key: felt252, value: T) nopanic;
extern fn felt252_dict_read<T>(ref dict: Felt252Dict<T>, key: felt252) -> T nopanic;

/// Squashes the dictionary and returns SquashedFelt252Dict.
///
/// NOTE: Never use this libfunc directly. Use Felt252DictTrait::squash() instead. Using this
/// libfunc directly will result in multiple unnecessary copies of the libfunc in the compiled CASM
/// code.
extern fn felt252_dict_squash<T>(
    dict: Felt252Dict<T>
) -> SquashedFelt252Dict<T> implicits(RangeCheck, GasBuiltin, SegmentArena) nopanic;

trait Felt252DictTrait<T> {
    fn new() -> Felt252Dict<T>;
    fn insert(ref self: Felt252Dict<T>, key: felt252, value: T);
    fn get(ref self: Felt252Dict<T>, key: felt252) -> T;
    fn squash(self: Felt252Dict<T>) -> SquashedFelt252Dict<T> nopanic;
}
impl Felt252DictImpl<T, impl TDefault: Felt252DictValue<T>> of Felt252DictTrait<T> {
    fn new() -> Felt252Dict<T> {
        felt252_dict_new()
    }
    fn insert(ref self: Felt252Dict<T>, key: felt252, value: T) {
        felt252_dict_write(ref self, key, value)
    }
    fn get(ref self: Felt252Dict<T>, key: felt252) -> T {
        felt252_dict_read(ref self, key)
    }
    #[inline(never)]
    fn squash(self: Felt252Dict<T>) -> SquashedFelt252Dict<T> nopanic {
        felt252_dict_squash(self)
    }
}

impl Felt252DictDestruct<T,
impl TDrop: Drop<T>,
impl TDefault: Felt252DictValue<T>> of Destruct<Felt252Dict<T>> {
    #[inline(always)]
    fn destruct(self: Felt252Dict<T>) nopanic {
        self.squash();
    }
}

impl Felt252DictIndex<T> of Index<Felt252Dict<T>, felt252, T> {
    #[inline(always)]
    fn index(ref self: Felt252Dict<T>, index: felt252) -> T {
        felt252_dict_read(ref self, index)
    }
}

