extern type DictFelt252To<T>;
extern type SquashedDictFelt252To<T>;
impl SquashedDictFelt252ToDrop<T, impl TDrop: Drop::<T>> of Drop::<SquashedDictFelt252To::<T>>;

extern fn dict_felt252_to_new<T>() -> DictFelt252To<T> implicits(SegmentArena) nopanic;
extern fn dict_felt252_to_write<T>(ref dict: DictFelt252To<T>, key: felt252, value: T) nopanic;
extern fn dict_felt252_to_read<T>(ref dict: DictFelt252To<T>, key: felt252) -> T nopanic;
extern fn dict_felt252_to_squash<T>(
    dict: DictFelt252To<T>
) -> SquashedDictFelt252To<T> implicits(RangeCheck, GasBuiltin, SegmentArena) nopanic;

trait DictFelt252ToTrait<T> {
    fn new() -> DictFelt252To<T>;
    fn insert(ref self: DictFelt252To<T>, key: felt252, value: T);
    fn get(ref self: DictFelt252To<T>, key: felt252) -> T;
    fn squash(self: DictFelt252To<T>) -> SquashedDictFelt252To<T>;
}
impl DictFelt252ToImpl<T> of DictFelt252ToTrait::<T> {
    fn new() -> DictFelt252To<T> {
        dict_felt252_to_new()
    }
    fn insert(ref self: DictFelt252To<T>, key: felt252, value: T) {
        dict_felt252_to_write(ref self, key, value)
    }
    fn get(ref self: DictFelt252To<T>, key: felt252) -> T {
        dict_felt252_to_read(ref self, key)
    }
    #[inline(never)]
    fn squash(self: DictFelt252To<T>) -> SquashedDictFelt252To<T> {
        dict_felt252_to_squash(self)
    }
}
