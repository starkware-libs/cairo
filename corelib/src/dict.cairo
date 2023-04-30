use traits::Index;
use traits::Default;

extern type Felt252Dict<T>;
extern type SquashedFelt252Dict<T>;
extern type Felt252DictEntry<T>;
impl SquashedFelt252DictDrop<T, impl TDrop: Drop<T>> of Drop<SquashedFelt252Dict<T>>;

extern fn felt252_dict_new<T>() -> Felt252Dict<T> implicits(SegmentArena) nopanic;

extern fn felt252_dict_entry_get<T>(
    dict: Felt252Dict<T>, key: felt252
) -> (Felt252DictEntry<T>, T) nopanic;

extern fn felt252_dict_entry_finalize<T>(
    dict_entry: Felt252DictEntry<T>, new_value: T
) -> Felt252Dict<T> nopanic;

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
    fn insert<impl TDestruct: Destruct<T>>(ref self: Felt252Dict<T>, key: felt252, value: T);
    fn get<impl TCopy: Copy<T>>(ref self: Felt252Dict<T>, key: felt252) -> T;
    fn squash(self: Felt252Dict<T>) -> SquashedFelt252Dict<T> nopanic;
    fn entry(self: Felt252Dict<T>, key: felt252) -> (Felt252DictEntry<T>, T) nopanic;
}
impl Felt252DictImpl<T, impl TDefault: Felt252DictValue<T>> of Felt252DictTrait<T> {
    fn new() -> Felt252Dict<T> {
        felt252_dict_new()
    }
    #[inline]
    fn insert<impl TDestruct: Destruct<T>>(ref self: Felt252Dict<T>, key: felt252, value: T) {
        let (entry, prev_value) = felt252_dict_entry_get(self, key);
        self = felt252_dict_entry_finalize(entry, value);
    }
    /// Notice that this function implicitly requires T to be destructable.
    #[inline]
    fn get<impl TCopy: Copy<T>>(ref self: Felt252Dict<T>, key: felt252) -> T {
        let (entry, prev_value) = felt252_dict_entry_get(self, key);
        let return_value = prev_value;
        self = felt252_dict_entry_finalize(entry, prev_value);
        return_value
    }
    #[inline(never)]
    fn squash(self: Felt252Dict<T>) -> SquashedFelt252Dict<T> nopanic {
        felt252_dict_squash(self)
    }
    #[inline(always)]
    fn entry(self: Felt252Dict<T>, key: felt252) -> (Felt252DictEntry<T>, T) nopanic {
        felt252_dict_entry_get(self, key)
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

impl Felt252DictEntryDestruct<T,
impl TDrop: Drop<T>,
impl TDefault: Felt252DictValue<T>> of Destruct<Felt252DictEntry<T>> {
    #[inline(always)]
    fn destruct(self: Felt252DictEntry::<T>) nopanic {
        felt252_dict_entry_finalize(self, TDefault::zero_default());
    }
}

impl Felt252DictIndex<T,
impl TDictImpl: Felt252DictTrait<T>,
impl TCopy: Copy<T>,
impl EntryDestruct: Destruct<Felt252DictEntry<T>>> of Index<Felt252Dict<T>, felt252, T> {
    #[inline(always)]
    fn index(ref self: Felt252Dict<T>, index: felt252) -> T {
        self.get(index)
    }
}
