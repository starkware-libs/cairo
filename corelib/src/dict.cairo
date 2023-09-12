use traits::{Index, Default};

extern type Felt252Dict<T>;
extern type SquashedFelt252Dict<T>;
extern type Felt252DictEntry<T>;
impl SquashedFelt252DictDrop<T, +Drop<T>> of Drop<SquashedFelt252Dict<T>>;

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
    /// Inserts the given value for the given key.
    ///
    /// Requires the `Destruct` trait, as the previous value is dropped.
    fn insert<+Destruct<T>>(ref self: Felt252Dict<T>, key: felt252, value: T);
    /// Returns a copy of the value at the given key.
    ///
    /// Requires the `Copy` trait.
    fn get<+Copy<T>>(ref self: Felt252Dict<T>, key: felt252) -> T;
    fn squash(self: Felt252Dict<T>) -> SquashedFelt252Dict<T> nopanic;
    fn entry(self: Felt252Dict<T>, key: felt252) -> (Felt252DictEntry<T>, T) nopanic;
}
impl Felt252DictImpl<T, +Felt252DictValue<T>> of Felt252DictTrait<T> {
    #[inline]
    fn insert<+Destruct<T>>(ref self: Felt252Dict<T>, key: felt252, value: T) {
        let (entry, _prev_value) = felt252_dict_entry_get(self, key);
        self = felt252_dict_entry_finalize(entry, value);
    }

    #[inline]
    fn get<+Copy<T>>(ref self: Felt252Dict<T>, key: felt252) -> T {
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

trait Felt252DictEntryTrait<T> {
    fn finalize(self: Felt252DictEntry<T>, new_value: T) -> Felt252Dict<T>;
}

impl Felt252DictEntryImpl<T, +Felt252DictValue<T>> of Felt252DictEntryTrait<T> {
    #[inline(always)]
    fn finalize(self: Felt252DictEntry<T>, new_value: T) -> Felt252Dict<T> {
        felt252_dict_entry_finalize(self, new_value)
    }
}

impl Felt252DictDefault<T> of Default<Felt252Dict<T>> {
    #[inline(always)]
    fn default() -> Felt252Dict<T> {
        felt252_dict_new()
    }
}

impl Felt252DictDestruct<T, +Drop<T>, +Felt252DictValue<T>> of Destruct<Felt252Dict<T>> {
    #[inline(always)]
    fn destruct(self: Felt252Dict<T>) nopanic {
        self.squash();
    }
}

impl Felt252DictEntryDestruct<T, +Drop<T>, +Felt252DictValue<T>> of Destruct<Felt252DictEntry<T>> {
    #[inline(always)]
    fn destruct(self: Felt252DictEntry::<T>) nopanic {
        felt252_dict_entry_finalize(self, Felt252DictValue::zero_default());
    }
}

impl Felt252DictIndex<
    T, +Felt252DictTrait<T>, +Copy<T>, +Destruct<Felt252DictEntry<T>>
> of Index<Felt252Dict<T>, felt252, T> {
    #[inline(always)]
    fn index(ref self: Felt252Dict<T>, index: felt252) -> T {
        self.get(index)
    }
}
