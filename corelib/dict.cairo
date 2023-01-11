extern type DictManager;
extern type DictFeltTo<T>;
extern type SquashedDictFeltTo<T>;
impl SquashedDictFeltToFeltDrop of Drop::<SquashedDictFeltTo::<felt>>;

extern fn dict_felt_to_new<T>() -> DictFeltTo::<T> implicits(DictManager) nopanic;
extern fn dict_felt_to_write<T>(ref dict: DictFeltTo::<T>, key: felt, value: T) nopanic;
extern fn dict_felt_to_read<T>(ref dict: DictFeltTo::<T>, key: felt) -> T nopanic;
extern fn dict_felt_to_squash<T>(
    dict: DictFeltTo::<T>
) -> SquashedDictFeltTo::<T> implicits(RangeCheck, GasBuiltin, DictManager) nopanic;
