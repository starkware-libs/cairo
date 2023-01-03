extern type DictManager;
extern type DictFeltTo<T>;
#[derive(Drop)]
extern type SquashedDictFeltTo<T>;

extern fn dict_felt_to_new<T>() -> DictFeltTo::<T> implicits(DictManager) nopanic;
extern fn dict_felt_to_write<T>(ref dict: DictFeltTo::<T>, key: felt, value: T) nopanic;
extern fn dict_felt_to_read<T>(ref dict: DictFeltTo::<T>, key: felt) -> T nopanic;
extern fn dict_felt_to_squash<T>(
    dict: DictFeltTo::<T>
) -> SquashedDictFeltTo::<T> implicits(DictManager) nopanic;
