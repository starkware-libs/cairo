extern type Pedersen;

extern func pedersen(a: felt, b: felt) -> felt implicits(Pedersen) nopanic;
extern func pedersen_get_gas() -> Option::<()> implicits(RangeCheck, GasBuiltin) nopanic;
