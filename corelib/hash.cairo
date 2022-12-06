extern type Pedersen;
extern type PedersenBuiltinCost;
impl PedersenBuiltinCostCopy of Copy::<PedersenBuiltinCost>;
impl PedersenBuiltinCostDrop of Drop::<PedersenBuiltinCost>;

extern func pedersen(a: felt, b: felt) -> felt implicits(Pedersen) nopanic;
extern func pedersen_get_gas() -> Option::<()> implicits(RangeCheck, GasBuiltin) nopanic;
