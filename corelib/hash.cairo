extern type Pedersen;

extern fn pedersen(a: felt, b: felt) -> felt implicits(Pedersen) nopanic;
