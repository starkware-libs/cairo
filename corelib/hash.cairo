extern type Pedersen;

extern func pedersen(a: felt, b: felt) -> felt implicits (pedersen: Pedersen) nopanic;
