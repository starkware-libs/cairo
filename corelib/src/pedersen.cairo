extern type Pedersen;

extern fn pedersen(a: felt252, b: felt252) -> felt252 implicits(Pedersen) nopanic;


/// State for Pedersen hash.
#[derive(Copy, Drop)]
struct HashState {
    state: felt252,
}

#[generate_trait]
impl PedersenImpl of PedersenTrait {
    /// Creates a state from a base value.
    #[inline(always)]
    fn new(base: felt252) -> HashState {
        HashState { state: base }
    }
}

impl HashStateImpl of hash::HashStateTrait<HashState> {
    #[inline(always)]
    fn update(self: HashState, value: felt252) -> HashState {
        HashState { state: pedersen(self.state, value) }
    }

    #[inline(always)]
    fn finalize(self: HashState) -> felt252 {
        self.state
    }
}

