pub extern type Pedersen;

pub extern fn pedersen(a: felt252, b: felt252) -> felt252 implicits(Pedersen) nopanic;


/// State for Pedersen hash.
#[derive(Copy, Drop, Debug)]
pub struct HashState {
    pub state: felt252,
}

#[generate_trait]
pub impl PedersenImpl of PedersenTrait {
    /// Creates a state from a base value.
    #[inline]
    fn new(base: felt252) -> HashState {
        HashState { state: base }
    }
}

impl HashStateImpl of crate::hash::HashStateTrait<HashState> {
    #[inline]
    fn update(self: HashState, value: felt252) -> HashState {
        HashState { state: pedersen(self.state, value) }
    }

    #[inline]
    fn finalize(self: HashState) -> felt252 {
        self.state
    }
}

