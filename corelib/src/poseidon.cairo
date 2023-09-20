use array::Span;
use array::SpanTrait;
use option::OptionTrait;
use hash::HashStateTrait;

extern type Poseidon;

extern fn hades_permutation(
    s0: felt252, s1: felt252, s2: felt252
) -> (felt252, felt252, felt252) implicits(Poseidon) nopanic;

/// State for Poseidon hash.
#[derive(Copy, Drop)]
struct HashState {
    s0: felt252,
    s1: felt252,
    s2: felt252,
    odd: bool,
}

#[generate_trait]
impl PoseidonImpl of PoseidonTrait {
    /// Creates an initial state.
    #[inline(always)]
    fn new() -> HashState {
        HashState { s0: 0, s1: 0, s2: 0, odd: false }
    }
}

impl HashStateDefault of Default<HashState> {
    fn default() -> HashState {
        PoseidonTrait::new()
    }
}

impl HashStateImpl of HashStateTrait<HashState> {
    #[inline(always)]
    fn update(self: HashState, value: felt252) -> HashState {
        if self.odd {
            let (s0, s1, s2) = hades_permutation(self.s0, self.s1 + value, self.s2);
            HashState { s0, s1, s2, odd: false }
        } else {
            HashState { s0: self.s0 + value, s1: self.s1, s2: self.s2, odd: true }
        }
    }

    #[inline(always)]
    fn finalize(self: HashState) -> felt252 {
        if self.odd {
            let (r, _, _) = hades_permutation(self.s0, self.s1 + 1, self.s2);
            r
        } else {
            let (r, _, _) = hades_permutation(self.s0 + 1, self.s1, self.s2);
            r
        }
    }
}

/// Computes the Poseidon hash on the given input.
///
/// Applies the sponge construction to digest many elements.
/// To distinguish between use cases, the capacity element is initialized to 0.
/// To distinguish between different input sizes always pads with 1, and possibly with another 0 to
/// complete to an even-sized input.
fn poseidon_hash_span(mut span: Span<felt252>) -> felt252 {
    _poseidon_hash_span_inner(get_builtin_costs(), (0, 0, 0), ref span)
}

/// Helper function for poseidon_hash_span.
fn _poseidon_hash_span_inner(
    builtin_costs: gas::BuiltinCosts, state: (felt252, felt252, felt252), ref span: Span<felt252>
) -> felt252 {
    let (s0, s1, s2) = state;
    let x = *match span.pop_front() {
        Option::Some(x) => x,
        Option::None => { return HashState { s0, s1, s2, odd: false }.finalize(); },
    };
    let y = *match span.pop_front() {
        Option::Some(y) => y,
        Option::None => { return HashState { s0: s0 + x, s1, s2, odd: true }.finalize(); },
    };
    let next_state = hades_permutation(s0 + x, s1 + y, s2);
    gas::withdraw_gas_all(builtin_costs).expect('Out of gas');
    _poseidon_hash_span_inner(builtin_costs, next_state, ref span)
}
