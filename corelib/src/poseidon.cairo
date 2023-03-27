use array::Span;
use array::SpanTrait;

extern type Poseidon;

extern fn hades_permutation(
    s0: felt252, s1: felt252, s2: felt252
) -> (felt252, felt252, felt252) implicits(Poseidon) nopanic;


// Represents a Poseidon state.
#[derive(Copy, Drop)]
struct PoseidonBuiltinState {
    s0: felt252,
    s1: felt252,
    s2: felt252,
}


// Apply the sponge construction to digest many elements.
// To distinguish between the use cases the capacity element is initialized to 0.
// To distinguish between different input sizes always pad with 1 and possibly with another 0 to
// complete to an even sized input.
fn poseidon_hash_span(ref span: Span<felt252>) -> felt252 {
    _poseidon_hash_span_inner(PoseidonBuiltinState { s0: 0, s1: 0, s2: 0 }, ref span).s0
}

/// Helper function for poseidon_hash_span.
fn _poseidon_hash_span_inner(
    state: PoseidonBuiltinState, ref span: Span<felt252>
) -> PoseidonBuiltinState {
    let x = match span.pop_front() {
        Option::Some(x) => x,
        Option::None(()) => {
            // Pad input with [1, 0].
            let (s0, s1, s2) = hades_permutation(state.s0 + 1, state.s1, state.s2);
            return PoseidonBuiltinState { s0, s1, s2 };
        },
    };
    let y = match span.pop_front() {
        Option::Some(y) => y,
        Option::None(()) => {
            // Pad input with [1, 0].
            let (s0, s1, s2) = hades_permutation(state.s0 + *x, state.s1 + 1, state.s2);
            return PoseidonBuiltinState { s0, s1, s2 };
        },
    };

    let (s0, s1, s2) = hades_permutation(state.s0 + *x, state.s1 + *y, state.s2);
    _poseidon_hash_span_inner(PoseidonBuiltinState { s0, s1, s2 }, ref span)
}
