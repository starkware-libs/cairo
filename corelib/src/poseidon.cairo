use array::Span;
use array::SpanTrait;
use option::OptionTrait;

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


/// Computes the Poseidon hash on the given input.
///
/// Applies the sponge construction to digest many elements.
/// To distinguish between use cases, the capacity element is initialized to 0.
/// To distinguish between different input sizes always pads with 1, and possibly with another 0 to
/// complete to an even-sized input.
fn poseidon_hash_span(mut span: Span<felt252>) -> felt252 {
    let builtin_costs = get_builtin_costs();
    _poseidon_hash_span_inner(builtin_costs, PoseidonBuiltinState { s0: 0, s1: 0, s2: 0 }, ref span)
}

/// Helper function for poseidon_hash_span.
fn _poseidon_hash_span_inner(
    builtin_costs: gas::BuiltinCosts, state: PoseidonBuiltinState, ref span: Span<felt252>
) -> felt252 {
    let x = match span.pop_front() {
        Option::Some(x) => x,
        Option::None(()) => {
            // Pad input with [1, 0].
            let (s0, s1, s2) = hades_permutation(state.s0 + 1, state.s1, state.s2);
            return s0;
        },
    };
    let y = match span.pop_front() {
        Option::Some(y) => y,
        Option::None(()) => {
            // Add x and pad with [0].
            let (s0, s1, s2) = hades_permutation(state.s0 + *x, state.s1 + 1, state.s2);
            return s0;
        },
    };

    let (s0, s1, s2) = hades_permutation(state.s0 + *x, state.s1 + *y, state.s2);
    gas::withdraw_gas_all(builtin_costs).expect('Out of gas');
    _poseidon_hash_span_inner(builtin_costs, PoseidonBuiltinState { s0, s1, s2 }, ref span)
}
