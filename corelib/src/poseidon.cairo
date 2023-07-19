use array::Span;
use array::SpanTrait;
use option::OptionTrait;
use hash::HashStateTrait;

extern type Poseidon;

extern fn hades_permutation(
    s0: felt252, s1: felt252, s2: felt252
) -> (felt252, felt252, felt252) implicits(Poseidon) nopanic;

/// Computes the Poseidon hash on the given input.
///
/// Applies the sponge construction to digest many elements.
/// To distinguish between use cases, the capacity element is initialized to 0.
/// To distinguish between different input sizes always pads with 1, and possibly with another 0 to
/// complete to an even-sized input.
fn poseidon_hash_span(mut span: Span<felt252>) -> felt252 {
    _poseidon_hash_span_inner(get_builtin_costs(), hash::PoseidonHashStateTrait::new(), ref span)
}

/// Helper function for poseidon_hash_span.
fn _poseidon_hash_span_inner(
    builtin_costs: gas::BuiltinCosts, state: hash::PoseidonHashState, ref span: Span<felt252>
) -> felt252 {
    gas::withdraw_gas_all(builtin_costs).expect('Out of gas');
    match span.pop_front() {
        Option::Some(x) => {
            _poseidon_hash_span_inner(builtin_costs, state.update(*x), ref span)
        },
        Option::None => {
            state.finalize()
        },
    }
}
