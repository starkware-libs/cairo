// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (introspection/src/interface.cairo)

pub const ISRC5_ID: felt252 = 0x3f918d17e5ee77373b56385708f855659a07f75997f365cf87748628532a055;

#[starknet::interface]
pub trait ISRC5<TState> {
    fn supports_interface(self: @TState, interface_id: felt252) -> bool;
}
