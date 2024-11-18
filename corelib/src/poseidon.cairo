//! Poseidon hash module that provides cryptographic hash functions based on the Poseidon
//! permutation.
//!
//! The Poseidon hash function is an arithmetic-friendly hash function optimized for use in
//! zero-knowledge proof systems. This module implements the Poseidon hash using a sponge
//! construction for arbitrary-length inputs.
//!
//! # Examples
//!
//! ```
//! use core::hash::HashStateTrait;
//! use core::poseidon::PoseidonTrait;
//!
//! // Create a new hash state
//! let mut state = PoseidonTrait::new();
//!
//! // Update with values
//! state = state.update(1);
//! state = state.update(2);
//!
//! // Finalize to get the hash
//! let hash = state.finalize();
//! ```

use crate::array::Span;
use crate::array::SpanTrait;
use crate::option::OptionTrait;
use crate::hash::HashStateTrait;

pub extern type Poseidon;

pub extern fn hades_permutation(
    s0: felt252, s1: felt252, s2: felt252
) -> (felt252, felt252, felt252) implicits(Poseidon) nopanic;

/// State for Poseidon hash.
#[derive(Copy, Drop, Debug)]
pub struct HashState {
    pub s0: felt252,
    pub s1: felt252,
    pub s2: felt252,
    pub odd: bool,
}

/// `PoseidonTrait` used to create a new Poseidon state.
#[generate_trait]
pub impl PoseidonImpl of PoseidonTrait {
    /// Creates an initial state with all fields set to 0.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::poseidon::PoseidonTrait;
    ///
    /// let mut state = PoseidonTrait::new();
    /// ```
    #[inline]
    fn new() -> HashState {
        HashState { s0: 0, s1: 0, s2: 0, odd: false }
    }
}

impl HashStateDefault of Default<HashState> {
    /// Returns the default zero-initialized state.
    fn default() -> HashState {
        PoseidonTrait::new()
    }
}

/// `HashStateTrait` allowing to update the Poseidon state and finalize it to return the hash.
impl HashStateImpl of HashStateTrait<HashState> {
    /// Takes the current state and a `felt252` value used to update
    /// the state using the Hades permutation.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::hash::HashStateTrait;
    /// use core::poseidon::PoseidonTrait;
    ///
    /// let mut state = PoseidonTrait::new();
    /// state = state.update(1);
    /// ```
    #[inline]
    fn update(self: HashState, value: felt252) -> HashState {
        if self.odd {
            let (s0, s1, s2) = hades_permutation(self.s0, self.s1 + value, self.s2);
            HashState { s0, s1, s2, odd: false }
        } else {
            HashState { s0: self.s0 + value, s1: self.s1, s2: self.s2, odd: true }
        }
    }

    /// Takes the current state, applies the Hades permutation and returns the hash result.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::hash::HashStateTrait;
    /// use core::poseidon::PoseidonTrait;
    ///
    /// let mut state = PoseidonTrait::new();
    /// state = state.update(1);
    /// let hash = state.finalize()
    /// ```
    #[inline]
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

/// Computes the Poseidon hash on the given span input.
/// Applies the sponge construction to digest many elements.
/// To distinguish between use cases, the capacity element is initialized to 0.
/// To distinguish between different input sizes always pads with 1, and possibly with another 0 to
/// complete to an even-sized input.
///
/// # Examples
///
/// ```
/// let mut state = PoseidonTrait::new();
/// state = state.update(1);
/// state = state.update(2);
/// let hash1 = state.finalize();
///
/// let span = array![1, 2].span();
/// let hash2 = poseidon_hash_span(span);
///
/// assert!(hash1 == hash2);
/// ```
pub fn poseidon_hash_span(mut span: Span<felt252>) -> felt252 {
    _poseidon_hash_span_inner(crate::gas::get_builtin_costs(), (0, 0, 0), ref span)
}

fn _poseidon_hash_span_inner(
    builtin_costs: crate::gas::BuiltinCosts,
    state: (felt252, felt252, felt252),
    ref span: Span<felt252>
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
    crate::gas::withdraw_gas_all(builtin_costs).expect('Out of gas');
    _poseidon_hash_span_inner(builtin_costs, next_state, ref span)
}
