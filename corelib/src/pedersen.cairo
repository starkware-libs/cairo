//! Pedersen hash related traits implementations.
//!
//! This module provides an implementation of the Pedersen hash function, which is a
//! collision-resistant cryptographic hash function.
//!
//! The `HashState` struct represents the state of a Pedersen hash computation. It contains a
//! single `felt252` field `state` that holds the current hash value.
//!
//! The `PedersenTrait` provides a `new` method to create a new `HashState` from a base value.
//!
//! The [`HashStateTrait`] defined in the Hash module provides the `update` and `finalize` methods
//! to update the hash state and obtain the final hash value, respectively.
//!
//!
//! # Examples
//!
//! ```
//! use core::hash::HashStateTrait;
//! use core::pedersen::PedersenTrait;
//!
//! let mut state = PedersenTrait::new(0);
//! state = state.update(1);
//! state = state.update(2);
//! let hash = state.finalize();
//! assert!(hash == 0x07546be9ecb576c12cd00962356afd90b615d8ef50605bc13badfd1fd218c0d5);
//! ```

pub extern type Pedersen;

pub extern fn pedersen(a: felt252, b: felt252) -> felt252 implicits(Pedersen) nopanic;

/// Represents the current state of a Pedersen hash computation.
///
/// The state is maintained as a single `felt252` value, which is updated
/// through the [`HashStateTrait::finalize`] method.
#[derive(Copy, Drop, Debug)]
pub struct HashState {
    /// The current hash state
    pub state: felt252,
}

/// A trait for creating a new Pedersen hash state.
#[generate_trait]
pub impl PedersenImpl of PedersenTrait {
    /// Creates a new Pedersen hash state with the given base value.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::pedersen::PedersenTrait;
    ///
    /// let mut state = PedersenTrait::new(0);
    /// assert!(state.state == 0);
    /// ```
    #[inline]
    fn new(base: felt252) -> HashState {
        HashState { state: base }
    }
}

impl HashStateImpl of crate::hash::HashStateTrait<HashState> {
    /// Updates the hash state with a new value.
    ///
    /// Applies the Pedersen commitment function to the current state and new value.
    ///
    ///
    /// # Examples
    ///
    /// ```
    /// use core::hash::HashStateTrait;
    /// use core::pedersen::PedersenTrait;
    ///
    /// let mut state = PedersenTrait::new(0);
    /// state = state.update(1);
    /// ```
    #[inline]
    fn update(self: HashState, value: felt252) -> HashState {
        HashState { state: pedersen(self.state, value) }
    }

    /// Finalizes the hash computation.
    ///
    /// For Pedersen, this simply returns the current state as the final hash value.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::hash::HashStateTrait;
    /// use core::pedersen::PedersenTrait;
    ///
    /// let mut state = PedersenTrait::new(0);
    /// state = state.update(1);
    /// let hash = state.finalize();
    /// ```
    #[inline]
    fn finalize(self: HashState) -> felt252 {
        self.state
    }
}

