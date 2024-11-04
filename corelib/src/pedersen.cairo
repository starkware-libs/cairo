//! Pedersen module that provides an implementation of the Pedersen hash function, which is a
//! collision-resistant cryptographic hash function.
//!
//! The `HashState` struct represents the state of a Pedersen hash computation. It contains a
//! single `felt252` field `state` that holds the current hash value.
//!
//! The `PedersenTrait` provides a `new` method to create a new `HashState` from a base value.
//!
//! The `HashStateTrait` defined in the Hash module provides the `update` and `finalize` methods to
//! update the hash state and obtain the final hash value, respectively.
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
//! ```

pub extern type Pedersen;

pub extern fn pedersen(a: felt252, b: felt252) -> felt252 implicits(Pedersen) nopanic;

/// State for Pedersen hash.
#[derive(Copy, Drop, Debug)]
pub struct HashState {
    // Current Pedersen state is representend as a `felt252` value.
    pub state: felt252,
}

/// `PedersenTrait` implementation.
#[generate_trait]
pub impl PedersenImpl of PedersenTrait {
    /// Returns a Pedersen state from a base `felt252` value.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut state = PedersenTrait::new(0);
    /// assert!(state.state == 0);
    /// ```
    #[inline]
    fn new(base: felt252) -> HashState {
        HashState { state: base }
    }
}

impl HashStateImpl of crate::hash::HashStateTrait<HashState> {
    /// Returns a new Pedersen state given a current state and a `felt252` value.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut state = PedersenTrait::new(0);
    /// state = state.update(1);
    /// ```
    #[inline]
    fn update(self: HashState, value: felt252) -> HashState {
        HashState { state: pedersen(self.state, value) }
    }

    /// Returns the final hash result corresponding to the current Pedersen state.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut state = PedersenTrait::new(0);
    /// state = state.update(1);
    /// let hash = state.finalize();
    /// ```
    #[inline]
    fn finalize(self: HashState) -> felt252 {
        self.state
    }
}

