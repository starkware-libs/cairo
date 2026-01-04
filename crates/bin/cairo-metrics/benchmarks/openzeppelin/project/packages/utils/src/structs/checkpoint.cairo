// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (utils/src/structs/checkpoint.cairo)

use core::num::traits::Sqrt;
use starknet::storage::{
    Mutable, MutableVecTrait, StorageAsPath, StoragePath, StoragePointerReadAccess,
    StoragePointerWriteAccess, Vec, VecTrait,
};
use starknet::storage_access::StorePacking;
use crate::math;

/// `Trace` struct, for checkpointing values as they change at different points in
/// time, and later looking up past values by block timestamp.
#[starknet::storage_node]
pub struct Trace {
    pub checkpoints: Vec<Checkpoint>,
}

/// Generic checkpoint representation.
#[derive(Copy, Drop, Serde)]
pub struct Checkpoint {
    pub key: u64,
    pub value: u256,
}

#[generate_trait]
pub impl TraceImpl of TraceTrait {
    /// Pushes a (`key`, `value`) pair into a Trace so that it is stored as the checkpoint
    /// and returns both the previous and the new value.
    fn push(self: StoragePath<Mutable<Trace>>, key: u64, value: u256) -> (u256, u256) {
        self.checkpoints.as_path()._insert(key, value)
    }

    /// Returns the value in the last (most recent) checkpoint with the key lower than or equal to
    /// the search key, or zero if there is none.
    fn upper_lookup(self: StoragePath<Trace>, key: u64) -> u256 {
        let checkpoints = self.checkpoints.as_path();
        let len = checkpoints.len();
        let pos = checkpoints._upper_binary_lookup(key, 0, len).into();

        if pos == 0 {
            0
        } else {
            checkpoints[pos - 1].read().value
        }
    }

    /// Returns the value in the last (most recent) checkpoint with key lower than or equal to
    /// the search key, or zero if there is none.
    ///
    /// NOTE: This is a variant of `upper_lookup` that is optimised to
    /// find "recent" checkpoints (checkpoints with high keys).
    fn upper_lookup_recent(self: StoragePath<Trace>, key: u64) -> u256 {
        let checkpoints = self.checkpoints.as_path();
        let len = checkpoints.len();

        let mut low = 0;
        let mut high = len;

        if len > 5 {
            let mid = len - len.sqrt().into();
            let mid_point = checkpoints[mid].read();
            if key == mid_point.key {
                return mid_point.value;
            }
            if key < mid_point.key {
                high = mid;
            } else {
                low = mid + 1;
            }
        }
        let pos = checkpoints._upper_binary_lookup(key, low, high);
        if pos == 0 {
            0
        } else {
            checkpoints[pos - 1].read().value
        }
    }

    /// Returns the value in the most recent checkpoint, or zero if there are no checkpoints.
    fn latest(self: StoragePath<Trace>) -> u256 {
        let checkpoints = self.checkpoints;
        let pos = checkpoints.len();

        if pos == 0 {
            0
        } else {
            checkpoints[pos - 1].read().value
        }
    }

    /// Returns whether there is a checkpoint in the structure (i.e. it is not empty),
    /// and if so the key and value in the most recent checkpoint.
    fn latest_checkpoint(self: StoragePath<Trace>) -> (bool, u64, u256) {
        let checkpoints = self.checkpoints;
        let pos = checkpoints.len();

        if pos == 0 {
            (false, 0, 0)
        } else {
            let checkpoint = checkpoints[pos - 1].read();
            (true, checkpoint.key, checkpoint.value)
        }
    }

    /// Returns the total number of checkpoints.
    fn length(self: StoragePath<Trace>) -> u64 {
        self.checkpoints.len()
    }

    /// Returns the checkpoint at the given position.
    fn at(self: StoragePath<Trace>, pos: u64) -> Checkpoint {
        assert(pos < self.length(), 'Vec overflow');
        self.checkpoints[pos].read()
    }
}

#[generate_trait]
impl CheckpointImpl of CheckpointTrait {
    /// Pushes a (`key`, `value`) pair into an ordered list of checkpoints, either by inserting a
    /// new checkpoint, or by updating the last one.
    fn _insert(self: StoragePath<Mutable<Vec<Checkpoint>>>, key: u64, value: u256) -> (u256, u256) {
        let pos = self.len();

        if pos > 0 {
            let mut last = self[pos - 1].read();

            // Update or append new checkpoint
            let prev = last.value;
            if last.key == key {
                last.value = value;
                self[pos - 1].write(last);
            } else {
                // Checkpoint keys must be non-decreasing
                assert(last.key < key, 'Unordered insertion');
                self.push(Checkpoint { key, value });
            }
            (prev, value)
        } else {
            self.push(Checkpoint { key, value });
            (0, value)
        }
    }

    /// Returns the index of the last (most recent) checkpoint with the key lower than or equal to
    /// the search key, or `high` if there is none. `low` and `high` define a section where to do
    /// the search, with inclusive `low` and exclusive `high`.
    fn _upper_binary_lookup(
        self: StoragePath<Vec<Checkpoint>>, key: u64, low: u64, high: u64,
    ) -> u64 {
        let mut _low = low;
        let mut _high = high;

        #[allow(inefficient_while_comp)]
        while _low < _high {
            let mid = math::average(_low, _high);
            if self[mid].read().key > key {
                _high = mid;
            } else {
                _low = mid + 1;
            };
        }
        _high
    }
}

const _2_POW_184: felt252 = 0x10000000000000000000000000000000000000000000000;
const _128_BITS_MASK: u256 = 0xffffffffffffffffffffffffffffffff;

/// Packs a Checkpoint into a (felt252, felt252).
///
/// The packing is done as follows:
///
/// - The first felt of the tuple contains `key` and `value.low`.
/// - `key` is stored at range [4,67] bits (0-indexed), taking the most significant usable bits.
/// - `value.low` is stored at range [124, 251], taking the less significant bits (at the end).
/// - `value.high` is stored as the second tuple element.
///
/// NOTE: In this first felt, the first four bits are skipped to avoid representation errors due
/// to `felt252` max value being a bit less than a 252 bits number max value
/// (https://docs.starknet.io/architecture-and-concepts/cryptography/#stark-field).
impl CheckpointStorePacking of StorePacking<Checkpoint, (felt252, felt252)> {
    fn pack(value: Checkpoint) -> (felt252, felt252) {
        let checkpoint = value;

        // shift-left to reach the corresponding position
        let key = checkpoint.key.into() * _2_POW_184;
        let key_and_low = key + checkpoint.value.low.into();

        (key_and_low, checkpoint.value.high.into())
    }

    fn unpack(value: (felt252, felt252)) -> Checkpoint {
        let (key_and_low, high) = value;
        let key_and_low: u256 = key_and_low.into();

        // shift-right and mask to extract the corresponding values
        let key: u256 = key_and_low / _2_POW_184.into();
        let low = key_and_low & _128_BITS_MASK;

        Checkpoint {
            key: key.try_into().unwrap(),
            value: u256 { low: low.try_into().unwrap(), high: high.try_into().unwrap() },
        }
    }
}
