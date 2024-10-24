use std::collections::HashMap;

use starknet_types_core::felt::Felt as Felt252;

/// The logical value of a variable for Sierra simulation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CoreValue {
    EcPoint(Felt252, Felt252),
    Felt252(Felt252),
    GasBuiltin(i64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
    Uint128(u128),
    Array(Vec<CoreValue>),
    Dict(HashMap<Felt252, CoreValue>),
    Enum {
        value: Box<CoreValue>,
        /// The index of the relevant variant.
        index: usize,
    },
    Struct(Vec<CoreValue>),
    // The untracked types - do not carry any value.
    Uninitialized,
    RangeCheck,
    Bitwise,
    U128MulGuarantee,
}
