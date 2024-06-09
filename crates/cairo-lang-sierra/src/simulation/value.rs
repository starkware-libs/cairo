use std::collections::HashMap;

use num_bigint::BigInt;
use starknet_types_core::felt::Felt as Felt252;

/// The logical value of a variable for Sierra simulation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CoreValue {
    EcPoint(Felt252, Felt252),
    Felt252(Felt252),
    GasBuiltin(i64),
    RangeCheck,
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
    Uint128(u128),
    NonZero(Box<CoreValue>),
    Ref(Box<CoreValue>),
    Array(Vec<CoreValue>),
    Dict(HashMap<BigInt, CoreValue>),
    Enum {
        value: Box<CoreValue>,
        /// The index of the relevant variant.
        index: usize,
    },
    Struct(Vec<CoreValue>),
    Uninitialized,
}
