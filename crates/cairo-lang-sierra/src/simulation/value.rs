use std::collections::HashMap;

use num_bigint::BigInt;

/// The logical value of a variable for Sierra simulation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CoreValue {
    EcPoint(BigInt, BigInt),
    // TODO(orizi): Use actual felt object.
    Felt(BigInt),
    GasBuiltin(i64),
    RangeCheck,
    Uint8(u8),
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
