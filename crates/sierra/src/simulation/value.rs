/// The logical value of a variable for Sierra simulation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CoreValue {
    // TODO(orizi): Use actual felt object.
    Felt(i128),
    GasBuiltin(i64),
    Uint128(u128),
    NonZero(Box<CoreValue>),
    Ref(Box<CoreValue>),
    Array(Vec<CoreValue>),
    Enum {
        value: Box<CoreValue>,
        /// The index of the relevant variant.
        index: usize,
    },
    Uninitialized,
}
