/// The logical value of a variable for Sierra simulation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CoreValue {
    // TODO(orizi): Use actual felt object.
    Felt(i128),
    GasBuiltin(i64),
    Integer(i64),
    NonZero(Box<CoreValue>),
    Ref(Box<CoreValue>),
    Uninitialized,
}
