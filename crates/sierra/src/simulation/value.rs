/// The logical value of a variable for Sierra simulation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value {
    Int(i64),
    // TODO(orizi): Use actual felt object.
    Felt(i128),
    Transient,
}
