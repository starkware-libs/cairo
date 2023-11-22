use num_bigint::BigInt;

/// A closed range.
pub struct Range {
    /// The lower bound (Inclusive).
    pub lower: BigInt,
    /// The upper bound (Inclusive).
    pub upper: BigInt,
}
