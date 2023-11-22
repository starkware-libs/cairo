use num_bigint::BigInt;

/// A struct for describing a range of the form [lower_bound,upper_bound)
pub struct Range {
    /// The lower bound (Inclusive).
    pub lower: BigInt,
    /// The upper bound (Exclusive).
    pub upper: BigInt,
}
