use num_bigint::BigInt;

/// A struct for describing a range of the form [lower_bound,upper_bound)
pub struct Range {
    pub lower_bound: BigInt,
    pub upper_bound: BigInt,
}
