/// Minimum of the two values.
/// # Arguments
/// * `a` - first comparable value
/// * `b` - Second comparable value
/// # Returns
/// * `result` - The smallest of the two values
#[must_use]
pub fn min<T, +PartialOrd<T>, +Drop<T>, +Copy<T>>(a: T, b: T) -> T {
    if a > b {
        b
    } else {
        a
    }
}

/// Maximum of the two values.
/// # Arguments
/// * `a` - first comparable value
/// * `b` - Second comparable value
/// # Returns
/// * `result` - The greatest of the two values
#[must_use]
pub fn max<T, +PartialOrd<T>, +Drop<T>, +Copy<T>>(a: T, b: T) -> T {
    if a > b {
        a
    } else {
        b
    }
}

/// Minimum and maximum of the two values.
/// # Arguments
/// * `a` - first comparable value
/// * `b` - Second comparable value
/// # Returns
/// * `result` - The two values sorted in ascending order
#[must_use]
pub fn minmax<T, +PartialOrd<T>, +Drop<T>, +Copy<T>>(a: T, b: T) -> (T, T) {
    if a > b {
        (b, a)
    } else {
        (a, b)
    }
}
