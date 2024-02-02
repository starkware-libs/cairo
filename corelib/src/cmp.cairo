#[must_use]
pub fn min<T, +PartialOrd<T>, +Drop<T>, +Copy<T>>(a: T, b: T) -> T {
    if a > b {
        return b;
    }
    a
}

#[must_use]
pub fn max<T, +PartialOrd<T>, +Drop<T>, +Copy<T>>(a: T, b: T) -> T {
    if a > b {
        return a;
    }
    b
}

#[must_use]
pub fn min_max<T, +PartialOrd<T>, +Drop<T>, +Copy<T>>(a: T, b: T) -> (T, T) {
    if a > b {
        return (b, a);
    }
    (a, b)
}
