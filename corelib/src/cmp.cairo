pub fn min<T, +PartialOrd<T>, +Drop<T>, +Copy<T>>(a: T, b: T) -> T {
    if a > b {
        return b;
    }
    a
}

pub fn max<T, +PartialOrd<T>, +Drop<T>, +Copy<T>>(a: T, b: T) -> T {
    if a > b {
        return a;
    }
    b
}
