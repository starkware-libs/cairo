fn min<T, impl PartialOrd<T>, impl Drop<T>, impl Copy<T>>(a: T, b: T) -> T {
    if a > b {
        return b;
    }
    a
}

fn max<T, impl PartialOrd<T>, impl Drop<T>, impl Copy<T>>(a: T, b: T) -> T {
    if a > b {
        return a;
    }
    b
}
