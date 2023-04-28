fn min<T, impl TPartialOrd: PartialOrd<T>, impl DropT: Drop<T>, impl CopyT: Copy<T>>(
    a: T, b: T
) -> T {
    if a > b {
        return b;
    }
    a
}

fn max<T, impl TPartialOrd: PartialOrd<T>, impl DropT: Drop<T>, impl CopyT: Copy<T>>(
    a: T, b: T
) -> T {
    if a > b {
        return a;
    }
    b
}
