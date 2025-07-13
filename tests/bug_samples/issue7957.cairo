fn outer<F, impl Fn: core::ops::Fn<F, (u32,)>[Output: u32], +Drop<F>>(f: F) -> u32 {
    let inner = |x| {
        let nested = |y| {
            f(y)
        };
        nested(x)
    };
    inner(42)
}
