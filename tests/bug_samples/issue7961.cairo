fn outer<F, impl Fn: core::ops::Fn<F, (u32,)>[Output: u32], +Drop<F>>(f: F) {
    inner(|x| f(x)); // Capturing 'f' in closure causes the panic
}

fn inner<F, impl Fn: core::ops::Fn<F, (u32,)>[Output: u32], +Drop<F>>(f: F) {
    f(42);
}

#[test]
fn trigger_bug() {
    outer(|x| x + 1);
}
