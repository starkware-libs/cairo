fn outer<FO, impl Fn: core::ops::Fn<FO, (u32,)>[Output: u32], +Drop<FO>>(f: FO) {
    inner(|x| f(x)); // Capturing 'f' in closure causes the panic
}

fn inner<FI, impl Fn: core::ops::Fn<FI, (u32,)>[Output: u32], +Drop<FI>>(f: FI) {
    f(42);
}

#[test]
fn trigger_bug() {
    outer(|x| x + 1);
}
