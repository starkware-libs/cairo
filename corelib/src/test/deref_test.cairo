#[derive(Drop, Copy)]
struct Inner {
    a: usize,
    b: felt252
}

#[derive(Drop, Copy)]
struct Outer {
    inner: Inner,
}

impl OuterDeref of core::ops::deref::Deref<Outer> {
    type Target = Inner;
    fn deref(self: Outer) -> Inner {
        self.inner
    }
}

#[test]
fn test_simple_deref() {
    let inner = Inner { a: 1, b: 2 };
    let outer = Outer { inner: inner };
    let _a = outer.a;
}
