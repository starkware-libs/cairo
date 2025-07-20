#[inline(never)]
pub fn drop_wrapper(t: Wrapper<()>) {
    t.t
}

fn specialized(a: felt252, t: Wrapper<()>) -> felt252 {
    drop_wrapper(t);
    a + 1
}

// The size of generic type `T` is not known.
struct Wrapper<T> {
    t: T,
}

#[test]
fn call_specialized() {
    specialized(8, Wrapper { t: () });
}
