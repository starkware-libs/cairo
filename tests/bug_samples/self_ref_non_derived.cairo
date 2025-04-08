#[test]
fn foo() -> E {
    let e = E::A;
    bar(@e);
    e
}

#[inline(never)]
fn bar(e: @E) nopanic {
    match e {
        E::R(_) => {},
        E::A => {},
    }
}

enum E {
    R: Box<@E>,
    A,
}
