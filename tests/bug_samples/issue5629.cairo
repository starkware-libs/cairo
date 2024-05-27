#[derive(Drop)]
struct NonCopy {
    a: ByteArray,
    b: ByteArray,
}

fn foo(v: NonCopy) -> (NonCopy, ByteArray) {
    let r = NonCopy { a: "4", ..v };
    (r, v.a)
}

#[test]
fn test() {
    foo(NonCopy { a: "a", b: "b" });
}
