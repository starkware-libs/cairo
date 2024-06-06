#[derive(Drop, Copy)]
struct S1 {
    a: usize,
    b: felt252
}

#[derive(Drop, Copy)]
struct S2 {
    inner: S1,
    a: usize,
}

#[derive(Drop, Copy)]
struct S3 {
    inner: S2
}


impl S2Deref of core::ops::deref::Deref<S2> {
    type Target = S1;
    fn deref(self: S2) -> S1 {
        self.inner
    }
}

impl S3Deref of core::ops::deref::Deref<S3> {
    type Target = S2;
    fn deref(self: S3) -> S2 {
        self.inner
    }
}

#[test]
fn test_simple_deref() {
    let s1 = S1 { a: 1, b: 2 };
    let s2 = S2 { inner: s1, a: 3 };
    let s3 = S3 { inner: s2 };
    assert_eq!(s1.a, 1);
    assert_eq!(s2.a, 3);
    assert_eq!(s3.a, 3);
    assert_eq!(s3.inner.a, 3);
    assert_eq!(s3.inner.inner.a, 1);
    assert_eq!(s3.b, 2);
    assert_eq!(s3.inner.b, 2);
}
