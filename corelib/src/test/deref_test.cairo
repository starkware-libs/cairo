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

#[derive(Drop, Copy)]
enum E1 {
    V1: S1,
    V2: S1,
}

impl E1Deref of core::ops::deref::Deref<E1> {
    type Target = S1;
    fn deref(self: E1) -> S1 {
        match self {
            E1::V1(s) => s,
            E1::V2(s) => s,
        }
    }
}

#[test]
fn test_simple_enum_deref() {
    let s1 = S1 { a: 1, b: 2 };
    let e1 = E1::V1(s1);
    assert_eq!(e1.a, 1);
    assert_eq!(e1.b, 2);
}

#[derive(Drop, Copy)]
enum E3 {
    V1: S3,
    V2: S3,
}

impl E3Deref of core::ops::deref::Deref<E3> {
    type Target = S3;
    fn deref(self: E3) -> S3 {
        match self {
            E3::V1(s) => s,
            E3::V2(s) => s,
        }
    }
}

#[test]
fn test_nested_enum_deref() {
    let s1 = S1 { a: 1, b: 2 };
    let s2 = S2 { inner: s1, a: 3 };
    let s3 = S3 { inner: s2 };
    let e3 = E3::V1(s3);
    assert_eq!(e3.a, 3);
    assert_eq!(e3.inner.a, 3);
    assert_eq!(e3.inner.inner.a, 1);
    assert_eq!(e3.b, 2);
    assert_eq!(e3.inner.b, 2);
}

#[derive(Drop, Copy)]
struct S4 {
    e3: E3,
    a: usize,
}

impl S4Deref of core::ops::deref::Deref<S4> {
    type Target = E3;
    fn deref(self: S4) -> E3 {
        self.e3
    }
}

#[test]
fn test_struct_enum_deref() {
    let s1 = S1 { a: 1, b: 2 };
    let s2 = S2 { inner: s1, a: 3 };
    let s3 = S3 { inner: s2 };
    let e3 = E3::V1(s3);
    let s4 = S4 { e3: e3, a: 4 };
    assert_eq!(s4.a, 4);
    assert_eq!(s4.e3.a, 3);
    assert_eq!(s4.e3.inner.a, 3);
    assert_eq!(s4.e3.inner.inner.a, 1);
    assert_eq!(s4.e3.b, 2);
    assert_eq!(s4.e3.inner.b, 2);
}

struct ArithOps {
    add: usize,
    sub: usize,
    mul: usize,
    div: usize,
}

impl UsizeTupleDeref of core::ops::deref::Deref<(usize, usize)> {
    type Target = ArithOps;
    fn deref(self: (usize, usize)) -> ArithOps {
        let (x, y) = self;
        ArithOps { add: x + y, sub: x - y, mul: x * y, div: x / y, }
    }
}

impl UsizeFixedSizeArrayDeref of core::ops::deref::Deref<[usize; 2]> {
    type Target = ArithOps;
    fn deref(self: [usize; 2]) -> ArithOps {
        let [x, y] = self;
        ArithOps { add: x + y, sub: x - y, mul: x * y, div: x / y, }
    }
}

#[test]
fn test_tuple_deref() {
    let a = 6;
    let b = 3;
    let usize_tuple = (a, b);
    assert_eq!(usize_tuple.add, 9);
    assert_eq!(usize_tuple.sub, 3);
    assert_eq!(usize_tuple.mul, 18);
    assert_eq!(usize_tuple.div, 2);
}

#[test]
fn test_fixed_size_array_deref() {
    let a = 6;
    let b = 3;
    let usize_array = [a, b];
    assert_eq!(usize_array.add, 9);
    assert_eq!(usize_array.sub, 3);
    assert_eq!(usize_array.mul, 18);
    assert_eq!(usize_array.div, 2);
}
