#[test]
fn test_const_to_const_shadowing() {
    const X: u8 = 1;
    assert_eq!(X, 1);
    {
        const X: u8 = 4;
        assert_eq!(X, 4);
    }
    assert_eq!(X, 1);
}

#[test]
fn test_let_to_const_shadowing() {
    let X = 2;
    assert_eq!(X, 2);
    {
        const X: u8 = 5;
        assert_eq!(X, 5);
    }
    assert_eq!(X, 2);
}

#[test]
fn test_const_to_let_shadowing() {
    const X: u8 = 3;
    assert_eq!(X, 3);
    {
        let X = 6;
        assert_eq!(X, 6);
    }
    assert_eq!(X, 3);
}

const A: u8 = 1;
#[test]
fn test_global_const_to_const_shadowing() {
    assert_eq!(A, 1);
    const A: u8 = 4;
    assert_eq!(A, 4);
}

#[test]
fn test_global_const_to_let_shadowing() {
    assert_eq!(A, 1);
    let A = 4;
    assert_eq!(A, 4);
}

pub mod X {
    pub const A: u8 = 1;
}

#[test]
fn test_use_usage() {
    use X::A;
    assert_eq!(A, 1);
}

#[test]
fn test_use_usage_with_alias() {
    use X::A as B;
    assert_eq!(B, 1);
}

#[test]
fn test_use_constant_shadowing() {
    use X::A;
    assert_eq!(A, 1);
    {
        const A: u8 = 4;
        assert_eq!(A, 4);
    }
    assert_eq!(A, 1);
}

pub mod Y {
    pub const A: u8 = 4;
    pub const B: u8 = 6;
}

#[test]
fn test_use_use_shadowing() {
    use X::A;
    assert_eq!(A, 1);
    {
        use Y::A;
        assert_eq!(A, 4);
    }
    assert_eq!(A, 1);
}

#[test]
fn test_const_use_shadowing() {
    const A: u8 = 1;
    assert_eq!(A, 1);
    {
        use Y::A;
        assert_eq!(A, 4);
    }
    assert_eq!(A, 1);
}

#[test]
fn test_use_let_shadowing() {
    use X::A;
    assert_eq!(A, 1);
    {
        let A = 4;
        assert_eq!(A, 4);
    }
    assert_eq!(A, 1);
}

#[test]
fn test_let_use_shadowing() {
    let A = 1;
    assert_eq!(A, 1);
    {
        use Y::A;
        assert_eq!(A, 4);
    }
    assert_eq!(A, 1);
}

#[test]
fn test_multiple_use() {
    use Y::{A, B};
    assert_eq!(A, 4);
    assert_eq!(B, 6);
}

#[test]
fn test_type_usage() {
    let mut arr = ArrayTrait::<u8>::new();
    type R = u8;
    let a: R = 4;
    arr.append(a);
    assert_eq!(arr.len(), 1);
    assert_eq!(arr[0], @4);
}

#[test]
fn test_type_shadowing() {
    let mut arr_u8 = ArrayTrait::<u8>::new();
    let mut arr_u16 = ArrayTrait::<u16>::new();
    type R = u8;
    let mut a: R = 4;
    arr_u8.append(a);
    {
        type R = u16;
        a = 3;
        arr_u8.append(a);
        let a: R = 8;
        arr_u16.append(a);
    }
    a = 2;
    arr_u8.append(a);
    assert_eq!(arr_u8.len(), 3);
    assert_eq!(arr_u8[0], @4);
    assert_eq!(arr_u8[1], @3);
    assert_eq!(arr_u8[2], @2);
    assert_eq!(arr_u16.len(), 1);
    assert_eq!(arr_u16[0], @8);
}

pub mod B {
    pub type R = u8;
}

#[test]
fn test_use_type_usage() {
    let mut arr = ArrayTrait::<u8>::new();
    use B::R;
    let a: R = 4;
    arr.append(a);
    assert_eq!(arr.len(), 1);
    assert_eq!(arr[0], @4);
}

#[test]
fn test_use_type_usage_with_alias() {
    let mut arr = ArrayTrait::<u8>::new();
    use B::R as B;
    let a: B = 4;
    arr.append(a);
    assert_eq!(arr.len(), 1);
    assert_eq!(arr[0], @4);
}

#[test]
fn test_use_type_shadowing() {
    let mut arr_u8 = ArrayTrait::<u8>::new();
    let mut arr_u16 = ArrayTrait::<u16>::new();
    use B::R;
    let mut a: R = 4;
    arr_u8.append(a);
    {
        type R = u16;
        a = 3;
        arr_u8.append(a);
        let a: R = 8;
        arr_u16.append(a);
    }
    a = 2;
    arr_u8.append(a);
    assert_eq!(arr_u8.len(), 3);
    assert_eq!(arr_u8[0], @4);
    assert_eq!(arr_u8[1], @3);
    assert_eq!(arr_u8[2], @2);
    assert_eq!(arr_u16.len(), 1);
    assert_eq!(arr_u16[0], @8);
}

pub mod C {
    pub type R = u16;
    pub type Q = u8;
}

#[test]
fn test_double_use_type_shadowing() {
    let mut arr_u8 = ArrayTrait::<u8>::new();
    let mut arr_u16 = ArrayTrait::<u16>::new();
    use B::R;
    let mut a: R = 4;
    arr_u8.append(a);
    {
        use C::R;
        a = 3;
        arr_u8.append(a);
        let a: R = 8;
        arr_u16.append(a);
    }
    a = 2;
    arr_u8.append(a);
    assert_eq!(arr_u8.len(), 3);
    assert_eq!(arr_u8[0], @4);
    assert_eq!(arr_u8[1], @3);
    assert_eq!(arr_u8[2], @2);
    assert_eq!(arr_u16.len(), 1);
    assert_eq!(arr_u16[0], @8);
}

#[test]
fn test_type_use_shadowing() {
    let mut arr_u8 = ArrayTrait::<u8>::new();
    let mut arr_u16 = ArrayTrait::<u16>::new();
    type R = u8;
    let mut a: R = 4;
    arr_u8.append(a);
    {
        use C::R;
        a = 3;
        arr_u8.append(a);
        let a: R = 8;
        arr_u16.append(a);
    }
    a = 2;
    arr_u8.append(a);
    assert_eq!(arr_u8.len(), 3);
    assert_eq!(arr_u8[0], @4);
    assert_eq!(arr_u8[1], @3);
    assert_eq!(arr_u8[2], @2);
    assert_eq!(arr_u16.len(), 1);
    assert_eq!(arr_u16[0], @8);
}

#[test]
fn test_multiple_use_type() {
    let mut arr_u8 = ArrayTrait::<u8>::new();
    let mut arr_u16 = ArrayTrait::<u16>::new();
    use C::{R, Q};
    let a: R = 4;
    arr_u16.append(a);
    let b: Q = 6;
    arr_u8.append(b);
    assert_eq!(arr_u16.len(), 1);
    assert_eq!(arr_u16[0], @4);
    assert_eq!(arr_u8.len(), 1);
    assert_eq!(arr_u8[0], @6);
}
