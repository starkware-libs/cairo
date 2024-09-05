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
