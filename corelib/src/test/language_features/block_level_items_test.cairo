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

pub mod single_const {
    pub const A: u8 = 1;
}

#[test]
fn test_use_usage() {
    use single_const::A;
    assert_eq!(A, 1);
}

#[test]
fn test_use_usage_with_alias() {
    use single_const::A as B;
    assert_eq!(B, 1);
}

#[test]
fn test_use_constant_shadowing() {
    use single_const::A;
    assert_eq!(A, 1);
    {
        const A: u8 = 4;
        assert_eq!(A, 4);
    }
    assert_eq!(A, 1);
}

pub mod double_const {
    pub const A: u8 = 4;
    pub const B: u8 = 6;
}

#[test]
fn test_use_use_shadowing() {
    use single_const::A;
    assert_eq!(A, 1);
    {
        use double_const::A;
        assert_eq!(A, 4);
    }
    assert_eq!(A, 1);
}

#[test]
fn test_const_use_shadowing() {
    const A: u8 = 1;
    assert_eq!(A, 1);
    {
        use double_const::A;
        assert_eq!(A, 4);
    }
    assert_eq!(A, 1);
}

#[test]
fn test_use_let_shadowing() {
    use single_const::A;
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
        use double_const::A;
        assert_eq!(A, 4);
    }
    assert_eq!(A, 1);
}

#[test]
fn test_multiple_use() {
    use double_const::{A, B};
    assert_eq!(A, 4);
    assert_eq!(B, 6);
}

pub mod generic_type {
    pub struct S {
        pub x: u8,
    }
    pub enum E {
        A: u8,
        B: u16,
    }
}

#[test]
fn test_type_struct_usage() {
    use generic_type::S;
    let s = S { x: 1 };
    assert_eq!(s.x, 1);
}

#[test]
fn test_type_enum_usage() {
    use generic_type::E;
    let e = E::A(1);
    match e {
        E::A(val) => assert_eq!(val, 1),
        E::B(_) => panic!("Shouldn't get here"),
    }
}

pub mod generic_type_generics {
    pub struct S<T> {
        pub x: T,
    }
    pub enum E<T> {
        A: T,
        B: u16,
    }
}

#[test]
fn test_type_struct_generic_usage() {
    use generic_type_generics::S;
    let s = S::<u8> { x: 1 };
    assert_eq!(s.x, 1);
}

#[test]
fn test_type_enum_generic_usage() {
    use generic_type_generics::E;
    let e = E::A::<u8>(1);
    match e {
        E::A(val) => assert_eq!(val, 1),
        E::B(_) => panic!("Shouldn't get here"),
    }
}

mod a {
    pub mod b {
        pub const C: u8 = 1;
    }
}

#[test]
fn use_module_in_statement() {
    use a::b;
    assert_eq!(b::C, 1);
}

#[test]
fn use_module_in_statement_with_self() {
    use a::b::{self, C};
    assert_eq!(C, b::C);
}
