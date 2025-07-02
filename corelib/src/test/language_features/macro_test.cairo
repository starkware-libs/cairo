macro count_idents {
    ($x:ident) => { 1 };

    ($x:ident, $y:ident) => { 2 };

    ($x:ident, $y:ident, $z:ident) => { 3 };

    ($x:ident $y:ident $z:ident) => { 3 };

    ($x:ident | $y:ident | $z:ident) => { 3 };

    (abc {$x:ident, (abc $y:ident) }) => { 2 };
}

#[test]
fn test_macro_count_idents() {
    assert_eq!(count_idents!(x), 1);
    assert_eq!(count_idents!(x, y), 2);
    assert_eq!(count_idents!(x, y, z), 3);
    assert_eq!(count_idents!(x y z), 3);
    assert_eq!(count_idents!(x | y | z), 3);
    assert_eq!(count_idents!(abc {x, (abc y) }), 2);
}

macro add_one {
    ($x:ident) => { $x + 1 };
}

#[test]
fn test_macro_add_one() {
    let x1 = 1;
    let x2 = 2;
    let x3 = 3;
    assert_eq!(add_one!(x1), 2);
    assert_eq!(add_one!(x2), 3);
    assert_eq!(add_one!(x3), 4);
}

mod test_assert_eq {
    macro assert_eq {
        ($left:ident, $right:ident) => {
            if $left != $right {
                $callsite::panic!("PANIC!");
            }
        };
    }

    #[test]
    #[should_panic(expected: ("PANIC!",))]
    fn test_user_defined_assert_eq() {
        {
            let x = 1;
            let y = 2;
            assert_eq!(x, y);
        }
    }
}

macro add_exprs {
    ($x:expr) => { $x + 1 };

    ($x:expr 2) => { $x + 2 };

    ($x:ident 1) => { $x + 1 };

    ($x:expr $y:expr) => { $x + $y };

    (abc $x:expr $y:expr) => { $x + $y  };
}

#[test]
fn test_add_exprs() {
    assert_eq!(add_exprs!(3), 4);
    let x = 1;
    assert_eq!(add_exprs!(x 1), 2);
    assert_eq!(add_exprs!(2 - 1), 2);
    assert_eq!(add_exprs!(3 123), 126);
    assert_eq!(add_exprs!(abc 1 2), 3);
    assert_eq!(add_exprs!(0 2), 2);
    assert_eq!(add_exprs!(0 + 2), 3);
}


mod inner {
    fn foo(x: felt252) -> felt252 {
        x + 1
    }

    pub macro add_one {
        ($x:ident) => { $defsite::foo($x) };
    }

    pub macro add_ten {
        ($x:ident) => { $defsite::foo($x) + 9 };
    }

    pub macro add_recursive {
        ($x:ident) => {
            {
                let x = $x;
                $defsite::foo(x) + $defsite::inner::add_one!(x)
            }
        };
    }
    mod inner {
        fn foo(x: felt252) -> felt252 {
            x + 2
        }

        pub macro add_one {
            ($x:expr) => { $defsite::foo($x) };
        }
    }
}


#[test]
fn test_macro_add_one_with_defsite() {
    let x1 = 1;
    let x2 = 2;
    let x3 = 3;
    assert_eq!(inner::add_one!(x1), 2);
    assert_eq!(inner::add_one!(x2), 3);
    assert_eq!(inner::add_one!(x3), 4);
}


#[test]
fn test_macro_add_recursive() {
    let x1 = 1;
    let x2 = 2;
    let x3 = 3;
    assert_eq!(inner::add_recursive!(x1), 5);
    assert_eq!(inner::add_recursive!(x2), 7);
    assert_eq!(inner::add_recursive!(x3), 9);
}

#[test]
fn test_macro_add_ten() {
    let x1 = 1;
    let x2 = 2;
    let x3 = 3;
    assert_eq!(inner::add_ten!(x1), 11);
    assert_eq!(inner::add_ten!(x2), 12);
    assert_eq!(inner::add_ten!(x3), 13);
}

macro accessing_expanded_placehoders {
    ($x:expr, $y:expr) => { ($x, $y) };

    ($x:expr) => {
        {
            let z = 2;
            // TODO(Dean): Use $x directly in the macro call when supported.
            let y = $x;
            $defsite::accessing_expanded_placehoders!(y, z)
        }
    };
}

#[test]
fn test_accessing_expanded_placehoders() {
    let x = 1;
    assert_eq!(accessing_expanded_placehoders!(x), (1, 2));
}

macro use_z_from_callsite {
    () => {
        {
            let z = 4;
            ($callsite::z, z)
        }
    };
}

#[test]
fn test_use_z_from_callsite() {
    let z = 3;
    assert_eq!(use_z_from_callsite!(), (3, 4));
}

macro macro_wrapped_use_z_from_callsite {
    () => {
        {
            let z = 2;
            $defsite::use_z_from_callsite!()
        }
    };
}

#[test]
fn test_wrap_use_z_from_callsite() {
    assert_eq!(macro_wrapped_use_z_from_callsite!(), (2, 4));
}

mod repetition_macro_matcher {
    macro matcher_plus {
        ($($x:expr), +) => { 111 };
    }

    macro matcher_star {
        ($($x:expr), *) => { 222 };
    }

    macro matcher_optional {
        ($($x:expr)?) => { 333 };
    }

    macro matcher_ident_star {
        ($($x:ident) *) => { 444 };
    }

    #[test]
    fn repetition_macro_matcher() {
        assert_eq!(matcher_plus!(1), 111);
        assert_eq!(matcher_plus!(1, 2), 111);
        #[cairofmt::skip]
        assert_eq!(matcher_plus!(1, 2,), 111);
        #[cairofmt::skip]
        assert_eq!(matcher_plus!(1,), 111);

        assert_eq!(matcher_star!(), 222);
        assert_eq!(matcher_star!(3), 222);
        assert_eq!(matcher_star!(4, 5), 222);
        #[cairofmt::skip]
        assert_eq!(matcher_star!(3,), 222);
        #[cairofmt::skip]
        assert_eq!(matcher_star!(4, 5,), 222);

        assert_eq!(matcher_optional!(), 333);
        assert_eq!(matcher_optional!(9), 333);

        let _x1 = 42;
        assert_eq!(matcher_ident_star!(_x1), 444);
        assert_eq!(matcher_ident_star!(_x1 _x1 _x1), 444);
    }
}

mod repetition_macro_expansion {
    macro repetition_macro_expansion {
        ($($x:ident), +) => { array![$($x + 2), +] };

        ($($x:expr), *) => { array![$($x + 1), *] };
    }


    #[test]
    fn test_repetition_macro_expansion() {
        let expected_expr = array![2, 3];
        let actual_expr = repetition_macro_expansion!(1, 2);
        assert_eq!(expected_expr, actual_expr);
        let x = 1;
        let expected_ident = array![3];
        let actual_ident = repetition_macro_expansion!(x);
        assert_eq!(expected_ident, actual_ident);
    }
}

macro count_exprs_rec {
    [] => { 0 };

    [$x:expr] => { 1 };

    [$x:expr, $($xs:expr), +] => { 1 + count_exprs_rec![$($xs), +] };
}

#[test]
fn test_count_exprs_rec() {
    assert_eq!(count_exprs_rec![], 0);
    assert_eq!(count_exprs_rec![5], 1);
    assert_eq!(count_exprs_rec![5, 6], 2);
    assert_eq!(count_exprs_rec![1 + 2, 3 * 4, 5 - 6], 3);
    assert_eq!(count_exprs_rec![10, 20, 30, 40, 50], 5);
}

macro my_array {
    [$x:expr] => {
        {
            let mut arr = $defsite::array![];
            arr.append($x);
            arr
        }
    };

    [$first:expr, $($rest:expr),*] => {
        {
            let mut arr = my_array![$($rest), *];
            arr.append($first);
            arr
        }
    };
}

#[test]
fn test_array_macro() {
    let result = my_array![1, 2, 3];
    let mut expected = array![3, 2, 1];
    assert_eq!(result, expected);
}

mod callsite_test {
    fn foo(x: felt252) -> felt252 {
        x + 100
    }

    mod inner {
        fn foo(x: felt252) -> felt252 {
            x + 200
        }

        pub macro call_foo {
            ($x:expr) => { $callsite::foo($x) };
        }
    }

    #[test]
    fn test_callsite_resolution() {
        assert_eq!(inner::call_foo!(5), 105);
        assert_eq!(inner::call_foo!((foo(5))), 205);
    }
}

macro statement_expansion {
    [$($x:expr), *] => {
        {
            let mut arr = $defsite::ArrayTrait::new ();
            $(arr.append ($x); arr.append ($x);)*
            arr
        }
    };
}

#[test]
fn test_statement_expansion() {
    let result = statement_expansion![1, 2, 3];
    let mut expected = array![1, 1, 2, 2, 3, 3];
    assert_eq!(result, expected);
}

mod defsite_as_type {
    type u32_redefinition = u32;
    pub macro use_redefinition {
        ($x:expr) => {
            {
                let mut x: $defsite::u32_redefinition = $x;
                x
            }
        };
    }
}

#[test]
fn test_defsite_as_type() {
    let _y = defsite_as_type::use_redefinition!(5_u32);
}


mod defsite_inference {
    #[derive(Drop, Copy, Default)]
    pub struct A {
        pub x: u32,
    }
    pub macro use_local_type {
        ($y:expr) => {
            {
                let mut x: $defsite::A = $defsite::A{
                    x: 0,
                };
                x.x = $y;
                x
            }
        };
    }
    pub macro use_local_impl_inference {
        ($y:expr) => {
            {
                let mut x: $defsite::A = $defsite::Default::default();
                x.x = $y;
                x
            }
        };
    }
}


#[test]
fn test_defsite_inference() {
    let y: defsite_inference::A = defsite_inference::use_local_type!(5);
    let z: defsite_inference::A = defsite_inference::use_local_impl_inference!(5);
    assert_eq!(y.x, z.x);
}


macro tail_only {
    () => {
        123
    };
}

macro wrapped_tail_only {
    () => {
        tail_only!()
    };
}

macro statements_and_tail {
    () => {
        let _y = 5;
        42
    };
}

#[test]
fn test_tail_macro_in_statement_position() {
    let _x = 1;
    wrapped_tail_only!();
}

#[test]
fn test_tail_macro_in_tail_position() -> felt252 {
    let _x = 1;
    wrapped_tail_only!()
}

#[test]
fn test_statements_and_tail_macro_in_statement_position() {
    statements_and_tail!();
}

#[test]
fn test_statements_and_tail_macro_in_tail_position() -> felt252 {
    statements_and_tail!()
}
