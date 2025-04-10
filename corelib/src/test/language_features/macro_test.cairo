macro count_idents {
    ($x:ident) => {
        1
    };

    ($x:ident, $y:ident) => {
        2
    };

    ($x:ident, $y:ident, $z:ident) => {
        3
    };

    ($x:ident $y:ident $z:ident) => {
        3
    };

    ($x:ident | $y:ident | $z:ident) => {
        3
    };

    (abc {$x:ident, (abc $y:ident) }) => {
        2
    };
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
    ($x:ident) => {
        $x + 1
    };
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
                panic!("PANIC!");
            }
        };
    }

    #[test]
    #[should_panic(expected: ("PANIC!",))]
    fn test_user_defined_assert_eq() {
        let x = 1;
        let y = 2;
        assert_eq!(x, y);
    }
}

macro add_exprs {
    ($x:expr) => {
        $x + 1
    };

    ($x:expr 2) => {
        $x + 2
    };

    ($x:ident 1) => {
        $x + 1
    };

    ($x:expr $y:expr) => {
        $x + 1
    };

    (abc $x:expr $y:expr) => {
        $x + $y
    };
}
#[test]
fn test_add_exprs() {
    assert_eq!(add_exprs!(3), 4);
    let x = 1;
    assert_eq!(add_exprs!(x 1), 2);
    assert_eq!(add_exprs!(2 - 1), 2);
    assert_eq!(add_exprs!(3 123), 4);
    assert_eq!(add_exprs!(abc 1 2), 3);
    assert_eq!(add_exprs!(0 2), 2);
    assert_eq!(add_exprs!(0 + 2), 3);
}


mod inner {
    fn foo(x: felt252) -> felt252 {
        x + 1
    }

    pub macro add_one {
        ($x:ident) => {
            $defsite::foo($x)
        };
    }

    pub macro add_ten {
        ($x:ident) => {
            $defsite::foo($x) + 9
        };
    }

    pub macro add_recursive {
        ($x:ident) => {
            let x = $x;
            $defsite::foo(x) + $defsite::inner::add_one!(x)
        };
    }
    mod inner {
        fn foo(x: felt252) -> felt252 {
            x + 2
        }

        pub macro add_one {
            ($x:expr) => {
                $defsite::foo($x)
            };
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
    ($x:expr, $y:expr) => {
        ($x, $y)
    };

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
