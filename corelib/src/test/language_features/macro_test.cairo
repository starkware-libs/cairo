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

    ($x:expr + $y:expr) => {
        $x + $y
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
    assert_eq!(add_exprs!(1 + 2), 3);
    assert_eq!(add_exprs!(3 123), 4);
    assert_eq!(add_exprs!(abc 1 2), 3);
    assert_eq!(add_exprs!(0 2), 2);
}

macro optional_repetitions {
    ($x:ident) => {
        $x
    };

    ($x:ident {$y:ident }? {$z:ident }?) => {
        2
    };

    ($x:ident {$y:ident, {$z:ident }? }?) => {
        3
    };

    ($x:ident, $y:ident?) => {
        OptionTrait::unwrap_or(y, $x)
    };

    ($x:ident, $y:ident?, $z:ident?) => {
        OptionTrait::unwrap_or(z, OptionTrait::unwrap_or(y, $x))
    };
}
#[test]
fn test_macro_optional_repetitions() {
    let x = 1;
    assert_eq!(optional_repetitions!(x), 1);
    let y: Option<felt252> = Some(2);
    assert_eq!(optional_repetitions!(x, y), 2);
    let z: Option<felt252> = Some(3);
    assert_eq!(optional_repetitions!(x, y, z), 3);
    assert_eq!(optional_repetitions!(x {y, {
        z
    }}), 3);
}
