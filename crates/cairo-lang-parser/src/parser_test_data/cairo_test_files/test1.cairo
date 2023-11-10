mod submod;
use submod::something;

// Func foo
// Second line
;
fn foo(,var1: int,, mut ref var2: felt252,) -> int {
    let bla3 = --7; // succeeds, ok
    let bla4 = -!-!8; // succeeds, ok
    let x = 1;
    let y = match x {
        0 => { 1 },
        _ => 0,
    };
    let z = if 0 + x == y {
        1
    } else {
        2
    };
    let block_combination = {5} + match x { E::V1(_) => 4, E::V2(_) => 2 }
        - if cond { 32 } else { 1 };
    let w = calc_with_error(x, y?, z)?;
    let member_value = value.member?;
    let method_return = value.method()?;
    let x = true; // bla1
                  // bla2
    z = 5;
    func1(x);
    func2::<int>(x)
    return x;

fn bar<A, B>() -> felt252 { }

extern type S<>;
extern fn glee<A, b>(var1: int,) -> crate::S<int> nopanic;

struct A<A, B> {
    pub member: bool,
    #[annot(a)]
    member2: (bool, felt252, ())
}

pub(crate) enum E<A, B> {
    V1: A,
    V2: B,
    VEmpty,
}

fn match_e_enum(e: E) -> felt252 {
    match e {
        E::V1(_) => 1,
        E::V2(x) => 2,
        E::VEmpty => 0,
    }
}

type Renamed = submod::inner::Other;
type Generic<T> = super::other::OtherGeneric::<T>;

inline_item_macro!(x, y, z);

#[attribute_without_item]
