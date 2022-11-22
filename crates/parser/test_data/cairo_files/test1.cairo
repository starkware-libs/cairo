mod submod;
use submod::something;

// Func foo
// Second line
;
func foo(,var1: int,, mut ref var2: felt,) -> int {
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
    let x = true; // bla1
                  // bla2
    z = 5;
    func1(x);
    func2::<int>(x)
    return x;

func bar<A, B>() -> felt { }

extern type S<>;
extern func glee<A, b>(var1: int,) -> crate::S<int> nopanic;

struct A<A, B> {
    member: bool,
    member2: (bool, felt, ())
}
