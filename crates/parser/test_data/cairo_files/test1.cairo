// Func foo
// Second line
;
fn foo(,var1: int,, var2: felt,) -> int {
    let bla3 = --7; // succeeds, ok
    let bla4 = -!-!8; // succeeds, ok
    let x = 1;
    let x = true; // bla1
                  // bla2
    func(x);
    func2(x)
    return x;
}
