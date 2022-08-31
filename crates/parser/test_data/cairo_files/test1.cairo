// Func foo
// Second line
;
func foo(,var1: int,, var2: felt,) -> int {
    let bla3 = --7; // succeeds, ok
    let bla4 = -!-!8; // succeeds, ok
    let x = 1;
    let y = match x {
        0 => { 1 },
        _ => 0,
    };
    let x = true; // bla1
                  // bla2
    func1(x);
    func2(x)
    return x;
}

extern type felt;
extern func bar(var1: int,) -> int;
