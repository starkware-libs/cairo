// try
// try2
fn foo(x: T) -> S {
    let x = -5 + 3;
    let y: T = x * 2 + 3 - 5; // Comment.

    let z = 5;
    5 + Struct { a: 5, b: Struct2 { _gg: () } };
    let df = 6;
    {
        let k = 1;
        let z = 1;
        let block = {
            y + 1;
            x + 5
        };
    }

    { // Comment.
        6;

        // 445.

        5; // Comment.
    } // Comment.

    // A long comment that shouldn't affect the breaking of the next line. Make it a little longer.
    let var = 1 + 2 + 3;
}

fn bar<T>(x: T) -> T {
    let x: T = 1;
}


struct A {}
struct B {}

// Calculates fib, but all variables are boxes.
fn fib(a: Box::<felt>, b: Box::<felt>, n: Box::<felt>) -> Box::<felt> {
    match n {
        0 => {
            a
        },
        _ => {
            fib(
                b,
                into_box::<felt>(unbox::<felt>(a) + unbox::<felt>(b)),
                into_box::<felt>(unbox::<felt>(n) - 1),
            )
        },
    }
}
