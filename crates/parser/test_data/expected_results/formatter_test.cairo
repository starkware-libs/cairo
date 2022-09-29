// try
// try2
func foo(x: T) -> S {
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
}


struct A { }
struct B { }
// This test is broken in this PR, will be fixed in the next one.
// Calculates fib, but all variables are references.
// func fib(a: Ref::<felt>, b: Ref::<felt>, n: Ref::<felt>) -> Ref::<felt> {
//     match n {
//         0 => { a },
//         _ => {
//             fib(
//                 b,
//                 into_ref::<felt>(deref::<felt>(a) + deref::<felt>(b)),
//                
//                 into_ref::<felt>(deref::<felt>(n) - 1),
//             )
//         },
//     }
// }
