// try
// try2
fn foo(x: T) -> S {
    let x = -5 + 3;
    let y: T = x * 2 + 3 - 5; // Comment.

    let z = 5;
    z += 4;
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
    let x: [u32; 3] = [1, 2, 3];
    let [a, b, c] = x;
    let [a, b, c] = f(
        "very long string that should not cause a break in the fixed size array pattern",
    );
}


struct A {}
struct B {}

// Calculates fib, but all variables are boxes.
fn fib(a: Box<felt252>, b: Box<felt252>, n: Box<felt252>) -> Box<felt252> {
    match n {
        0 => { a },
        _ => {
            fib(
                b,
                into_box::<felt252>(unbox::<felt252>(a) + unbox::<felt252>(b)),
                into_box::<felt252>(unbox::<felt252>(n) - 1),
            )
        },
    }
    // A trailing comment.
}

fn if_let_test() {
    if let (x, y) =
        (
            x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x,
            y + y + y + y + y + y,
        ) {
        if_let_block_content();
    }
}


impl DropMyImplCoupon<T> of Drop<MyImpl::<T>::trait_fn::Coupon>;
impl DropMyImplCoupon<T> of Drop<MyImpl::<T>::trait_fn>;
impl DropMyImplCoupon<T> of Drop<MyImpl<T>>;
