// Calculates fib, but all variables are boxes.
fn fib(a: Box<felt252>, b: Box<felt252>, n: Box<felt252>) -> Box<felt252> {
    let unboxed_n = n.unbox();
    if unboxed_n == 0 {
        a
    } else {
        fib(b, BoxTrait::new(a.unbox() + b.unbox()), BoxTrait::new(unboxed_n - 1),)
    }
}
