// Calculates fib, but all variables are boxes.
fn fib(a: Box<felt252>, b: Box<felt252>, n: Box<felt252>) -> Box<felt252> {
    let unboxed_n = n.unbox();
    if unboxed_n == 0 {
        a
    } else {
        fib(b, BoxImpl::new(a.unbox() + b.unbox()), BoxImpl::new(unboxed_n - 1),)
    }
}
