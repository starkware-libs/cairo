// Calculates fib...
pub fn fib(a: felt252, b: felt252, n: felt252) -> felt252 {
    match n {
        0 => a,
        _ => fib(b, a + b, n - 1),
    }
}
fn foo(a: A) -> felt252 {
    match a {
        A::Two(_) => { 2 },
        A::One(_) => { 1 },
    }
}
