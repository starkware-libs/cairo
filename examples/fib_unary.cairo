// Test two level of inline module nesting.
mod inner {
    pub mod inner {
        pub fn fib_inner(a: felt252, b: felt252, n: felt252) -> felt252 {
            if n != 0 {
                fib_inner(b, a + b, n - 1)
            } else {
                a
            }
        }
    }
}

fn fib(n: felt252) -> felt252 {
    -inner::inner::fib_inner(-1, -1, n)
}
