// Calculates fib...
fn fib(mut a: felt252, mut b: felt252, mut n: felt252) -> felt252 {
    loop {
        if n == 0 {
            break a;
        }
        n = n - 1;
        let temp = b;
        b = a + b;
        a = temp;
    }
}
