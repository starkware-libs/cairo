// Calculates fib...
fn fib(a: int, b: int, n: int) -> int {
    if n == 0 { return a; }
    return fib(b, a+b, n-1);
}
