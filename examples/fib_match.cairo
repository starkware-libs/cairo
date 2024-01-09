// Calculates fib...
fn fib(n: u32) -> u32 {
    match n {
        0_u32 => 1,
        1_u32 => 1,
        2_u32 => 2,
        3_u32 => 3,
        4_u32 => 5,
        _ => { 5 * fib(n - 4) + 3 * fib(n - 5) }
    }
}
