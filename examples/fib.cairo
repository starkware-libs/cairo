// Calculates fib...
pub fn fib(a: felt252, b: felt252, n: felt252) -> felt252 {
    match n {
        0 => a,
        _ => fib(b, a + b, n - 1),
    }
}
[crate_roots]
stwo_cairo_verifier = "src"

[config.override.stwo_cairo_verifier]
edition = "2024_07"

[config.override.stwo_cairo_verifier.dependencies.bounded_int]

[config.override.bounded_int]
edition = "2023_10"
