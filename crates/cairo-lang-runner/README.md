# Compiling and running cairo files

```
cargo run --bin cairo-run -- -p /path/to/file.cairo
```

If we want to run code that is gas tested:
```
cargo run --bin cairo-run -- -p /path/to/file.cairo --available-gas 200
```

We currently only run the `main` function with no arguments beside implicits.

# Examples

## With gas:
```
fn main() -> Option::<felt252> {
    fib(1, 1, 13)
}

/// Calculates fib...
fn fib(a: felt252, b: felt252, n: felt252) -> Option::<felt252> {
    gas::withdraw_gas()?;
    match n {
        0 => Option::<felt252>::Some(a),
        _ => fib(b, a + b, n - 1),
    }
}
```

## Without gas:
```
// Calculates fib...
fn main() -> u128 {
    fib(1_u128, 1_u128, 100_u128)
}

fn fib(a: u128, b: u128, n: u128) -> u128 {
    if n == 0 {
        a
    } else {
        fib(b, a + b, n - 1_u128)
    }
}
```

# Additional Information
* Functions with calls to `withdraw_gas` will not compile without `--available-gas` value.
* Functions without calls to `withdraw_gas` will not compile with `--available-gas` value.
* When running functions returning arrays `--print-full-memory` should probably be used, to actually see the values contained in the array.
