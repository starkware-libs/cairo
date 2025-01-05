# Compiling and running cairo files

```
cargo run --bin cairo-run -- --single-file /path/to/file.cairo
```

If we want to run code that is gas tested:

```
cargo run --bin cairo-run -- --single-file /path/to/file.cairo --available-gas 200
```

We currently only run the `main` function with no arguments besides implicits.

# Example

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

- When compiling with --available-gas, if there are cycles in the code, calls to
  `withdraw_gas_all` will be automatically added.
- Functions with calls to `withdraw_gas_all` will not compile without `--available-gas` value.
- Functions without calls to `withdraw_gas_all` will not compile with `--available-gas` value.
- When running functions returning arrays `--print-full-memory` should probably be used,
  to actually see the values contained in the array.
