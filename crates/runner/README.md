# Compiling and running cairo files

```
cargo run --bin cairo-run -- -p /path/to/file.cairo
```

If we want to run code that is gas tested:
```
cargo run --bin cairo-run -- -p /path/to/file.cairo --available-gas 200
```

We currently only run the a `main` function with no arguments beside implicits.

# Examples

## With gas:
```
func main() -> Option::<felt> implicits(RangeCheck, GasBuiltin) {
    fib(1, 1, 13)
}

/// Calculates fib...
func fib(a: felt, b: felt, n: felt) -> Option::<felt> implicits(RangeCheck, GasBuiltin) {
    get_gas()?;
    match n {
        0 => Option::<felt>::Some(a),
        _ => fib(b, a + b, n - 1),
    }
}
```

## Without gas:
```
// Calculates fib...
func main() -> Option::<u128> implicits(RangeCheck) {
    fib(u128_from_felt(1)?, u128_from_felt(1)?, u128_from_felt(100)?)
}

func fib(a: u128, b: u128, n: u128) -> Option::<u128> implicits(RangeCheck) {
    match u128_to_felt(n) {
        0 => Option::<u128>::Some(a),
        _ => {
            let r = fib(b, (a + b)?, (n - u128_from_felt(1)?)?)?;
            Option::<u128>::Some(r)
        },
    }
}
```

# Additional Information
* Functions without calls to `get_gas` will not compile without `--available-gas` value.
* Functions with calls to `get_gas` will not compile with `--available-gas` value.
* When running functions returning arrays `--print-full-memory` should probably be used, to actually see the values contained in the array.
