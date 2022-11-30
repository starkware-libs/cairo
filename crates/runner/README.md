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
func main() -> Option::<felt> implicits (rc: RangeCheck, gb: GasBuiltin) {
    fib(1, 1, 13)
}

/// Calculates fib...
func fib(a: felt, b: felt, n: felt) -> Option::<felt> implicits (rc: RangeCheck, gb: GasBuiltin) {
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
func main() -> Option::<uint128> implicits (rc: RangeCheck) {
    fib(uint128_from_felt(1)?, uint128_from_felt(1)?, uint128_from_felt(100)?)
}

func fib(a: uint128, b: uint128, n: uint128) -> Option::<uint128> implicits (rc: RangeCheck) {
    match uint128_to_felt(n) {
        0 => Option::<uint128>::Some(a),
        _ => {
            let r = fib(b, (a + b)?, (n - uint128_from_felt(1)?)?)?;
            Option::<uint128>::Some(r)
        },
    }
}
```

# Additional Information
* Functions without calls to `get_gas` will not compile without `--available-gas` value.
* Functions with calls to `get_gas` will not compile with `--available-gas` value.
* When running functions returning arrays `--print-full-memory` should probably be used, to actually see the values contained in the array.
