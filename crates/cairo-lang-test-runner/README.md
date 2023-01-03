# Testing cairo files

```
cargo run --bin cairo-test -- -p /path/to/file.cairo
```

We can use this command to run Cairo level tests.

# Example

```
#[test]
fn test_assert_true() {
    // Asserts that true
    assert(true, 'assert(true)');
}

#[test]
#[should_panic]
fn test_assert_false() {
    assert(false, 'assert(false)');
}
```

# Longer Example

Longer example can be found at [Core Library Test](../../corelib/test.cairo).

```
cargo run --bin cairo-test -- -p corelib/
```
