# Cairo Test Runner

This guide explains how to write and run tests for Cairo programs using the Cairo test runner.

## Quick Start

To run tests in a single Cairo file:
```bash
cargo run --bin cairo-test -- --single-file /path/to/file.cairo
```

To run all tests in a directory:
```bash
cargo run --bin cairo-test -- /path/to/directory/
```

## Writing Tests

Tests in Cairo are functions marked with the `#[test]` attribute. Here are some examples:

```cairo
#[test]
fn test_assert_true() {
    // Basic assertion
    assert(true, 'assert(true)');
}

#[test]
#[should_panic]
fn test_assert_false() {
    // This test is expected to fail
    assert(false, 'assert(false)');
}

#[test]
fn test_basic_arithmetic() {
    let a = 5;
    let b = 3;
    assert(a + b == 8, 'Basic addition failed');
}
```

## Test Attributes

- `#[test]` - Marks a function as a test
- `#[should_panic]` - Indicates that the test is expected to fail
- `#[available_gas(n)]` - Sets the gas limit for the test

## Command Line Options

- `--single-file <path>` - Run tests from a single file
- `-f, --filter <string>` - Run only tests containing the specified string
- `--path <path>` - Run tests from a directory
- `--include-ignored` - Run ignored tests as well

### Filtering Examples

Run specific tests by name:
```bash
cargo run --bin cairo-test -- --single-file /path/to/file.cairo -f test_name
```

## More Examples

For more comprehensive test examples, check out:
- Core Library Tests: [Core Library Test](../../corelib/src/test.cairo)
- Integration Tests: Check the `tests/` directory in the project root

## Best Practices

1. Give your tests descriptive names
2. Include meaningful assertion messages
3. Test both success and failure cases
4. Use appropriate attributes for special cases

## Running the Core Library Tests

To run the entire core library test suite:
```bash
cargo run --bin cairo-test -- corelib/
```
