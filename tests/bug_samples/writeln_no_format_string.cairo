//! Regression: `writeln!(f)` (a formatter with no format string) writes just a newline, matching
//! Rust. It previously errored with "Macro expected format string argument". `println!()` is valid
//! for free via the same `writeln!(f, )` expansion; `write!(f)` / `print!()` still require a format
//! string.

use core::fmt::Formatter;

#[test]
fn test_writeln_with_no_format_string_writes_newline() {
    let mut f: Formatter = Default::default();
    let _ = writeln!(f);
    assert!(f.buffer == "\n");
    // `println!()` (no arguments) now compiles too, via the same expansion.
    println!();
}
