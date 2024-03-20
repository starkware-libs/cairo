#![cfg(feature = "testing")]

pub mod parse_test_file;
use std::fs;
use std::path::Path;
use std::str::FromStr;
use std::sync::{Mutex, MutexGuard};

use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
pub use parse_test_file::parse_test_file;

/// Returns the content of the relevant test file.
fn get_expected_contents(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_else(|_| panic!("Could not read file: '{path:?}'"))
}

/// Overrides the test file data.
fn set_contents(path: &Path, content: String) {
    fs::write(path, content).unwrap_or_else(|_| panic!("Could not write file: '{path:?}'"));
}

/// Compares content to examples content, or overrides it if the `CAIRO_FIX_TESTS` environment
/// value is set to `1`.
pub fn compare_contents_or_fix_with_path(path: &Path, content: String) {
    let is_fix_mode = std::env::var("CAIRO_FIX_TESTS") == Ok("1".into());
    if is_fix_mode {
        set_contents(path, content);
    } else {
        pretty_assertions::assert_eq!(content, get_expected_contents(path));
    }
}

/// Locks the given mutex, and prints an informative error on failure.
pub fn test_lock<'a, T: ?Sized + 'a>(m: &'a Mutex<T>) -> MutexGuard<'a, T> {
    match m.lock() {
        Ok(guard) => guard,
        // Allow other test to take the lock if it was poisoned by a thread that panicked.
        Err(poisoned) => poisoned.into_inner(),
    }
}

/// Returns an error string if there are diagnostics when args.expect_diagnostics is false.
/// Returns an error string if there are no diagnostics when args.expect_diagnostics is true.
/// Returns None on success.
pub fn verify_diagnostics_expectation(
    args: &OrderedHashMap<String, String>,
    diagnostics: &str,
) -> Option<String> {
    let expect_diagnostics = args.get("expect_diagnostics")?;
    if expect_diagnostics == "*" {
        return None;
    }

    let expect_diagnostics = bool_input(expect_diagnostics);
    let has_diagnostics = !diagnostics.is_empty();
    if !expect_diagnostics && has_diagnostics {
        Some(format!(
            "`expect_diagnostics` is false, but diagnostics were generated:\n{}",
            diagnostics
        ))
    } else if expect_diagnostics && !has_diagnostics {
        Some("`expect_diagnostics` is true, but no diagnostics were generated.\n".to_string())
    } else {
        None
    }
}

/// Translates a string test input to bool ("false" -> false, "true" -> true). Panics if invalid.
/// Ignores case.
pub fn bool_input(input: &str) -> bool {
    let input = input.to_lowercase();
    bool::from_str(&input).unwrap_or_else(|_| panic!("Expected 'true' or 'false', actual: {input}"))
}

/// Parses a test input that may be a file input. If the input starts with ">>> file: " it reads the
/// file and returns the file path and content, otherwise, it returns the input and a default dummy
/// path.
pub fn get_direct_or_file_content(input: &str) -> (String, String) {
    if let Some(path) = input.strip_prefix(">>> file: ") {
        (
            path.to_string(),
            fs::read_to_string(path).unwrap_or_else(|_| panic!("Could not read file: '{path}'")),
        )
    } else {
        ("dummy_file.cairo".to_string(), input.to_string())
    }
}
