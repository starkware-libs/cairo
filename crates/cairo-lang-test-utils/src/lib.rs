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

// Fails if there are diagnostics when args.expect_diagnostics == false.
// Fails if there are no diagnostics when args.expect_diagnostics == true.
// Succeeds otherwise.
pub fn verify_diagnostics_expectation(
    args: &OrderedHashMap<String, String>,
    diagnostics: &str,
) -> Result<(), String> {
    let Some(expect_diagnostics) = args.get("expect_diagnostics") else {
        return Ok(());
    };
    if expect_diagnostics.trim() == "*" {
        return Ok(());
    }

    let expect_diagnostics = bool_input(expect_diagnostics);
    let has_diagnostics = !diagnostics.is_empty();
    if !expect_diagnostics && has_diagnostics {
        Err(format!(
            "`expect_diagnostics` is false, but diagnostics were generated:\n{}",
            diagnostics
        ))
    } else if expect_diagnostics && !has_diagnostics {
        Err("`expect_diagnostics` is true, but no diagnostics were generated\n".to_string())
    } else {
        Ok(())
    }
}

/// Translates a string test input to bool ("false" -> false, "true" -> true). Panics if invalid.
/// Ignores case and whitespaces in the edges.
pub fn bool_input(input: &str) -> bool {
    let input = input.trim().to_lowercase();
    bool::from_str(&input).unwrap_or_else(|_| panic!("Expected 'true' or 'false', actual: {input}"))
}
