pub mod parse_test_file;
use std::fs;
use std::path::Path;
use std::sync::{Mutex, MutexGuard};

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
    // If this error occurs, look for a "thread '...' panicked at ..." string in the tests output to
    // identify the test culprit.
    m.lock().expect(&format!(
        "Lock failed, probably because another test panicked while holding a lock."
    ))
}
