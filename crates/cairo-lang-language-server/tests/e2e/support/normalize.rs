use std::path::Path;

use crate::support::fixture::Fixture;

/// Like [`assert_eq`], but performs normalization of the left side.
macro_rules! assert_normalized_eq {
    ($fixture:expr, $actual:expr, $expected:expr $(,)?) => {
        assert_eq!($crate::support::normalize::normalize($fixture, $actual), $expected,);
    };
}

pub(crate) use assert_normalized_eq;

/// Performs various normalization steps of the input data, to remove any runtime-specific artifacts
/// and make comparisons in test assertions deterministic.
pub fn normalize(fixture: impl AsRef<Fixture>, data: impl ToString) -> String {
    return inner(fixture.as_ref(), data.to_string());

    fn inner(fixture: &Fixture, data: String) -> String {
        normalize_well_known_paths(fixture, normalize_paths(data))
    }
}

/// Replace all well-known paths/urls for a fixture with placeholders.
fn normalize_well_known_paths(fixture: &Fixture, data: String) -> String {
    let mut data = data
        .replace(&fixture.root_url().to_string(), "[ROOT_URL]")
        .replace(&normalize_path(fixture.root_path()), "[ROOT]");

    if let Ok(pwd) = std::env::current_dir() {
        data = data.replace(&normalize_path(&pwd), "[PWD]");
    }

    data
}

/// Normalizes path separators.
fn normalize_paths(data: String) -> String {
    data.replace('\\', "/")
}

/// Normalize a path to a consistent format.
fn normalize_path(path: &Path) -> String {
    normalize_paths(path.to_string_lossy().to_string())
}
