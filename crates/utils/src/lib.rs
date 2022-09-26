use std::fmt;

pub mod extract_matches;
pub mod ordered_hash_map;
pub mod ordered_hash_set;
pub mod unordered_hash_map;

#[cfg(any(feature = "testing", test))]
pub mod parse_test_file;
#[cfg(any(feature = "testing", test))]
pub use parse_test_file::parse_test_file;

/// Similar to From / TryFrom, but returns an option.
pub trait OptionFrom<T>
where
    Self: Sized,
{
    fn option_from(other: T) -> Option<Self>;
}

pub fn write_comma_separated<Iter: IntoIterator<Item = V>, V: std::fmt::Display>(
    f: &mut fmt::Formatter<'_>,
    values: Iter,
) -> fmt::Result {
    let mut iter = values.into_iter();
    if let Some(value) = iter.next() {
        write!(f, "{value}")?;
    }
    for value in iter {
        write!(f, ", {value}")?;
    }
    Ok(())
}

/// Helper operations on Option<T>.
pub trait OptionHelper {
    fn on_none<F: FnOnce()>(self, f: F) -> Self;
}
impl<T> OptionHelper for Option<T> {
    fn on_none<F: FnOnce()>(self, f: F) -> Self {
        if self.is_none() {
            f();
        }
        self
    }
}
