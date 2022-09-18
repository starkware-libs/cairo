use std::fmt;

pub mod extract_matches;
pub mod ordered_hash_map;
pub mod ordered_hash_set;
pub mod unordered_hash_map;

#[cfg(any(feature = "testing", test))]
pub mod parse_test_file;

/// Similar to From / TryFrom, but returns an option.
pub trait OptFrom<T>
where
    Self: Sized,
{
    fn opt_from(other: T) -> Option<Self>;
}

pub fn write_comma_separated<V: std::fmt::Display>(
    f: &mut fmt::Formatter<'_>,
    values: &[V],
) -> fmt::Result {
    values.iter().take(1).try_for_each(|v| write!(f, "{v}"))?;
    values.iter().skip(1).try_for_each(|v| write!(f, ", {v}"))
}
