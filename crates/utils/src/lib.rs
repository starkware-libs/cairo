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

pub fn write_comma_separated<'a, Iter: IntoIterator<Item = V>, V: std::fmt::Display>(
    f: &mut fmt::Formatter<'_>,
    values: Iter,
) -> fmt::Result {
    let mut is_first = true;
    for value in values {
        if !is_first {
            write!(f, ", ")?;
        }
        write!(f, "{value}")?;
        is_first = false;
    }
    Ok(())
}
