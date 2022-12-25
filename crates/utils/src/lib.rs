use std::fmt;

pub mod casts;
pub mod collection_arithmetics;
pub mod extract_matches;
pub mod logging;
pub mod ordered_hash_map;
pub mod ordered_hash_set;
pub mod strongly_connected_components;
pub mod unordered_hash_map;
pub mod unordered_hash_set;

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

/// Helper operations on `Option<T>`.
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

/// Helper operations on `Option<T>`.
pub trait ResultHelper<E> {
    fn on_err<F: FnOnce(&E)>(self, f: F) -> Self;
}
impl<T, E> ResultHelper<E> for Result<T, E> {
    fn on_err<F: FnOnce(&E)>(self, f: F) -> Self {
        match &self {
            Ok(_) => self,
            Err(e) => {
                f(e);
                self
            }
        }
    }
}

/// Borrows a mutable reference as Box for the lifespan of this function. Runs the given closure
/// with the boxed value as a parameter.
/// The closure is expected to return a boxed value, whose changes will be reflected on the mutable
/// reference.
/// Example:
/// ```
/// use utils::borrow_as_box;
/// let mut x = 5;
/// borrow_as_box(&mut x, |mut x: Box<usize>| {
///     *x += 1;
///     ((), x)
/// });
/// assert_eq!(x, 6);
/// ```
pub fn borrow_as_box<T: Default, R, F: FnOnce(Box<T>) -> (R, Box<T>)>(ptr: &mut T, f: F) -> R {
    // TODO(spapini): Consider replacing take with something the leaves the memory dangling, instead
    // of filling with default().
    let (res, boxed) = f(Box::new(std::mem::take(ptr)));
    *ptr = *boxed;
    res
}
