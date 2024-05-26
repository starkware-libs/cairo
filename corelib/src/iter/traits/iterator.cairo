/// An iterator over a collection of values.
pub trait Iterator<T> {
    type Item;
    fn next(ref self: T) -> Option<Self::Item>;
}

/// Turn a collection of values into an iterator.
#[unstable(feature: "collections")]
pub trait IntoIterator<T> {
    type IntoIter;
    fn into_iter(ref self: T) -> Self::IntoIter<Iterator<T, Self::Item>>;
}
