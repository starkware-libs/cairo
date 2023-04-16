use itertools::zip_eq;

/// Similar to [zip_eq], except that it works with 3 iterators.
pub fn zip_eq3<A, B, C>(a: A, b: B, c: C) -> impl Iterator<Item = (A::Item, B::Item, C::Item)>
where
    A: IntoIterator,
    B: IntoIterator,
    C: IntoIterator,
{
    zip_eq(a, zip_eq(b, c)).map(|(a, (b, c))| (a, b, c))
}
