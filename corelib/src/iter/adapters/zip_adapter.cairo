/// An iterator that iterates two other iterators simultaneously.
#[derive(Drop, Clone)]
#[must_use]
pub struct Zip<A, B> {
    a: A,
    b: B,
}

#[inline]
pub fn zipped_iterator<A, B>(a: A, b: B) -> Zip<A, B> {
    Zip { a, b }
}

/// Converts the arguments to iterators and zips them.
///
/// # Examples
///
/// ```
/// use core::iter::zip;
///
/// let mut iter = zip(array![1, 2, 3], array![4, 5, 6]);
///
/// assert_eq!(iter.next(), Option::Some((1, 4)));
/// assert_eq!(iter.next(), Option::Some((2, 5)));
/// assert_eq!(iter.next(), Option::Some((3, 6)));
/// assert_eq!(iter.next(), Option::None);
///
/// // Nested zips are also possible:
/// let mut iter = zip(zip(array![1, 2, 3], array![4, 5, 6]), array![7, 8, 9]);
///
/// assert_eq!(iter.next(), Option::Some(((1, 4), 7)));
/// assert_eq!(iter.next(), Option::Some(((2, 5), 8)));
/// assert_eq!(iter.next(), Option::Some(((3, 6), 9)));
/// assert_eq!(iter.next(), Option::None);
/// ```
pub fn zip<
    A,
    B,
    impl IntoIterA: IntoIterator<A>,
    impl IntoIterB: IntoIterator<B>,
    +Destruct<A>,
    +Destruct<B>,
    +Destruct<IntoIterA::IntoIter>,
    +Destruct<IntoIterB::IntoIter>,
>(
    a: A, b: B,
) -> Zip<IntoIterA::IntoIter, IntoIterB::IntoIter> {
    zipped_iterator(a.into_iter(), b.into_iter())
}

impl ZipIterator<
    A,
    B,
    impl IterA: Iterator<A>,
    impl IterB: Iterator<B>,
    +Destruct<A>,
    +Destruct<B>,
    +Destruct<IterA::Item>,
    +Destruct<IterB::Item>,
> of Iterator<Zip<A, B>> {
    type Item = (IterA::Item, IterB::Item);

    #[inline]
    fn next(ref self: Zip<A, B>) -> Option<Self::Item> {
        let a = self.a.next()?;
        let b = self.b.next()?;
        Option::Some((a, b))
    }
}
