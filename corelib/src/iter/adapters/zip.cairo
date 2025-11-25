/// An iterator that iterates two other iterators simultaneously.
///
/// This `struct` is created by [`zip`] or [`Iterator::zip`].
/// See their documentation for more.
///
/// [`Iterator::zip`]: core::iter::Iterator::zip
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
/// See the documentation of [`Iterator::zip`] for more.
///
/// # Examples
///
/// ```
/// use core::iter::zip;
///
/// let xs = array![1, 2, 3];
/// let ys = array![4, 5, 6];
///
/// let mut iter = zip(xs, ys);
///
/// assert_eq!(iter.next().unwrap(), (1, 4));
/// assert_eq!(iter.next().unwrap(), (2, 5));
/// assert_eq!(iter.next().unwrap(), (3, 6));
/// assert!(iter.next().is_none());
/// ```
#[inline]
pub fn zip<
    A,
    B,
    impl AIntoIter: IntoIterator<A>,
    impl BIntoIter: IntoIterator<B>,
    +Destruct<AIntoIter::IntoIter>,
    +Destruct<B>,
>(
    a: A, b: B,
) -> Zip<AIntoIter::IntoIter, BIntoIter::IntoIter> {
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
        Some((a, b))
    }
}
