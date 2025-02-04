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
