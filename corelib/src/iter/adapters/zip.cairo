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


/// An iterator that iterates two other iterators simultaneously, panics if the iterators have
/// different lengths.
///
/// This `struct` is created by [`zip_eq`] or [`Iterator::zip_eq`].
/// See their documentation for more.
///
/// Unlike [`Zip`], this iterator ensures that both input iterators have exactly the same length.
/// If one iterator is exhausted while the other still has elements, the iterator will panic.
///
/// [`Iterator::zip_eq`]: core::iter::Iterator::zip_eq
/// [`Zip`]: crate::iter::Zip
#[derive(Drop, Clone)]
#[must_use]
pub struct ZipEq<A, B> {
    a: A,
    b: B,
}

#[inline]
pub fn zip_eq<A, B>(a: A, b: B) -> ZipEq<A, B> {
    ZipEq { a, b }
}

impl ZipEqIterator<
    A,
    B,
    impl IterA: Iterator<A>,
    impl IterB: Iterator<B>,
    +Destruct<A>,
    +Destruct<B>,
    +Destruct<IterA::Item>,
    +Destruct<IterB::Item>,
> of Iterator<ZipEq<A, B>> {
    type Item = (IterA::Item, IterB::Item);

    #[inline]
    fn next(ref self: ZipEq<A, B>) -> Option<Self::Item> {
        let Some(a) = self.a.next() else {
            if let Some(_) = self.b.next() {
                // Note that we unpack 'b' here because we don't have a
                // Destruct<Option<IterB::Item>>.
                panic!("ZipEq: iterators have different lengths");
            }
            return None;
        };
        let Some(b) = self.b.next() else {
            panic!("ZipEq: iterators have different lengths");
        };
        Some((a, b))
    }
}
