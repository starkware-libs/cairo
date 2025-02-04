/// An iterator that yields the current count and the element during iteration.
///
/// This `struct` is created by the [`enumerate`] method on [`Iterator`]. See its
/// documentation for more.
///
/// [`enumerate`]: Iterator::enumerate
/// [`Iterator`]: core::iter::Iterator
#[must_use]
#[derive(Drop, Clone, Debug)]
pub struct Enumerate<I> {
    iter: I,
    count: usize,
}

pub fn enumerated_iterator<I>(iter: I) -> Enumerate<I> {
    Enumerate { iter, count: 0 }
}

impl EnumerateIterator<
    I, T, +Iterator<I>[Item: T], +Destruct<I>, +Destruct<T>,
> of Iterator<Enumerate<I>> {
    type Item = (usize, T);

    /// # Overflow Behavior
    ///
    /// The method does no guarding against overflows, so enumerating more than
    /// `Bounded::<usize>::MAX` elements will always panic.
    ///
    /// [`Bounded`]: core::num::traits::Bounded
    ///
    /// # Panics
    ///
    /// Will panic if the index of the element overflows a `usize`.
    #[inline]
    fn next(ref self: Enumerate<I>) -> Option<Self::Item> {
        let a = self.iter.next()?;
        let i = self.count;
        self.count += 1;
        Some((i, a))
    }
}
