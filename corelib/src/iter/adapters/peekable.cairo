/// An iterator with a `peek()` that returns a copy to the next
/// element.
///
/// This `struct` is created by the [`peekable`] method on [`Iterator`]. See its
/// documentation for more.
///
/// [`peekable`]: Iterator::peekable
#[must_use]
#[derive(Drop, Debug)]
pub struct Peekable<I, R> {
    iter: I,
    /// Remember a peeked value, even if it was None.
    peeked: Option<Option<R>>,
}

pub fn peekable_iterator<I, impl IterI: Iterator<I>>(iter: I) -> Peekable<I, IterI::Item> {
    Peekable { iter, peeked: Option::None }
}

impl PeekableIterator<
    I, impl IterI: Iterator<I>, +Drop<I>, +Drop<IterI::Item>,
> of Iterator<Peekable<I, IterI::Item>> {
    type Item = IterI::Item;

    fn next(ref self: Peekable<I, IterI::Item>) -> Option<Self::Item> {
        // `take()` makes sure that if a value was already peeked, `peeked` will be reset to None
        match self.peeked.take() {
            Option::Some(v) => v,
            Option::None => self.iter.next(),
        }
    }
}

#[generate_trait]
pub impl PeekableTraitImpl<
    I, impl IterI: Iterator<I>, +Copy<IterI::Item>, +Drop<IterI::Item>,
> of PeekableTrait<I> {
    /// Returns a copy to the next() value without advancing the iterator.
    ///
    /// Like [`next`], if there is a value, it is wrapped in a `Some(T)`.
    /// But if the iteration is over, `None` is returned.
    ///
    /// [`next`]: Iterator::next
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let xs = array![1, 2, 3];
    ///
    /// let mut iter = xs.into_iter().peekable();
    ///
    /// // peek() lets us see one step into the future
    /// assert_eq!(iter.peek(), Some(1));
    /// assert_eq!(iter.next(), Some(1));
    ///
    /// assert_eq!(iter.next(), Some(2));
    ///
    /// // The iterator does not advance even if we `peek` multiple times
    /// assert_eq!(iter.peek(), Some(3));
    /// assert_eq!(iter.peek(), Some(3));
    ///
    /// assert_eq!(iter.next(), Some(3));
    ///
    /// // After the iterator is finished, so is `peek()`
    /// assert_eq!(iter.peek(), None);
    /// assert_eq!(iter.next(), None);
    /// ```
    #[inline]
    fn peek(ref self: Peekable<I, IterI::Item>) -> Option<IterI::Item> {
        match self.peeked {
            Option::Some(value) => value,
            Option::None => {
                let next_value = self.iter.next();
                self.peeked = Option::Some(next_value);
                next_value
            },
        }
    }
}
