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
    /// The underlying iterator.
    iter: I,
    /// Caches the next value of the iterator when `peek()` is called,
    /// consuming the iterator only once even when several consecutive calls
    /// to `peek()` are made.
    ///
    /// - `None`: Indicates that no peek has been performed.
    /// - `Some(None)`: Indicates that a peek has been performed but the iterator is exhausted.
    /// - `Some(Some(R))`: Contains the next element in the iterator, ready to be returned by
    /// `peek()`.
    ///
    /// `peeked` is reset to `None` when `next()` is called.
    peeked: Option<Option<R>>,
}

pub fn peekable_iterator<I, R>(iter: I) -> Peekable<I, R> {
    Peekable { iter, peeked: Option::None }
}

impl PeekableIterator<
    I, impl IterI: Iterator<I>, +Destruct<I>, +Destruct<IterI::Item>,
> of Iterator<Peekable<I, IterI::Item>> {
    type Item = IterI::Item;

    fn next(ref self: Peekable<I, IterI::Item>) -> Option<Self::Item> {
        let next_value = match self.peeked {
            Option::Some(v) => v,
            Option::None => self.iter.next(),
        };
        self.peeked = Option::None;
        next_value
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
    /// let mut iter = (1..4_u8).into_iter().peekable();
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
