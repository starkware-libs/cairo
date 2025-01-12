/// A trait for dealing with iterators.
///
/// This is the main iterator trait. For more about the concept of iterators
/// generally, please see the [module-level documentation]. In particular, you
/// may want to know how to [implement `Iterator`][impl].
///
/// [module-level documentation]: crate::iter
/// [impl]: crate::iter#implementing-iterator
pub trait Iterator<T> {
    /// The type of the elements being iterated over.
    type Item;

    /// Advances the iterator and returns the next value.
    ///
    /// Returns [`None`] when iteration is finished. Individual iterator
    /// implementations may choose to resume iteration, and so calling `next()`
    /// again may or may not eventually start returning [`Some(Item)`] again at some
    /// point.
    ///
    /// [`Some(Item)`]: Option::Some
    /// [`None`]: Option::None
    ///
    /// # Examples
    ///
    /// ```
    /// let mut iter = [1, 2, 3].span().into_iter();
    ///
    /// // A call to next() returns the next value...
    /// assert_eq!(Option::Some(@1), iter.next());
    /// assert_eq!(Option::Some(@2), iter.next());
    /// assert_eq!(Option::Some(@3), iter.next());
    ///
    /// // ... and then None once it's over.
    /// assert_eq!(Option::None, iter.next());
    ///
    /// // More calls may or may not return `None`. Here, they always will.
    /// assert_eq!(Option::None, iter.next());
    /// assert_eq!(Option::None, iter.next());
    /// ```
    fn next(ref self: T) -> Option<Self::Item>;

    /// Advances the iterator by `n` elements.
    ///
    /// This method will eagerly skip `n` elements by calling [`next`] up to `n`
    /// times until [`None`] is encountered.
    ///
    /// `advance_by(n)` will return `Ok(())` if the iterator successfully advances by
    /// `n` elements, or a `Err(NonZero<usize>)` with value `k` if [`None`] is encountered,
    /// where `k` is remaining number of steps that could not be advanced because the iterator ran
    /// out.
    /// If `self` is empty and `n` is non-zero, then this returns `Err(n)`.
    /// Otherwise, `k` is always less than `n`.
    ///
    /// [`None`]: Option::None
    /// [`next`]: Iterator::next
    ///
    /// # Examples
    ///
    /// ```
    /// let mut iter = array![1_u8, 2, 3, 4].into_iter();
    ///
    /// assert_eq!(iter.advance_by(2), Result::Ok(()));
    /// assert_eq!(iter.next(), Option::Some(3));
    /// assert_eq!(iter.advance_by(0), Result::Ok(()));
    /// assert_eq!(iter.advance_by(100), Result::Err(99));
    /// ```
    fn advance_by<+Destruct<T>, +Drop<Self::Item>>(
        ref self: T, n: usize,
    ) -> Result<
        (), NonZero<usize>,
    > {
        let mut res = Result::Ok(());
        for i in 0..n {
            if Self::next(ref self).is_none() {
                res = Result::Err((n - i).try_into().unwrap());
                break;
            }
        };
        res
    }
}
