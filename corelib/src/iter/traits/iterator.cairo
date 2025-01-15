use crate::iter::adapters::{Map, mapped_iterator};

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
    fn advance_by<+Destruct<T>, +Destruct<Self::Item>>(
        ref self: T, n: usize,
    ) -> Result<
        (), NonZero<usize>,
    > {
        if let Option::Some(nz_n) = n.try_into() {
            if let Option::Some(_) = Self::next(ref self) {
                return Self::advance_by(ref self, n - 1);
            } else {
                Result::Err(nz_n)
            }
        } else {
            Result::Ok(())
        }
    }

    /// Takes a closure and creates an iterator which calls that closure on each
    /// element.
    ///
    /// `map()` transforms one iterator into another, by means of its argument:
    /// something that implements [`FnOnce`]. It produces a new iterator which
    /// calls this closure on each element of the original iterator.
    ///
    /// If you are good at thinking in types, you can think of `map()` like this:
    /// If you have an iterator that gives you elements of some type `A`, and
    /// you want an iterator of some other type `B`, you can use `map()`,
    /// passing a closure that takes an `A` and returns a `B`.
    ///
    /// `map()` is conceptually similar to a `for` loop. However, as `map()` is
    /// lazy, it is best used when you're already working with other iterators.
    /// If you're doing some sort of looping for a side effect, it's considered
    /// more idiomatic to use `for` than `map()`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut iter = array![1, 2, 3].into_iter().map(|x| 2 * x);
    ///
    /// assert!(iter.next() == Option::Some(2));
    /// assert!(iter.next() == Option::Some(4));
    /// assert!(iter.next() == Option::Some(6));
    /// assert!(iter.next() == Option::None);
    /// ```
    ///
    /// If you're doing some sort of side effect, prefer `for` to `map()`:
    ///
    /// ```
    /// // don't do this:
    /// let _ = (0..5_usize).into_iter().map(|x| println!("{x}"));
    ///
    /// // it won't even execute, as it is lazy. Cairo will warn you about this if not specifically
    /// ignored, as is done here.
    ///
    /// // Instead, use for:
    /// for x in 0..5_usize {
    ///     println!("{x}");
    /// }
    /// ```
    #[inline]
    fn map<
        B, F, impl TIter: Self, +core::ops::Fn<F, (TIter::Item,)>[Output: B], +Drop<T>, +Drop<F>,
    >(
        self: T, f: F,
    ) -> Map<T, F> {
        mapped_iterator(self, f)
    }
}
