use crate::iter::adapters::{
    Enumerate, Filter, Map, Peekable, Zip, enumerated_iterator, filter_iterator, mapped_iterator,
    peekable_iterator, zipped_iterator,
};
use crate::iter::traits::{Product, Sum};

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
    /// [`Some(Item)`]: Some
    /// [`None`]: None
    ///
    /// # Examples
    ///
    /// ```
    /// let mut iter = [1, 2, 3].span().into_iter();
    ///
    /// // A call to next() returns the next value...
    /// assert_eq!(Some(@1), iter.next());
    /// assert_eq!(Some(@2), iter.next());
    /// assert_eq!(Some(@3), iter.next());
    ///
    /// // ... and then None once it's over.
    /// assert_eq!(None, iter.next());
    ///
    /// // More calls may or may not return `None`. Here, they always will.
    /// assert_eq!(None, iter.next());
    /// assert_eq!(None, iter.next());
    /// ```
    fn next(ref self: T) -> Option<Self::Item>;

    /// Consumes the iterator, counting the number of iterations and returning it.
    ///
    /// This method will call [`next`] repeatedly until [`None`] is encountered,
    /// returning the number of times it saw [`Some`]. Note that [`next`] has to be
    /// called at least once even if the iterator does not have any elements.
    ///
    /// [`next`]: Iterator::next
    ///
    /// # Overflow Behavior
    ///
    /// The method does no guarding against overflows, so counting elements of
    /// an iterator with more than [`Bounded::<usize>::MAX`] elements either produces the
    /// wrong result or panics.
    ///
    /// [`Bounded::<usize>::MAX`]: core::num::traits::Bounded
    ///
    /// # Panics
    ///
    /// This function might panic if the iterator has more than [`Bounded::<usize>::MAX`]
    /// elements.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut a = array![1, 2, 3].into_iter();
    /// assert_eq!(a.count(), 3);
    ///
    /// let mut a = array![1, 2, 3, 4, 5].into_iter();
    /// assert_eq!(a.count(), 5);
    /// ```
    #[inline]
    fn count<+Destruct<T>, +Destruct<Self::Item>>(
        self: T,
    ) -> usize {
        let mut self = self;
        Self::fold(ref self, 0_usize, |count, _x| {
            count + 1
        })
    }

    /// Consumes the iterator, returning the last element.
    ///
    /// This method will evaluate the iterator until it returns [`None`]. While
    /// doing so, it keeps track of the current element. After [`None`] is
    /// returned, `last()` will then return the last element it saw.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut a = array![1, 2, 3].into_iter();
    /// assert_eq!(a.last(), Option::Some(3));
    ///
    /// let mut a = array![].into_iter();
    /// assert_eq!(a.last(), Option::None);
    /// ```
    #[inline]
    fn last<+Destruct<T>, +Destruct<Self::Item>>(
        self: T,
    ) -> Option<
        Self::Item,
    > {
        let mut self = self;
        let next = Self::next(ref self)?;
        Option::Some(Self::last(self).unwrap_or(next))
    }

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
    /// [`None`]: None
    /// [`next`]: Iterator::next
    ///
    /// # Examples
    ///
    /// ```
    /// let mut iter = array![1_u8, 2, 3, 4].into_iter();
    ///
    /// assert_eq!(iter.advance_by(2), Ok(()));
    /// assert_eq!(iter.next(), Some(3));
    /// assert_eq!(iter.advance_by(0), Ok(()));
    /// assert_eq!(iter.advance_by(100), Err(99));
    /// ```
    fn advance_by<+Destruct<T>, +Destruct<Self::Item>>(
        ref self: T, n: usize,
    ) -> Result<
        (), NonZero<usize>,
    > {
        if let Some(nz_n) = n.try_into() {
            if let Some(_) = Self::next(ref self) {
                return Self::advance_by(ref self, n - 1);
            } else {
                Err(nz_n)
            }
        } else {
            Ok(())
        }
    }

    /// Returns the `n`th element of the iterator.
    ///
    /// Like most indexing operations, the count starts from zero, so `nth(0)`
    /// returns the first value, `nth(1)` the second, and so on.
    ///
    /// Note that all preceding elements, as well as the returned element, will be
    /// consumed from the iterator. That means that the preceding elements will be
    /// discarded, and also that calling `nth(0)` multiple times on the same iterator
    /// will return different elements.
    ///
    /// `nth()` will return [`None`] if `n` is greater than or equal to the length of the
    /// iterator.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut iter = array![1, 2, 3].into_iter();
    /// assert_eq!(iter.nth(1), Some(2));
    /// ```
    ///
    /// Calling `nth()` multiple times doesn't rewind the iterator:
    ///
    /// ```
    /// let mut iter = array![1, 2, 3].into_iter();
    ///
    /// assert_eq!(iter.nth(1), Some(2));
    /// assert_eq!(iter.nth(1), None);
    /// ```
    ///
    /// Returning `None` if there are less than `n + 1` elements:
    ///
    /// ```
    /// let mut iter = array![1, 2, 3].into_iter();
    /// assert_eq!(iter.nth(10), None);
    /// ```
    #[inline]
    fn nth<+Destruct<T>, +Destruct<Self::Item>>(
        ref self: T, n: usize,
    ) -> Option<
        Self::Item,
    > {
        match Self::advance_by(ref self, n) {
            Result::Ok(_) => Self::next(ref self),
            Result::Err(_) => Option::None,
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
    /// assert!(iter.next() == Some(2));
    /// assert!(iter.next() == Some(4));
    /// assert!(iter.next() == Some(6));
    /// assert!(iter.next() == None);
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
    fn map<B, F, +core::ops::Fn<F, (Self::Item,)>[Output: B], +Drop<T>, +Drop<F>>(
        self: T, f: F,
    ) -> Map<T, F> {
        mapped_iterator(self, f)
    }
}
