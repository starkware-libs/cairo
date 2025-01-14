use crate::iter::adapters::{Map, Zip, mapped_iterator, zipped_iterator};

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
    /// let a = array![1, 2, 3];
    ///
    /// let mut iter = a.into_iter().map(|x| 2 * x);
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

    /// 'Zips up' two iterators into a single iterator of pairs.
    ///
    /// `zip()` returns a new iterator that will iterate over two other
    /// iterators, returning a tuple where the first element comes from the
    /// first iterator, and the second element comes from the second iterator.
    ///
    /// In other words, it zips two iterators together, into a single one.
    ///
    /// If either iterator returns [`Option::None`], [`next`] from the zipped iterator
    /// will return [`Option::None`].
    /// If the zipped iterator has no more elements to return then each further attempt to advance
    /// it will first try to advance the first iterator at most one time and if it still yielded an
    /// item try to advance the second iterator at most one time.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let a1 = array![1, 2, 3];
    /// let a2 = array![4, 5, 6];
    ///
    /// let mut iter = a1.into_iter().zip(a2.into_iter());
    ///
    /// assert_eq!(iter.next(), Option::Some((1, 4)));
    /// assert_eq!(iter.next(), Option::Some((2, 5)));
    /// assert_eq!(iter.next(), Option::Some((3, 6)));
    /// assert_eq!(iter.next(), Option::None);
    /// ```
    ///
    /// Since the argument to `zip()` uses [`IntoIterator`], we can pass
    /// anything that can be converted into an [`Iterator`], not just an
    /// [`Iterator`] itself. For example:
    ///
    /// ```
    /// let a1 = array![1, 2, 3];
    /// let a2 = array![4, 5, 6];
    ///
    /// let mut iter = a1.into_iter().zip(a2);
    ///
    /// assert_eq!(iter.next(), Option::Some((1, 4)));
    /// assert_eq!(iter.next(), Option::Some((2, 5)));
    /// assert_eq!(iter.next(), Option::Some((3, 6)));
    /// assert_eq!(iter.next(), Option::None);
    /// ```
    ///
    /// If both iterators have roughly equivalent syntax, it may be more readable to use [`zip`]:
    ///
    /// ```
    /// use core::iter::zip;
    ///
    /// let a = array![1, 2, 3];
    /// let b = array![2, 3, 4];
    ///
    /// let mut zipped = zip(a, b);
    ///
    /// assert_eq!(iter.next(), Option::Some((1, 4)));
    /// assert_eq!(iter.next(), Option::Some((2, 5)));
    /// assert_eq!(iter.next(), Option::Some((3, 6)));
    /// assert_eq!(iter.next(), Option::None);
    /// );
    /// ```
    ///
    /// [`enumerate`]: Iterator::enumerate
    /// [`next`]: Iterator::next
    /// [`zip`]: core::iter::zip
    #[inline]
    fn zip<U, +Iterator<U> //, +IntoIterator<U>
    >(
        self: T, other: U,
    ) -> Zip<T, U> {
        zipped_iterator(self, other //.into_iter()
        )
    }
}
