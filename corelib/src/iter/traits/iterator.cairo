use crate::iter::adapters::{
    Enumerate, Map, Zip, enumerated_iterator, mapped_iterator, zipped_iterator,
};

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

    /// Creates an iterator which gives the current iteration count as well as
    /// the next value.
    ///
    /// The iterator returned yields pairs `(i, val)`, where `i` is the
    /// current index of iteration and `val` is the value returned by the
    /// iterator.
    ///
    /// `enumerate()` keeps its count as a [`usize`].
    ///
    /// # Overflow Behavior
    ///
    /// The method does no guarding against overflows, so enumerating more than
    /// `Bounded::<usize>::MAX` elements will always panic.
    ///
    /// [`Bounded`]: core::num::traits::Bounded
    ///
    /// # Panics
    ///
    /// Will panic if the to-be-returned index overflows a `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut iter = array!['a', 'b', 'c'].into_iter().enumerate();
    ///
    /// assert_eq!(iter.next(), Option::Some((0, 'a')));
    /// assert_eq!(iter.next(), Option::Some((1, 'b')));
    /// assert_eq!(iter.next(), Option::Some((2, 'c')));
    /// assert_eq!(iter.next(), Option::None);
    /// ```
    #[inline]
    fn enumerate(self: T) -> Enumerate<T> {
        enumerated_iterator(self)
    }

    /// Folds every element into an accumulator by applying an operation,
    /// returning the final result.
    ///
    /// `fold()` takes two arguments: an initial value, and a closure with two
    /// arguments: an 'accumulator', and an element. The closure returns the value that
    /// the accumulator should have for the next iteration.
    ///
    /// The initial value is the value the accumulator will have on the first
    /// call.
    ///
    /// After applying this closure to every element of the iterator, `fold()`
    /// returns the accumulator.
    ///
    /// Folding is useful whenever you have a collection of something, and want
    /// to produce a single value from it.
    ///
    /// Note: `fold()`, and similar methods that traverse the entire iterator,
    /// might not terminate for infinite iterators, even on traits for which a
    /// result is determinable in finite time.
    ///
    /// Note: `fold()` combines elements in a *left-associative* fashion. For associative
    /// operators like `+`, the order the elements are combined in is not important, but for
    /// non-associative operators like `-` the order will affect the final result.
    ///
    /// # Note to Implementors
    ///
    /// Several of the other (forward) methods have default implementations in
    /// terms of this one, so try to implement this explicitly if it can
    /// do something better than the default `for` loop implementation.
    ///
    /// In particular, try to have this call `fold()` on the internal parts
    /// from which this iterator is composed.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut iter = array![1, 2, 3].into_iter();
    ///
    /// // the sum of all of the elements of the array
    /// let sum = iter.fold(0, |acc, x| acc + x);
    ///
    /// assert_eq!(sum, 6);
    /// ```
    ///
    /// Let's walk through each step of the iteration here:
    ///
    /// | element | acc | x | result |
    /// |---------|-----|---|--------|
    /// |         | 0   |   |        |
    /// | 1       | 0   | 1 | 1      |
    /// | 2       | 1   | 2 | 3      |
    /// | 3       | 3   | 3 | 6      |
    ///
    /// And so, our final result, `6`.
    ///
    /// It's common for people who haven't used iterators a lot to
    /// use a `for` loop with a list of things to build up a result. Those
    /// can be turned into `fold()`s:
    ///
    /// ```
    /// let mut numbers = array![1, 2, 3, 4, 5].span();
    ///
    /// let mut result = 0;
    ///
    /// // for loop:
    /// for i in numbers{
    ///     result = result + (*i);
    /// };
    ///
    /// // fold:
    /// let mut numbers_iter = numbers.into_iter();
    /// let result2 = numbers_iter.fold(0, |acc, x| acc + (*x));
    ///
    /// // they're the same
    /// assert_eq!(result, result2);
    /// ```
    fn fold<
        B,
        F,
        +core::ops::Fn<F, (B, Self::Item)>[Output: B],
        +Destruct<T>,
        +Destruct<F>,
        +Destruct<B>,
    >(
        ref self: T, init: B, f: F,
    ) -> B {
        match Self::next(ref self) {
            Option::None => init,
            Option::Some(x) => Self::fold(ref self, f(init, x), f),
        }
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
    /// let mut iter = array![1, 2, 3].into_iter().zip(array![4, 5, 6].into_iter());
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
    /// let mut iter = array![1, 2, 3].into_iter().zip(array![4, 5, 6]);
    ///
    /// assert_eq!(iter.next(), Option::Some((1, 4)));
    /// assert_eq!(iter.next(), Option::Some((2, 5)));
    /// assert_eq!(iter.next(), Option::Some((3, 6)));
    /// assert_eq!(iter.next(), Option::None);
    /// ``
    ///
    /// [`enumerate`]: Iterator::enumerate
    /// [`next`]: Iterator::next
    #[inline]
    fn zip<U, impl UIntoIter: IntoIterator<U>, +Destruct<T>>(
        self: T, other: U,
    ) -> Zip<T, UIntoIter::IntoIter> {
        zipped_iterator(self, other.into_iter())
    }
}
