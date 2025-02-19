use core::metaprogramming::TypeEqual;
use crate::iter::adapters::{
    Enumerate, Filter, Map, Peekable, Take, Zip, enumerated_iterator, filter_iterator,
    mapped_iterator, peekable_iterator, take_iterator, zipped_iterator,
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
    /// assert_eq!(iter.next(), Some((0, 'a')));
    /// assert_eq!(iter.next(), Some((1, 'b')));
    /// assert_eq!(iter.next(), Some((2, 'c')));
    /// assert_eq!(iter.next(), None);
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
    /// # Note to Implementers
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
            None => init,
            Some(x) => Self::fold(ref self, f(init, x), f),
        }
    }

    /// Searches for an element of an iterator that satisfies a predicate.
    ///
    /// `find()` takes a closure that returns `true` or `false`. It applies
    /// this closure to each element of the iterator as a snapshot, and if
    /// any of them return `true`, then `find()` returns [`Some(element)`].
    /// If they all return `false`, it returns [`None`].
    ///
    /// `find()` is short-circuiting; in other words, it will stop processing
    /// as soon as the closure returns `true`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut iter = array![1, 2, 3].into_iter();
    ///
    /// assert_eq!(iter.find(|x| *x == 2), Option::Some(2));
    ///
    /// assert_eq!(iter.find(|x| *x == 5), Option::None);
    /// ```
    ///
    /// Stopping at the first `true`:
    ///
    /// ```
    /// let mut iter = array![1, 2, 3].into_iter();
    ///
    /// assert_eq!(iter.find(|x| *x == 2), Option::Some(2));
    ///
    /// // we can still use `iter`, as there are more elements.
    /// assert_eq!(iter.next(), Option::Some(3));
    /// ```
    ///
    /// Note that `iter.find(f)` is equivalent to `iter.filter(f).next()`.
    fn find<
        P,
        +core::ops::Fn<P, (@Self::Item,)>[Output: bool],
        +Destruct<P>,
        +Destruct<T>,
        +Destruct<Self::Item>,
    >(
        ref self: T, predicate: P,
    ) -> Option<
        Self::Item,
    > {
        match Self::next(ref self) {
            Option::None => Option::None,
            Option::Some(x) => if predicate(@x) {
                Option::Some(x)
            } else {
                Self::find(ref self, predicate)
            },
        }
    }

    /// Creates an iterator which uses a closure to determine if an element
    /// should be yielded. The closure takes each element as a snapshot.
    ///
    /// Given an element the closure must return `true` or `false`. The returned
    /// iterator will yield only the elements for which the closure returns
    /// `true`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let a = array![0_u32, 1, 2];
    ///
    /// let mut iter = a.into_iter().filter(|x| *x > 0);
    ///
    /// assert_eq!(iter.next(), Option::Some(1));
    /// assert_eq!(iter.next(), Option::Some(2));
    /// assert_eq!(iter.next(), Option::None);
    /// ```
    ///
    /// Note that `iter.filter(f).next()` is equivalent to `iter.find(f)`.
    #[inline]
    fn filter<
        P,
        +core::ops::Fn<P, (@Self::Item,)>[Output: bool],
        +Destruct<P>,
        +Destruct<T>,
        +Destruct<Self::Item>,
    >(
        self: T, predicate: P,
    ) -> Filter<T, P> {
        filter_iterator(self, predicate)
    }

    /// 'Zips up' two iterators into a single iterator of pairs.
    ///
    /// `zip()` returns a new iterator that will iterate over two other
    /// iterators, returning a tuple where the first element comes from the
    /// first iterator, and the second element comes from the second iterator.
    ///
    /// In other words, it zips two iterators together, into a single one.
    ///
    /// If either iterator returns [`None`], [`next`] from the zipped iterator
    /// will return [`None`].
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
    /// assert_eq!(iter.next(), Some((1, 4)));
    /// assert_eq!(iter.next(), Some((2, 5)));
    /// assert_eq!(iter.next(), Some((3, 6)));
    /// assert_eq!(iter.next(), None);
    /// ```
    ///
    /// Since the argument to `zip()` uses [`IntoIterator`], we can pass
    /// anything that can be converted into an [`Iterator`], not just an
    /// [`Iterator`] itself. For example:
    ///
    /// ```
    /// let mut iter = array![1, 2, 3].into_iter().zip(array![4, 5, 6]);
    ///
    /// assert_eq!(iter.next(), Some((1, 4)));
    /// assert_eq!(iter.next(), Some((2, 5)));
    /// assert_eq!(iter.next(), Some((3, 6)));
    /// assert_eq!(iter.next(), None);
    /// ```
    ///
    /// [`enumerate`]: Iterator::enumerate
    /// [`next`]: Iterator::next
    #[inline]
    fn zip<U, impl UIntoIter: IntoIterator<U>, +Destruct<T>>(
        self: T, other: U,
    ) -> Zip<T, UIntoIter::IntoIter> {
        zipped_iterator(self, other.into_iter())
    }

    /// Transforms an iterator into a collection.
    ///
    /// `collect()` can take anything iterable, and turn it into a relevant
    /// collection. This is one of the more powerful methods in the core
    /// library, used in a variety of contexts.
    ///
    /// The most basic pattern in which `collect()` is used is to turn one
    /// collection into another. You take a collection, call [`iter`] on it,
    /// do a bunch of transformations, and then `collect()` at the end.
    ///
    /// `collect()` can also create instances of types that are not typical
    /// collections.
    ///
    /// Because `collect()` is so general, it can cause problems with type
    /// inference. As such, `collect()` is one of the few times you'll see
    /// the syntax affectionately known as the 'turbofish': `::<>`. This
    /// helps the inference algorithm understand specifically which collection
    /// you're trying to collect into.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let doubled: Array<u32> = array![1, 2, 3].into_iter().map(|x| x * 2).collect();
    ///
    /// assert_eq!(array![2, 4, 6], doubled);
    /// ```
    ///
    /// Note that we needed the `: Array<u32>` on the left-hand side.
    ///
    /// Using the 'turbofish' instead of annotating `doubled`:
    ///
    /// ```
    /// let doubled = array![1, 2, 3].into_iter().map(|x| x * 2).collect::<Array<u32>>();
    ///
    /// assert_eq!(array![2, 4, 6], doubled);
    /// ```
    ///
    /// Because `collect()` only cares about what you're collecting into, you can
    /// still use a partial type hint, `_`, with the turbofish:
    ///
    /// ```
    /// let doubled = array![1, 2, 3].into_iter().map(|x| x * 2).collect::<Array<_>>();
    ///
    /// assert_eq!(array![2, 4, 6], doubled);
    /// ```
    #[inline]
    #[must_use]
    fn collect<
        B,
        impl IntoIter: IntoIterator<T>,
        impl ItemEqual: TypeEqual<IntoIter::Iterator::Item, Self::Item>,
        +Destruct<IntoIter::IntoIter>,
        +FromIterator<B, Self::Item>,
        +Destruct<T>,
    >(
        self: T,
    ) -> B {
        FromIterator::<B, Self::Item>::from_iter::<T, IntoIter, ItemEqual>(self)
    }

    /// Creates an iterator which can use the [`peek`] method to look at the next element of the
    /// iterator. See its documentation for more information.
    ///
    /// Note that the underlying iterator is still advanced when [`peek`] is called for the first
    /// time: In order to retrieve the next element, [`next`] is called on the underlying iterator,
    /// hence any side effects (i.e. anything other than fetching the next value) of the [`next`]
    /// method will occur.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut iter = (1..4_u8).into_iter().peekable();
    ///
    /// // peek() lets us see one step into the future
    /// assert_eq!(iter.peek(), Option::Some(1));
    /// assert_eq!(iter.next(), Option::Some(1));
    ///
    /// assert_eq!(iter.next(), Option::Some(2));
    ///
    /// // we can peek() multiple times, the iterator won't advance
    /// assert_eq!(iter.peek(), Option::Some(3));
    /// assert_eq!(iter.peek(), Option::Some(3));
    ///
    /// assert_eq!(iter.next(), Option::Some(3));
    ///
    /// // after the iterator is finished, so is peek()
    /// assert_eq!(iter.peek(), Option::None);
    /// assert_eq!(iter.next(), Option::None);
    /// ```
    #[inline]
    #[must_use]
    fn peekable(self: T) -> Peekable<T, Self::Item> {
        peekable_iterator(self)
    }

    /// Creates an iterator that yields the first `n` elements, or fewer
    /// if the underlying iterator ends sooner.
    ///
    /// `take(n)` yields elements until `n` elements are yielded or the end of
    /// the iterator is reached (whichever happens first).
    /// The returned iterator is a prefix of length `n` if the original iterator
    /// contains at least `n` elements, otherwise it contains all of the
    /// (fewer than `n`) elements of the original iterator.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut iter = array![1, 2, 3].into_iter().take(2);
    ///
    /// assert_eq!(iter.next(), Some(1));
    /// assert_eq!(iter.next(), Some(2));
    /// assert_eq!(iter.next(), None);
    /// ```
    ///
    /// If less than `n` elements are available,
    /// `take` will limit itself to the size of the underlying iterator:
    ///
    /// ```
    /// let mut iter = array![1, 2].into_iter().take(5);
    /// assert_eq!(iter.next(), Some(1));
    /// assert_eq!(iter.next(), Some(2));
    /// assert_eq!(iter.next(), None);
    /// ```
    #[inline]
    #[must_use]
    fn take(self: T, n: usize) -> Take<T> {
        take_iterator(self, n)
    }

    /// Sums the elements of an iterator.
    ///
    /// Takes each element, adds them together, and returns the result.
    ///
    /// An empty iterator returns the zero value of the type.
    ///
    /// `sum()` can be used to sum any type implementing [`Sum`][`core::iter::Sum`],
    /// including [`Option`][`Option::sum`] and [`Result`][`Result::sum`].
    ///
    /// # Panics
    ///
    /// When calling `sum()` and a primitive integer type is being returned, this
    /// method will panic if the computation overflows.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut iter = array![1, 2, 3].into_iter();
    /// let sum: usize = iter.sum();
    ///
    /// assert_eq!(sum, 6);
    /// ```
    fn sum<+Destruct<T>, +Destruct<Self::Item>, +Sum<Self::Item>>(
        self: T,
    ) -> Self::Item {
        Sum::<Self::Item>::sum::<T, Self>(self)
    }

    /// Iterates over the entire iterator, multiplying all the elements
    ///
    /// An empty iterator returns the one value of the type.
    ///
    /// # Panics
    ///
    /// When calling `product()` and a primitive integer type is being returned, this
    /// method will panic if the computation overflows.
    ///
    /// # Examples
    ///
    /// ```
    /// fn factorial(n: u32) -> u32 {
    ///     (1..=n).into_iter().product()
    /// }
    /// assert_eq!(factorial(0), 1);
    /// assert_eq!(factorial(1), 1);
    /// assert_eq!(factorial(5), 120);
    /// ```
    fn product<+Destruct<T>, +Destruct<Self::Item>, +Product<Self::Item>>(
        self: T,
    ) -> Self::Item {
        Product::<Self::Item>::product::<T, Self>(self)
    }
}
