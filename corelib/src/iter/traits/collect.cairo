use crate::metaprogramming::TypeEqual;

/// Conversion from an [`Iterator`].
///
/// By implementing `FromIterator` for a type, you define how it will be
/// created from an iterator. This is common for types which describe a
/// collection of some kind.
///
/// If you want to create a collection from the contents of an iterator, the
/// [`Iterator::collect()`] method is preferred. However, when you need to
/// specify the container type, [`FromIterator::from_iter()`] can be more
/// readable than using a turbofish (e.g. `::<Array<_>>()`). See the
/// [`Iterator::collect()`] documentation for more examples of its use.
///
/// See also: [`IntoIterator`].
///
/// # Examples
///
/// Basic usage:
///
/// ```
/// let v = FromIterator::from_iter(0..5_u32);
///
/// assert_eq!(v, array![0, 1, 2, 3, 4]);
/// ```
///
/// Implementing `FromIterator` for your type:
///
/// ```
/// use core::metaprogramming::TypeEqual;
///
/// // A sample collection, that's just a wrapper over Array<T>
/// #[derive(Drop, Debug)]
/// struct MyCollection {
///     arr: Array<u32>,
/// }
///
/// // Let's give it some methods so we can create one and add things
/// // to it.
/// #[generate_trait]
/// impl MyCollectionImpl of MyCollectionTrait {
///     fn new() -> MyCollection {
///         MyCollection { arr: ArrayTrait::new() }
///     }
///
///     fn add(ref self: MyCollection, elem: u32) {
///         self.arr.append(elem);
///     }
/// }
///
/// // and we'll implement FromIterator
/// impl MyCollectionFromIterator of FromIterator<MyCollection, u32> {
///     fn from_iter<
///             I,
///             impl IntoIter: IntoIterator<I>,
///             +TypeEqual<IntoIter::Iterator::Item, u32>,
///             +Destruct<IntoIter::IntoIter>,
///             +Destruct<I>,
///         >(
///             iter: I
///         ) -> MyCollection {
///         let mut c = MyCollectionTrait::new();
///         for i in iter {
///             c.add(i);
///         };
///         c
///     }
/// }
///
/// // Now we can make a new iterator...
/// let iter = (0..5_u32).into_iter();
///
/// // ... and make a MyCollection out of it
/// let c = FromIterator::<MyCollection>::from_iter(iter);
///
/// assert_eq!(c.arr, array![0, 1, 2, 3, 4]);
/// ```
pub trait FromIterator<T, A> {
    /// Creates a value from an iterator.
    ///
    /// See the [module-level documentation] for more.
    ///
    /// [module-level documentation]: crate::iter
    ///
    /// # Examples
    ///
    /// ```
    /// let iter = (0..5_u32).into_iter();
    ///
    /// let v = FromIterator::from_iter(iter);
    ///
    /// assert_eq!(v, array![0, 1, 2, 3, 4]);
    /// ```
    fn from_iter<
        I,
        impl IntoIter: IntoIterator<I>,
        +TypeEqual<IntoIter::Iterator::Item, A>,
        +Destruct<IntoIter::IntoIter>,
        +Destruct<I>,
    >(
        iter: I,
    ) -> T;
}

/// Conversion into an [`Iterator`].
///
/// By implementing `IntoIterator` for a type, you define how it will be
/// converted to an iterator. This is common for types which describe a
/// collection of some kind.
///
/// One benefit of implementing `IntoIterator` is that your type will work
/// with Cairo's `for` loop syntax.
///
/// See also: [`FromIterator`].
///
///
/// # Examples
///
/// Basic usage:
///
/// ```
/// let mut iter = array![1, 2, 3].into_iter();
///
/// assert!(Some(1) == iter.next());
/// assert!(Some(2) == iter.next());
/// assert!(Some(3) == iter.next());
/// assert!(None == iter.next());
/// ```
///
/// Implementing `IntoIterator` for your type:
///
/// ```
/// // A sample collection, that's just a wrapper over `Array<u32>`
/// #[derive(Drop, Debug)]
/// struct MyCollection {
///     arr: Array<u32>
/// }
///
/// // Let's give it some methods so we can create one and add things
/// // to it.
/// #[generate_trait]
/// impl MyCollectionImpl of MyCollectionTrait {
///     fn new() -> MyCollection {
///         MyCollection {
///             arr: ArrayTrait::new()
///         }
///     }
///
///     fn add(ref self: MyCollection, elem: u32) {
///         self.arr.append(elem);
///     }
/// }
///
/// // and we'll implement `IntoIterator`
/// impl MyCollectionIntoIterator of IntoIterator<MyCollection> {
///     type IntoIter = core::array::ArrayIter<u32>;
///     fn into_iter(self: MyCollection) -> Self::IntoIter {
///         self.arr.into_iter()
///     }
/// }
///
/// // Now we can make a new collection...
/// let mut c = MyCollectionTrait::new();
///
/// // ... add some stuff to it ...
/// c.add(0);
/// c.add(1);
/// c.add(2);
///
/// // ... and then turn it into an `Iterator`:
/// let mut n = 0;
/// for i in c {
///     assert!(i == n);
///     n += 1;
/// };
/// ```
pub trait IntoIterator<T> {
    /// The iterator type that will be created.
    type IntoIter;
    impl Iterator: Iterator<Self::IntoIter>;

    /// Creates an iterator from a value.
    ///
    /// See the [module-level documentation] for more.
    ///
    /// [module-level documentation]: crate::iter
    ///
    /// # Examples
    ///
    /// ```
    /// let mut iter = array![1, 2, 3].into_iter();
    ///
    /// assert_eq!(Some(1), iter.next());
    /// assert_eq!(Some(2), iter.next());
    /// assert_eq!(Some(3), iter.next());
    /// assert_eq!(None, iter.next());
    /// ```
    fn into_iter(self: T) -> Self::IntoIter;
}

impl IteratorIntoIterator<T, +Iterator<T>> of IntoIterator<T> {
    type IntoIter = T;
    fn into_iter(self: T) -> T {
        self
    }
}

impl SnapshotFixedSizeArrayIntoIterator<
    T, const SIZE: usize, +Drop<T>, impl ToSpan: crate::array::ToSpanTrait<[T; SIZE], T>,
> of IntoIterator<@[T; SIZE]> {
    type IntoIter = crate::array::SpanIter<T>;
    fn into_iter(self: @[T; SIZE]) -> Self::IntoIter {
        ToSpan::span(self).into_iter()
    }
}

/// Extend a collection with the contents of an iterator.
///
/// Iterators produce a series of values, and collections can also be thought
/// of as a series of values. The `Extend` trait bridges this gap, allowing you
/// to extend a collection by including the contents of that iterator. When
/// extending a collection with an already existing key, that entry is updated
/// or, in the case of collections that permit multiple entries with equal
/// keys, that entry is inserted.
///
/// # Examples
///
/// Basic usage:
///
/// ```
/// let mut arr = array![1, 2];
///
/// arr.extend(array![3, 4, 5]);
///
/// assert_eq!(arr, array![1, 2, 3, 4, 5]);
/// ```
pub trait Extend<T, A> {
    /// Extends a collection with the contents of an iterator.
    fn extend<
        I,
        impl IntoIter: IntoIterator<I>,
        +TypeEqual<IntoIter::Iterator::Item, A>,
        +Destruct<IntoIter::IntoIter>,
        +Destruct<I>,
    >(
        ref self: T, iter: I,
    );
}

