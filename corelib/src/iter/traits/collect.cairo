/// Conversion into an [`Iterator`].
///
/// By implementing `IntoIterator` for a type, you define how it will be
/// converted to an iterator. This is common for types which describe a
/// collection of some kind.
///
/// One benefit of implementing `IntoIterator` is that your type will work
/// with Cairo's `for` loop syntax.
///
/// # Examples
///
/// Basic usage:
///
/// ```
/// let mut iter = array![1, 2, 3].into_iter();
///
/// assert!(Option::Some(1) == iter.next());
/// assert!(Option::Some(2) == iter.next());
/// assert!(Option::Some(3) == iter.next());
/// assert!(Option::None == iter.next());
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
///     type Item = u32;
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
    /// The type of the elements being iterated over.
    type Item;

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
    /// assert_eq!(Option::Some(1), iter.next());
    /// assert_eq!(Option::Some(2), iter.next());
    /// assert_eq!(Option::Some(3), iter.next());
    /// assert_eq!(Option::None, iter.next());
    /// ```
    fn into_iter(self: T) -> Self::IntoIter;
}

impl IteratorIntoIterator<T, impl IterT: Iterator<T>> of IntoIterator<T> {
    type Item = IterT::Item;
    type IntoIter = T;
    fn into_iter(self: T) -> T {
        self
    }
}

impl SnapshotFixedSizeArrayIntoIterator<
    T, const SIZE: usize, +Drop<T>, impl ToSpan: core::array::ToSpanTrait<[T; SIZE], T>,
> of IntoIterator<@[T; SIZE]> {
    type Item = @T;
    type IntoIter = crate::array::SpanIter<T>;
    fn into_iter(self: @[T; SIZE]) -> Self::IntoIter {
        ToSpan::span(self).into_iter()
    }
}
