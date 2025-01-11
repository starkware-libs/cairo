/// An iterator that iterates two other iterators simultaneously.
#[derive(Drop, Clone)]
#[must_use]
pub struct Zip<A, B> {
    a: A,
    b: B,
}

#[inline]
pub fn zipped_iterator<A, B>(a: A, b: B) -> Zip<A, B> {
    Zip { a, b }
}

/// Converts the arguments to iterators and zips them.
///
/// # Examples
///
/// ```
/// use core::iter::zip;
///
/// let xs = array![1, 2, 3];
/// let ys = array![4, 5, 6];
///
/// let mut iter = zip(xs, ys);
///
/// assert_eq!(iter.next(), Option::Some((1, 4)));
/// assert_eq!(iter.next(), Option::Some((2, 5)));
/// assert_eq!(iter.next(), Option::Some((3, 6)));
/// assert_eq!(iter.next(), Option::None);
///
/// // Nested zips are also possible:
/// let xs = array![1, 2, 3];
/// let ys = array![4, 5, 6];
/// let zs = array![7, 8, 9];
///
/// let mut iter = zip(zip(xs, ys), zs);
///
/// assert_eq!(iter.next(), Option::Some(((1, 4), 7)));
/// assert_eq!(iter.next(), Option::Some(((2, 5), 8)));
/// assert_eq!(iter.next(), Option::Some(((3, 6), 9)));
/// assert_eq!(iter.next(), Option::None);
/// ```
pub fn zip<
    A,
    B,
    impl IntoIterA: IntoIterator<A>,
    impl IntoIterB: IntoIterator<B>,
    +Destruct<A>,
    +Destruct<B>,
    +Destruct<IntoIterA::IntoIter>,
    +Destruct<IntoIterB::IntoIter>,
>(
    a: A, b: B,
) -> Zip<IntoIterA::IntoIter, IntoIterB::IntoIter> {
    zipped_iterator(a.into_iter(), b.into_iter())
}

impl ZipIterator<
    A,
    B,
    IA,
    IB,
    +Iterator<A>[Item: IA],
    +Iterator<B>[Item: IB],
    +Destruct<A>,
    +Destruct<B>,
    +Destruct<IA>,
    +Destruct<IB>,
> of Iterator<Zip<A, B>> {
    type Item = (IA, IB);

    #[inline]
    fn next(ref self: Zip<A, B>) -> Option<Self::Item> {
        let a = self.a.next()?;
        let b = self.b.next()?;
        Option::Some((a, b))
    }
}
