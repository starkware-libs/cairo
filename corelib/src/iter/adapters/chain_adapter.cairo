/// An iterator that links two iterators together, in a chain.
///
/// This `struct` is created by [`chain`]. See the
/// documentation for more.
#[derive(Drop)]
pub struct Chain<A, B> {
    // These are "fused" with `Option` so we don't need separate state to track which part is
    // already exhausted, and we may also get niche layout for `None`.
    //
    // Only the "first" iterator is actually set `None` when exhausted.
    a: Option<A>,
    b: Option<B>,
}

#[inline]
pub fn chained_iterator<A, B>(a: A, b: B) -> Chain<A, B> {
    Chain { a: Option::Some(a), b: Option::Some(b) }
}

/// Converts the arguments to iterators and links them together, in a chain.
///
/// Arguments do not have to be of the same type as long as the underlying iterated
/// over items are.
///
/// # Examples
///
/// ```
/// use core::iter::chain;
///
/// let a: Array<u8> = array![7, 8, 9];
/// let b: Range<u8> = 0..5;
///
/// let mut iter = chain(a, b);
///
/// assert_eq!(iter.next(), Option::Some(7));
/// assert_eq!(iter.next(), Option::Some(8));
/// assert_eq!(iter.next(), Option::Some(9));
/// assert_eq!(iter.next(), Option::Some(0));
/// assert_eq!(iter.next(), Option::Some(1));
/// assert_eq!(iter.next(), Option::Some(2));
/// assert_eq!(iter.next(), Option::Some(3));
/// assert_eq!(iter.next(), Option::Some(4));
/// assert_eq!(iter.next(), Option::None);
/// ```
pub fn chain<
    A,
    B,
    impl IntoIterA: IntoIterator<A>,
    impl IntoIterB: IntoIterator<B>[Item: IntoIterA::Item],
    +Drop<A>,
    +Drop<B>,
    +Drop<IntoIterA::IntoIter>,
>(
    a: A, b: B,
) -> Chain<IntoIterA::IntoIter, IntoIterB::IntoIter> {
    chained_iterator(a.into_iter(), b.into_iter())
}

impl ChainIterator<
    A,
    B,
    impl IterA: Iterator<A>,
    +Iterator<B>[Item: IterA::Item],
    +Drop<A>,
    +Drop<B>,
    +Drop<IterA::Item>,
> of Iterator<Chain<A, B>> {
    type Item = IterA::Item;

    fn next(ref self: Chain<A, B>) -> Option<Self::Item> {
        // First iterate over first container values
        if self.a.is_some() {
            let mut first_container = self.a.unwrap();
            let value = first_container.next();
            if value.is_some() {
                self.a = Option::Some(first_container);
                return value;
            } else {
                self.a = Option::None;
            }
        }

        // Then iterate over second container values
        let mut second_container = self.b.unwrap();
        let value = second_container.next();
        self.b = Option::Some(second_container);

        value
    }
}
