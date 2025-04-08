/// An iterator that links two iterators together, in a chain.
///
/// This `struct` is created by [`Iterator::chain`]. See the
/// documentation for more.
#[derive(Drop)]
pub enum Chain<A, B> {
    /// First iterator processing is not done yet.
    Both: (A, B),
    /// First iterator processing is done, processing the second iterator.
    Second: B,
}

#[inline]
pub fn chained_iterator<A, B>(a: A, b: B) -> Chain<A, B> {
    Chain::Both((a, b))
}

impl ChainIterator<
    A,
    B,
    impl IterA: Iterator<A>,
    +Iterator<B>[Item: IterA::Item],
    +Destruct<A>,
    +Destruct<B>,
    +Destruct<IterA::Item>,
> of Iterator<Chain<A, B>> {
    type Item = IterA::Item;

    fn next(ref self: Chain<A, B>) -> Option<Self::Item> {
        let mut b = match self {
            Chain::Second(b) => b,
            Chain::Both((
                mut a, b,
            )) => {
                if let Some(value) = a.next() {
                    self = Chain::Both((a, b));
                    return Option::Some(value);
                }
                b
            },
        };
        let next_val = b.next();
        self = Chain::Second(b);
        next_val
    }
}
