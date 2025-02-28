/// An iterator that links two iterators together, in a chain.
///
/// This `struct` is created by [`Iterator::chain`]. See the
/// documentation for more.
#[derive(Drop)]
pub struct Chain<A, B> {
    // These are "fused" with `Option` so we don't need separate state to track which part is
    // already exhausted.
    //
    // The "first" iterator is actually set to `Option::None` when exhausted.
    a: Option<A>,
    b: B,
}

#[inline]
pub fn chained_iterator<A, B>(a: A, b: B) -> Chain<A, B> {
    Chain { a: Option::Some(a), b }
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
        // First iterate over first container values.
        if let Option::Some(mut first) = self.a {
            if let Option::Some(value) = first.next() {
                self.a = Option::Some(first);
                return Option::Some(value);
            }
        }

        // Then iterate over second container values.
        let next_val = self.b.next();
        self.a = Option::None;
        next_val
    }
}
