/// An iterator that links two iterators together, in a chain.
///
/// This `struct` is created by [`Iterator::chain`]. See the
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
