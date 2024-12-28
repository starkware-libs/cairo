use crate::iter::Iterator;

#[must_use]
#[derive(Clone)]
pub struct Map<I, F> {
    iter: I,
    f: F,
}

#[generate_trait]
pub(crate) impl MapImpl<I, F> of MapTrait<I, F> {
    fn new(iter: I, f: F) -> Map<I, F> {
        Map { iter, f }
    }
}

impl MapIterator<
    B,
    I,
    F,
    impl TIter: Iterator<I>,
    +core::ops::FnOnce<F, (TIter::Item,)>[Output: B],
    +Drop<I>,
    +Drop<F>,
    +Copy<F>,
> of Iterator<Map<I, F>> {
    type Item = B;
    fn next(ref self: Map<I, F>) -> Option<B> {
        self.iter.next().map(self.f)
    }
}
