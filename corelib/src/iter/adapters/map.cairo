use crate::iter::Iterator;

/// An iterator that maps the values of `iter` with `f`.
///
/// This `struct` is created by the [`map`] method on [`Iterator`]. See its
/// documentation for more.
///
/// [`map`]: Iterator::map
/// [`Iterator`]: core::iter::Iterator
///
#[must_use]
#[derive(Drop, Clone)]
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
    I,
    B,
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
