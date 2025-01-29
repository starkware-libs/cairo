/// An iterator that filters the elements of `iter` with `predicate`.
///
/// This `struct` is created by the [`filter`] method on [`Iterator`]. See its
/// documentation for more.
///
/// [`filter`]: Iterator::filter
#[must_use]
#[derive(Drop, Clone)]
pub struct Filter<I, P> {
    pub iter: I,
    pub predicate: P,
}

pub fn filter_iterator<I, P>(iter: I, predicate: P) -> Filter<I, P> {
    Filter { iter, predicate }
}

impl FilterIterator<
    I,
    P,
    impl TIter: Iterator<I>,
    +core::ops::Fn<P, (@TIter::Item,)>[Output: bool],
    +Destruct<I>,
    +Destruct<P>,
    +Destruct<TIter::Item>,
> of Iterator<Filter<I, P>> {
    type Item = TIter::Item;
    fn next(ref self: Filter<I, P>) -> Option<Self::Item> {
        self.iter.find(@self.predicate)
    }
}
