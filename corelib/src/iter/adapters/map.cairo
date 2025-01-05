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

pub fn mapped_iterator<I, F>(iter: I, f: F) -> Map<I, F> {
    Map { iter, f }
}

impl MapIterator<
    I,
    F,
    impl TIter: Iterator<I>,
    impl func: core::ops::Fn<F, (TIter::Item,)>,
    +Destruct<I>,
    +Destruct<F>,
> of Iterator<Map<I, F>> {
    type Item = func::Output;
    fn next(ref self: Map<I, F>) -> Option<func::Output> {
        self.iter.next().map(@self.f)
    }
}
