/// Trait to represent types that can be created by summing up an iterator.
///
/// This trait is used to implement [`Iterator::sum()`]. Types which implement
/// this trait can be generated by using the [`sum()`] method on an iterator.
/// Like [`FromIterator`], this trait should rarely be called directly.
///
/// [`sum()`]: crate::iter::Iterator::sum
/// [`FromIterator`]: crate::iter::FromIterator
pub trait Sum<A> {
    /// Takes an iterator and generates `Self` from the elements by "summing up"
    /// the items.
    fn sum<I, +Iterator<I>[Item: A], +Destruct<I>, +Destruct<A>>(iter: I) -> A;
}

impl SumAddableTypesImpl<A, +Add<A>, impl ZeroA: core::num::traits::Zero<A>> of Sum<A> {
    fn sum<I, +Iterator<I>[Item: A], +Destruct<I>, +Destruct<A>>(mut iter: I) -> A {
        iter.fold(ZeroA::zero(), |acc, x| acc + x)
    }
}
