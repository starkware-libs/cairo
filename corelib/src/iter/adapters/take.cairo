use crate::num::traits::{CheckedAdd, CheckedSub};

/// An iterator that only iterates over the first `n` iterations of `iter`.
///
/// This `struct` is created by the [`take`] method on [`Iterator`]. See its
/// documentation for more.
///
/// [`take`]: Iterator::take
#[must_use]
#[derive(Drop, Clone)]
pub struct Take<I> {
    iter: I,
    n: usize,
}

pub fn take_iterator<I>(iter: I, n: usize) -> Take<I> {
    Take { iter, n }
}

impl TakeIterator<I, impl TIter: Iterator<I>, +Drop<I>> of Iterator<Take<I>> {
    type Item = TIter::Item;
    #[inline]
    fn next(ref self: Take<I>) -> Option<Self::Item> {
        self.n = self.n.checked_sub(1)?;
        self.iter.next()
    }

    #[inline]
    fn nth<+Destruct<Take<I>>, +Destruct<Self::Item>>(
        ref self: Take<I>, n: usize,
    ) -> Option<Self::Item> {
        if let Some(n_plus_1) = n.checked_add(1)
            && let Some(updated_n) = self.n.checked_sub(n_plus_1) {
            self.n = updated_n;
            self.iter.nth(n)
        } else {
            if self.n != 0 {
                let _ = self.iter.advance_by(self.n);
                self.n = 0;
            }
            None
        }
    }

    #[inline]
    fn advance_by<+Destruct<Take<I>>, +Destruct<Self::Item>>(
        ref self: Take<I>, n: usize,
    ) -> Result<(), NonZero<usize>> {
        if let Some(updated_n) = self.n.checked_sub(n) {
            self.n = updated_n;
            match self.iter.advance_by(n) {
                Ok(_) => Ok(()),
                Err(rem_nz) => {
                    // Can't overflow - at most restores `self.n` to its value before the
                    // subtraction above.
                    if let Some(restored) = self.n.checked_add(rem_nz.into()) {
                        self.n = restored;
                    }
                    Err(rem_nz)
                },
            }
        } else {
            let available = self.n;
            // Set `self.n = 0`, as it is equivalent to shrinking the size by the taken amount if
            // the inner iterator was exhausted.
            self.n = 0;
            let inner_taken = match self.iter.advance_by(available) {
                Ok(_) => available,
                // The sub can't actually fail for properly implemented iterators.
                Err(inner) => if let Some(inner_taken) = available.checked_sub(inner.into()) {
                    inner_taken
                } else {
                    return Ok(());
                },
            };
            if let Some(untaken) = n.checked_sub(inner_taken) && let Some(nz) = untaken.try_into() {
                Err(nz)
            } else {
                Ok(())
            }
        }
    }
}
