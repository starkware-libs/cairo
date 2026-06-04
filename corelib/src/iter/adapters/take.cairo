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
        // All arithmetic here goes through `checked_*` + `let ... else` (never `-`/`+`/`unwrap`),
        // so no panic-handling code is emitted into the Sierra. Each `else { return Ok(()) }`
        // guards a case that cannot happen for a valid iterator - one whose `advance_by` reports a
        // remainder no larger than the amount requested - so the `Ok(())` is never actually
        // reached.
        if let Some(updated_n) = self.n.checked_sub(n) {
            // Advancing `n` fits within the remaining take budget.
            self.n = updated_n;
            match self.iter.advance_by(n) {
                Ok(_) => Ok(()),
                Err(inner_rem) => {
                    // The inner advanced `n - inner_rem`; refund the `inner_rem` it could not take.
                    // `updated_n + inner_rem` is at most `self.n` before the split, so no overflow.
                    let Some(refunded) = self.n.checked_add(inner_rem.into()) else {
                        return Ok(());
                    };
                    self.n = refunded;
                    Err(inner_rem)
                },
            }
        } else {
            // `n` exceeds the budget, so drain the whole budget and report the shortfall. The take
            // is now exhausted, so reset `self.n` to 0 - a later `next`/`count` then short-circuits
            // instead of re-driving the now-dead inner iterator.
            let available = self.n;
            self.n = 0;
            // How many of `available` the inner actually advanced (all of them, unless it ran out
            // first - a valid inner reports `inner_rem <= available`).
            let advanced = match self.iter.advance_by(available) {
                Ok(_) => available,
                Err(inner_rem) => {
                    let Some(advanced) = available.checked_sub(inner_rem.into()) else {
                        return Ok(());
                    };
                    advanced
                },
            };
            // `n > available >= advanced`, so the shortfall `n - advanced` is always `>= 1`.
            let Some(shortfall) = n.checked_sub(advanced) else {
                return Ok(());
            };
            let Some(remainder) = shortfall.try_into() else {
                return Ok(());
            };
            Err(remainder)
        }
    }
}
