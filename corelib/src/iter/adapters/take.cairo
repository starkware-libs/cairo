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
        if self.n != 0 {
            self.n -= 1;
            self.iter.next()
        } else {
            None
        }
    }

    #[inline]
    fn nth<+Destruct<Take<I>>, +Destruct<Self::Item>>(
        ref self: Take<I>, n: usize,
    ) -> Option<Self::Item> {
        if self.n > n {
            self.n -= n + 1;
            self.iter.nth(n)
        } else {
            if self.n != 0 {
                let _ = self.iter.advance_by(self.n - 1);
                self.n = 0;
            }
            None
        }
    }

    #[inline]
    fn advance_by<+Destruct<Take<I>>, +Destruct<Self::Item>>(
        ref self: Take<I>, n: usize,
    ) -> Result<(), NonZero<usize>> {
        if self.n >= n {
            self.n -= n;
            match self.iter.advance_by(n) {
                Ok(_) => Ok(()),
                Err(rem_nz) => {
                    self.n += rem_nz.into();
                    Err(rem_nz)
                },
            }
        } else {
            let available = self.n;
            match self.iter.advance_by(available) {
                Ok(_) => {
                    self.n = 0;
                    Err((n - available).try_into().unwrap())
                },
                Err(rem_nz) => {
                    self.n -= rem_nz.into();
                    Err((n - self.n).try_into().unwrap())
                },
            }
        }
    }
}
