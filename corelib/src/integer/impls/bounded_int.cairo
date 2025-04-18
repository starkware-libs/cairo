#[feature("deprecated-bounded-int-trait")]
pub impl ByBounded<
    T, impl Bounded: crate::num::traits::Bounded<T>,
> of crate::integer::BoundedInt<T> {
    #[inline]
    fn min() -> T nopanic {
        Bounded::MIN
    }

    #[inline]
    fn max() -> T nopanic {
        Bounded::MAX
    }
}
