#[feature("bounded-int-utils")]
use core::internal::bounded_int::{BoundedInt, SubHelper, UnitInt, sub, upcast};

impl SubHelperImpl<T, const MAX: felt252> of SubHelper<UnitInt<MAX>, T> {
    type Result = BoundedInt<0, MAX>;
}

pub impl Impl<T, const MAX: felt252, const MAX_TYPED: UnitInt<MAX>> of core::traits::BitNot<T> {
    fn bitnot(a: T) -> T {
        upcast::<BoundedInt<0, MAX>, T>(sub(MAX_TYPED, a))
    }
}
