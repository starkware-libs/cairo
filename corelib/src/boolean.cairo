use core::serde::Serde;

pub impl BoolSerde of Serde<bool> {
    fn serialize(self: @bool, ref output: Array<felt252>) {
        if *self {
            1_felt252
        } else {
            0_felt252
        }.serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<bool> {
        Option::Some(*serialized.pop_front()? != 0)
    }
}

pub(crate) extern fn bool_and_impl(lhs: bool, rhs: bool) -> (bool,) implicits() nopanic;
pub impl BoolBitAnd of BitAnd<bool> {
    #[inline(always)]
    fn bitand(lhs: bool, rhs: bool) -> bool {
        let (r,) = bool_and_impl(lhs, rhs);
        r
    }
}

pub(crate) extern fn bool_or_impl(lhs: bool, rhs: bool) -> (bool,) implicits() nopanic;
pub impl BoolBitOr of BitOr<bool> {
    #[inline(always)]
    fn bitor(lhs: bool, rhs: bool) -> bool {
        let (r,) = bool_or_impl(lhs, rhs);
        r
    }
}

pub(crate) extern fn bool_not_impl(a: bool) -> (bool,) implicits() nopanic;
#[inline(always)]
pub impl BoolNot of Not<bool> {
    #[inline(always)]
    fn not(a: bool) -> bool implicits() nopanic {
        let (r,) = bool_not_impl(a);
        r
    }
}

pub(crate) extern fn bool_xor_impl(lhs: bool, rhs: bool) -> (bool,) implicits() nopanic;
pub impl BoolBitXor of BitXor<bool> {
    #[inline(always)]
    fn bitxor(lhs: bool, rhs: bool) -> bool {
        let (r,) = bool_xor_impl(lhs, rhs);
        r
    }
}

pub impl BoolPartialEq of PartialEq<bool> {
    #[inline(always)]
    fn eq(lhs: @bool, rhs: @bool) -> bool {
        match lhs {
            bool::False => !*rhs,
            bool::True => *rhs,
        }
    }
    #[inline(always)]
    fn ne(lhs: @bool, rhs: @bool) -> bool {
        match lhs {
            bool::False => *rhs,
            bool::True => !*rhs,
        }
    }
}

/// Default values for felt252_dict values.
pub impl BoolFelt252DictValue of Felt252DictValue<bool> {
    #[inline(always)]
    fn zero_default() -> bool nopanic {
        false
    }
}

pub(crate) extern fn bool_to_felt252(a: bool) -> felt252 implicits() nopanic;
pub impl BoolIntoFelt252 of Into<bool, felt252> {
    #[inline(always)]
    fn into(self: bool) -> felt252 implicits() nopanic {
        bool_to_felt252(self)
    }
}


#[generate_trait]
pub impl BoolThenSomeImpl<T, +Drop<T>> of ThenSome<T> {
    /// Returns `Some(t)` if the `bool` is `true`, or `None` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// assert(false.then_some(0) == Option::None);
    /// assert(true.then_some(0) == Option::Some(0));
    /// ```
    #[inline(always)]
    fn then_some(self: bool, t: T) -> Option<T> nopanic {
        if self {
            Option::Some(t)
        } else {
            Option::None
        }
    }
}
