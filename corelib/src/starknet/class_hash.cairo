use core::serde::Serde;
use core::hash::{Hash, HashStateTrait};

#[derive(Copy, Drop)]
pub extern type ClassHash;

pub extern fn class_hash_const<const address: felt252>() -> ClassHash nopanic;
pub(crate) extern fn class_hash_to_felt252(address: ClassHash) -> felt252 nopanic;

pub(crate) extern fn class_hash_try_from_felt252(
    address: felt252
) -> Option<ClassHash> implicits(RangeCheck) nopanic;

pub(crate) impl Felt252TryIntoClassHash of TryInto<felt252, ClassHash> {
    fn try_into(self: felt252) -> Option<ClassHash> {
        class_hash_try_from_felt252(self)
    }
}
pub(crate) impl ClassHashIntoFelt252 of Into<ClassHash, felt252> {
    fn into(self: ClassHash) -> felt252 {
        class_hash_to_felt252(self)
    }
}

impl ClassHashZero of core::num::traits::Zero<ClassHash> {
    fn zero() -> ClassHash {
        class_hash_const::<0>()
    }
    #[inline(always)]
    fn is_zero(self: @ClassHash) -> bool {
        core::num::traits::Zero::<felt252>::is_zero(@class_hash_to_felt252(*self))
    }
    #[inline(always)]
    fn is_non_zero(self: @ClassHash) -> bool {
        !self.is_zero()
    }
}

pub(crate) impl ClassHashZeroable =
    core::zeroable::zero_based::ZeroableImpl<ClassHash, ClassHashZero>;

impl ClassHashSerde of Serde<ClassHash> {
    fn serialize(self: @ClassHash, ref output: Array<felt252>) {
        class_hash_to_felt252(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<ClassHash> {
        Option::Some(class_hash_try_from_felt252(Serde::<felt252>::deserialize(ref serialized)?)?)
    }
}

impl ClassHashPartialEq of PartialEq<ClassHash> {
    #[inline(always)]
    fn eq(lhs: @ClassHash, rhs: @ClassHash) -> bool {
        class_hash_to_felt252(*lhs) == class_hash_to_felt252(*rhs)
    }
    #[inline(always)]
    fn ne(lhs: @ClassHash, rhs: @ClassHash) -> bool {
        !(lhs == rhs)
    }
}

impl HashClassHash<S, +HashStateTrait<S>, +Drop<S>> =
    core::hash::into_felt252_based::HashImpl<ClassHash, S>;

impl DebugClassHash = core::fmt::into_felt252_based::DebugImpl<ClassHash>;
