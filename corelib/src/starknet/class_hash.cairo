use zeroable::Zeroable;

#[derive(Copy, Drop)]
extern type ClassHash;


extern fn class_hash_const<const address>() -> ClassHash nopanic;
extern fn class_hash_to_felt252(address: ClassHash) -> felt252 nopanic;

extern fn class_hash_try_from_felt252(
    address: felt252
) -> Option<ClassHash> implicits(RangeCheck) nopanic;

impl Felt252TryIntoClassHash of TryInto::<felt252, ClassHash> {
    fn try_into(self: felt252) -> Option<ClassHash> {
        class_hash_try_from_felt252(self)
    }
}
impl ClassHashIntoFelt252 of Into::<ClassHash, felt252> {
    fn into(self: ClassHash) -> felt252 {
        class_hash_to_felt252(self)
    }
}

impl ClassHashZeroable of Zeroable::<ClassHash> {
    fn zero() -> ClassHash {
        class_hash_const::<0>()
    }

    #[inline(always)]
    fn is_zero(self: ClassHash) -> bool {
        class_hash_to_felt252(self).is_zero()
    }

    #[inline(always)]
    fn is_non_zero(self: ClassHash) -> bool {
        !self.is_zero()
    }
}

impl ClassHashSerde of serde::Serde::<ClassHash> {
    fn serialize(ref serialized: Array<felt252>, input: ClassHash) {
        serde::Serde::serialize(ref serialized, class_hash_to_felt252(input));
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<ClassHash> {
        Option::Some(class_hash_try_from_felt252(serde::Serde::deserialize(ref serialized)?)?)
    }
}

impl ClassHashPartialEq of PartialEq::<ClassHash> {
    #[inline(always)]
    fn eq(a: ClassHash, b: ClassHash) -> bool {
        class_hash_to_felt252(a) == class_hash_to_felt252(b)
    }
    #[inline(always)]
    fn ne(a: ClassHash, b: ClassHash) -> bool {
        !(a == b)
    }
}
