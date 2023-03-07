use zeroable::Zeroable;

#[derive(Copy, Drop)]
extern type ClassHash;


extern fn class_hash_const<const address>() -> ClassHash nopanic;
extern fn class_hash_to_felt(address: ClassHash) -> felt nopanic;

extern fn class_hash_try_from_felt(
    address: felt
) -> Option<ClassHash> implicits(RangeCheck) nopanic;

impl FeltTryIntoClassHash of TryInto::<felt, ClassHash> {
    fn try_into(self: felt) -> Option<ClassHash> {
        class_hash_try_from_felt(self)
    }
}
impl ClassHashIntoFelt of Into::<ClassHash, felt> {
    fn into(self: ClassHash) -> felt {
        class_hash_to_felt(self)
    }
}

impl ClassHashZeroable of Zeroable::<ClassHash> {
    fn zero() -> ClassHash {
        class_hash_const::<0>()
    }

    #[inline(always)]
    fn is_zero(self: ClassHash) -> bool {
        class_hash_to_felt(self).is_zero()
    }

    #[inline(always)]
    fn is_non_zero(self: ClassHash) -> bool {
        !self.is_zero()
    }
}

impl ClassHashSerde of serde::Serde::<ClassHash> {
    fn serialize(ref serialized: Array<felt>, input: ClassHash) {
        serde::Serde::serialize(ref serialized, class_hash_to_felt(input));
    }
    fn deserialize(ref serialized: Span<felt>) -> Option<ClassHash> {
        Option::Some(class_hash_try_from_felt(serde::Serde::deserialize(ref serialized)?)?)
    }
}

impl ClassHashPartialEq of PartialEq::<ClassHash> {
    #[inline(always)]
    fn eq(a: ClassHash, b: ClassHash) -> bool {
        class_hash_to_felt(a) == class_hash_to_felt(b)
    }
    #[inline(always)]
    fn ne(a: ClassHash, b: ClassHash) -> bool {
        !(a == b)
    }
}
