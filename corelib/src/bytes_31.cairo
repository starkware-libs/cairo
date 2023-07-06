use traits::{Into, TryInto};
use option::OptionTrait;

#[derive(Copy, Drop)]
extern type bytes31;

extern fn bytes31_const<const value: felt252>() -> bytes31 nopanic;
extern fn bytes31_try_from_felt252(value: felt252) -> Option<bytes31> implicits(RangeCheck) nopanic;
extern fn bytes31_to_felt252(value: bytes31) -> felt252 nopanic;

impl Bytes31IntoFelt252 of Into<bytes31, felt252> {
    fn into(self: bytes31) -> felt252 {
        bytes31_to_felt252(self)
    }
}

impl Felt252TryIntoBytes31 of TryInto<felt252, bytes31> {
    fn try_into(self: felt252) -> Option<bytes31> {
        bytes31_try_from_felt252(self)
    }
}

// TODO(yuval): implement all `into`s using `integer::upcast(self)`.
impl U8IntoBytes31 of Into<u8, bytes31> {
    fn into(self: u8) -> bytes31 {
        let as_felt: felt252 = self.into();
        as_felt.try_into().unwrap()
    }
}
impl U16IntoBytes31 of Into<u16, bytes31> {
    fn into(self: u16) -> bytes31 {
        let as_felt: felt252 = self.into();
        as_felt.try_into().unwrap()
    }
}
impl U32IntoBytes31 of Into<u32, bytes31> {
    fn into(self: u32) -> bytes31 {
        let as_felt: felt252 = self.into();
        as_felt.try_into().unwrap()
    }
}
impl U64IntoBytes31 of Into<u64, bytes31> {
    fn into(self: u64) -> bytes31 {
        let as_felt: felt252 = self.into();
        as_felt.try_into().unwrap()
    }
}
impl U128IntoBytes31 of Into<u128, bytes31> {
    fn into(self: u128) -> bytes31 {
        let as_felt: felt252 = self.into();
        as_felt.try_into().unwrap()
    }
}
