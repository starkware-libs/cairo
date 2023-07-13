use traits::{Into, TryInto};
use option::OptionTrait;
use starknet::{
    SyscallResult, syscalls::{storage_read_syscall, storage_write_syscall},
    contract_address::{ContractAddress, Felt252TryIntoContractAddress, ContractAddressIntoFelt252},
    class_hash::{ClassHash, Felt252TryIntoClassHash, ClassHashIntoFelt252}
};
use serde::Serde;

#[derive(Copy, Drop)]
extern type StorageAddress;

#[derive(Copy, Drop)]
extern type StorageBaseAddress;

// Storage.
extern fn storage_base_address_const<const address: felt252>() -> StorageBaseAddress nopanic;
extern fn storage_base_address_from_felt252(
    addr: felt252
) -> StorageBaseAddress implicits(RangeCheck) nopanic;

extern fn storage_address_to_felt252(address: StorageAddress) -> felt252 nopanic;
extern fn storage_address_from_base_and_offset(
    base: StorageBaseAddress, offset: u8
) -> StorageAddress nopanic;

extern fn storage_address_from_base(base: StorageBaseAddress) -> StorageAddress nopanic;

extern fn storage_address_try_from_felt252(
    address: felt252
) -> Option<StorageAddress> implicits(RangeCheck) nopanic;

impl Felt252TryIntoStorageAddress of TryInto<felt252, StorageAddress> {
    fn try_into(self: felt252) -> Option<StorageAddress> {
        storage_address_try_from_felt252(self)
    }
}
impl StorageAddressIntoFelt252 of Into<StorageAddress, felt252> {
    fn into(self: StorageAddress) -> felt252 {
        storage_address_to_felt252(self)
    }
}

impl StorageAddressSerde of serde::Serde<StorageAddress> {
    fn serialize(self: @StorageAddress, ref output: Array<felt252>) {
        storage_address_to_felt252(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<StorageAddress> {
        Option::Some(
            storage_address_try_from_felt252(serde::Serde::<felt252>::deserialize(ref serialized)?)?
        )
    }
}

trait StorageValue<T> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<T>;
    fn write(address_domain: u32, base: StorageBaseAddress, value: T) -> SyscallResult<()>;
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<T>;
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: T
    ) -> SyscallResult<()>;
    fn size() -> u8;
}

impl StorageValueFelt252 of StorageValue<felt252> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<felt252> {
        storage_read_syscall(address_domain, storage_address_from_base(base))
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: felt252) -> SyscallResult<()> {
        storage_write_syscall(address_domain, storage_address_from_base(base), value)
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<felt252> {
        storage_read_syscall(address_domain, storage_address_from_base_and_offset(base, offset))
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: felt252
    ) -> SyscallResult<()> {
        storage_write_syscall(
            address_domain, storage_address_from_base_and_offset(base, offset), value
        )
    }
    #[inline(always)]
    fn size() -> u8 {
        1_u8
    }
}

impl StorageValueBool of StorageValue<bool> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<bool> {
        Result::Ok(StorageValue::<felt252>::read(address_domain, base)? != 0)
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: bool) -> SyscallResult<()> {
        StorageValue::<felt252>::write(address_domain, base, if value {
            1
        } else {
            0
        })
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<bool> {
        Result::Ok(StorageValue::<felt252>::read_at_offset(address_domain, base, offset)? != 0)
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: bool
    ) -> SyscallResult<()> {
        StorageValue::<felt252>::write_at_offset(
            address_domain, base, offset, if value {
                1
            } else {
                0
            }
        )
    }
    #[inline(always)]
    fn size() -> u8 {
        1_u8
    }
}

impl StorageValueU8 of StorageValue<u8> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<u8> {
        Result::Ok(
            StorageValue::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('StorageValueU8 - non u8')
        )
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: u8) -> SyscallResult<()> {
        StorageValue::<felt252>::write(address_domain, base, value.into())
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<u8> {
        Result::Ok(
            StorageValue::<felt252>::read_at_offset(address_domain, base, offset)?
                .try_into()
                .expect('StorageValueU8 - non u8')
        )
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: u8
    ) -> SyscallResult<()> {
        StorageValue::<felt252>::write_at_offset(address_domain, base, offset, value.into())
    }
    #[inline(always)]
    fn size() -> u8 {
        1_u8
    }
}

impl StorageValueU16 of StorageValue<u16> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<u16> {
        Result::Ok(
            StorageValue::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('StorageValueU16 - non u16')
        )
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: u16) -> SyscallResult<()> {
        StorageValue::<felt252>::write(address_domain, base, value.into())
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<u16> {
        Result::Ok(
            StorageValue::<felt252>::read_at_offset(address_domain, base, offset)?
                .try_into()
                .expect('StorageValueU16 - non u16')
        )
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: u16
    ) -> SyscallResult<()> {
        StorageValue::<felt252>::write_at_offset(address_domain, base, offset, value.into())
    }
    #[inline(always)]
    fn size() -> u8 {
        1_u8
    }
}

impl StorageValueU32 of StorageValue<u32> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<u32> {
        Result::Ok(
            StorageValue::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('StorageValueU32 - non u32')
        )
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: u32) -> SyscallResult<()> {
        StorageValue::<felt252>::write(address_domain, base, value.into())
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<u32> {
        Result::Ok(
            StorageValue::<felt252>::read_at_offset(address_domain, base, offset)?
                .try_into()
                .expect('StorageValueU32 - non u32')
        )
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: u32
    ) -> SyscallResult<()> {
        StorageValue::<felt252>::write_at_offset(address_domain, base, offset, value.into())
    }
    #[inline(always)]
    fn size() -> u8 {
        1_u8
    }
}

impl StorageValueU64 of StorageValue<u64> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<u64> {
        Result::Ok(
            StorageValue::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('StorageValueU64 - non u64')
        )
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: u64) -> SyscallResult<()> {
        StorageValue::<felt252>::write(address_domain, base, value.into())
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<u64> {
        Result::Ok(
            StorageValue::<felt252>::read_at_offset(address_domain, base, offset)?
                .try_into()
                .expect('StorageValueU64 - non u64')
        )
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: u64
    ) -> SyscallResult<()> {
        StorageValue::<felt252>::write_at_offset(address_domain, base, offset, value.into())
    }
    #[inline(always)]
    fn size() -> u8 {
        1_u8
    }
}

impl StorageValueU128 of StorageValue<u128> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<u128> {
        Result::Ok(
            StorageValue::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('StorageValueU128 - non u128')
        )
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: u128) -> SyscallResult<()> {
        StorageValue::<felt252>::write(address_domain, base, value.into())
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<u128> {
        Result::Ok(
            StorageValue::<felt252>::read_at_offset(address_domain, base, offset)?
                .try_into()
                .expect('StorageValueU128 - non u128')
        )
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: u128
    ) -> SyscallResult<()> {
        StorageValue::<felt252>::write_at_offset(address_domain, base, offset, value.into())
    }
    #[inline(always)]
    fn size() -> u8 {
        1_u8
    }
}

impl StorageValueStorageAddress of StorageValue<StorageAddress> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<StorageAddress> {
        Result::Ok(
            StorageValue::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('Non StorageAddress')
        )
    }
    #[inline(always)]
    fn write(
        address_domain: u32, base: StorageBaseAddress, value: StorageAddress
    ) -> SyscallResult<()> {
        StorageValue::<felt252>::write(address_domain, base, value.into())
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<StorageAddress> {
        Result::Ok(
            StorageValue::<felt252>::read_at_offset(address_domain, base, offset)?
                .try_into()
                .expect('Non StorageAddress')
        )
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: StorageAddress
    ) -> SyscallResult<()> {
        StorageValue::<felt252>::write_at_offset(address_domain, base, offset, value.into())
    }
    #[inline(always)]
    fn size() -> u8 {
        1_u8
    }
}

impl StorageValueContractAddress of StorageValue<ContractAddress> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<ContractAddress> {
        Result::Ok(
            StorageValue::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('Non ContractAddress')
        )
    }
    #[inline(always)]
    fn write(
        address_domain: u32, base: StorageBaseAddress, value: ContractAddress
    ) -> SyscallResult<()> {
        StorageValue::<felt252>::write(address_domain, base, value.into())
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<ContractAddress> {
        Result::Ok(
            StorageValue::<felt252>::read_at_offset(address_domain, base, offset)?
                .try_into()
                .expect('Non ContractAddress')
        )
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: ContractAddress
    ) -> SyscallResult<()> {
        StorageValue::<felt252>::write_at_offset(address_domain, base, offset, value.into())
    }
    #[inline(always)]
    fn size() -> u8 {
        1_u8
    }
}

impl StorageValueClassHash of StorageValue<ClassHash> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<ClassHash> {
        Result::Ok(
            StorageValue::<felt252>::read(address_domain, base)?.try_into().expect('Non ClassHash')
        )
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: ClassHash) -> SyscallResult<()> {
        StorageValue::<felt252>::write(address_domain, base, value.into())
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<ClassHash> {
        Result::Ok(
            StorageValue::<felt252>::read_at_offset(address_domain, base, offset)?
                .try_into()
                .expect('Non ClassHash')
        )
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: ClassHash
    ) -> SyscallResult<()> {
        StorageValue::<felt252>::write_at_offset(address_domain, base, offset, value.into())
    }
    #[inline(always)]
    fn size() -> u8 {
        1_u8
    }
}

impl TupleSize0StorageValue of StorageValue<()> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<()> {
        Result::Ok(())
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: ()) -> SyscallResult<()> {
        Result::Ok(())
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<()> {
        Result::Ok(())
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: ()
    ) -> SyscallResult<()> {
        Result::Ok(())
    }
    #[inline(always)]
    fn size() -> u8 {
        0
    }
}

impl TupleSize1StorageValue<
    E0, impl E0StorageValue: StorageValue<E0>, impl E0Drop: Drop<E0>
> of StorageValue<(E0, )> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<(E0, )> {
        Result::Ok((E0StorageValue::read(address_domain, base)?, ))
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: (E0, )) -> SyscallResult<()> {
        let (e0, ) = value;
        E0StorageValue::write(address_domain, base, e0)
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<(E0, )> {
        Result::Ok((E0StorageValue::read_at_offset(address_domain, base, offset)?, ))
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: (E0, )
    ) -> SyscallResult<()> {
        let (e0, ) = value;
        E0StorageValue::write_at_offset(address_domain, base, offset, e0)
    }
    #[inline(always)]
    fn size() -> u8 {
        E0StorageValue::size()
    }
}

impl TupleSize2StorageValue<
    E0,
    E1,
    impl E0StorageValue: StorageValue<E0>,
    impl E0Drop: Drop<E0>,
    impl E1StorageValue: StorageValue<E1>,
    impl E0Drop: Drop<E1>
> of StorageValue<(E0, E1)> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<(E0, E1)> {
        let e0 = E0StorageValue::read(address_domain, base)?;
        let offset = E0StorageValue::size();
        let e1 = E1StorageValue::read_at_offset(address_domain, base, offset)?;
        Result::Ok((e0, e1))
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: (E0, E1)) -> SyscallResult<()> {
        let (e0, e1) = value;
        E0StorageValue::write(address_domain, base, e0)?;
        let offset = E0StorageValue::size();
        E1StorageValue::write_at_offset(address_domain, base, offset, e1)
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<(E0, E1)> {
        let e0 = E0StorageValue::read_at_offset(address_domain, base, offset)?;
        let offset = E0StorageValue::size();
        let e1 = E1StorageValue::read_at_offset(address_domain, base, offset)?;
        Result::Ok((e0, e1))
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: (E0, E1)
    ) -> SyscallResult<()> {
        let (e0, e1) = value;
        E0StorageValue::write_at_offset(address_domain, base, offset, e0)?;
        let offset = offset + E0StorageValue::size();
        E1StorageValue::write_at_offset(address_domain, base, offset, e1)
    }
    #[inline(always)]
    fn size() -> u8 {
        E0StorageValue::size() + E1StorageValue::size()
    }
}

impl TupleSize3StorageValue<
    E0,
    E1,
    E2,
    impl E0StorageValue: StorageValue<E0>,
    impl E0Drop: Drop<E0>,
    impl E1StorageValue: StorageValue<E1>,
    impl E1Drop: Drop<E1>,
    impl E2StorageValue: StorageValue<E2>,
    impl E2Drop: Drop<E2>
> of StorageValue<(E0, E1, E2)> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<(E0, E1, E2)> {
        let e0 = E0StorageValue::read(address_domain, base)?;
        let offset = E0StorageValue::size();
        let e1 = E1StorageValue::read_at_offset(address_domain, base, offset)?;
        let offset = offset + E1StorageValue::size();
        let e2 = E2StorageValue::read_at_offset(address_domain, base, offset)?;
        Result::Ok((e0, e1, e2))
    }
    #[inline(always)]
    fn write(
        address_domain: u32, base: StorageBaseAddress, value: (E0, E1, E2)
    ) -> SyscallResult<()> {
        let (e0, e1, e2) = value;
        E0StorageValue::write(address_domain, base, e0)?;
        let offset = E0StorageValue::size();
        E1StorageValue::write_at_offset(address_domain, base, offset, e1)?;
        let offset = offset + E1StorageValue::size();
        E2StorageValue::write_at_offset(address_domain, base, offset, e2)
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<(E0, E1, E2)> {
        let e0 = E0StorageValue::read_at_offset(address_domain, base, offset)?;
        let offset = E0StorageValue::size();
        let e1 = E1StorageValue::read_at_offset(address_domain, base, offset)?;
        let offset = offset + E1StorageValue::size();
        let e2 = E2StorageValue::read_at_offset(address_domain, base, offset)?;
        Result::Ok((e0, e1, e2))
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: (E0, E1, E2)
    ) -> SyscallResult<()> {
        let (e0, e1, e2) = value;
        E0StorageValue::write_at_offset(address_domain, base, offset, e0)?;
        let offset = offset + E0StorageValue::size();
        E1StorageValue::write_at_offset(address_domain, base, offset, e1)?;
        let offset = offset + E1StorageValue::size();
        E2StorageValue::write_at_offset(address_domain, base, offset, e2)
    }
    #[inline(always)]
    fn size() -> u8 {
        E0StorageValue::size() + E1StorageValue::size() + E2StorageValue::size()
    }
}

impl TupleSize4StorageValue<
    E0,
    E1,
    E2,
    E3,
    impl E0StorageValue: StorageValue<E0>,
    impl E0Drop: Drop<E0>,
    impl E1StorageValue: StorageValue<E1>,
    impl E1Drop: Drop<E1>,
    impl E2StorageValue: StorageValue<E2>,
    impl E2Drop: Drop<E2>,
    impl E3StorageValue: StorageValue<E3>,
    impl E3Drop: Drop<E3>
> of StorageValue<(E0, E1, E2, E3)> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<(E0, E1, E2, E3)> {
        let e0 = E0StorageValue::read(address_domain, base)?;
        let offset = E0StorageValue::size();
        let e1 = E1StorageValue::read_at_offset(address_domain, base, offset)?;
        let offset = offset + E1StorageValue::size();
        let e2 = E2StorageValue::read_at_offset(address_domain, base, offset)?;
        let offset = offset + E2StorageValue::size();
        let e3 = E3StorageValue::read_at_offset(address_domain, base, offset)?;
        Result::Ok((e0, e1, e2, e3))
    }
    #[inline(always)]
    fn write(
        address_domain: u32, base: StorageBaseAddress, value: (E0, E1, E2, E3)
    ) -> SyscallResult<()> {
        let (e0, e1, e2, e3) = value;
        E0StorageValue::write(address_domain, base, e0)?;
        let offset = E0StorageValue::size();
        E1StorageValue::write_at_offset(address_domain, base, offset, e1)?;
        let offset = offset + E1StorageValue::size();
        E2StorageValue::write_at_offset(address_domain, base, offset, e2)?;
        let offset = offset + E2StorageValue::size();
        E3StorageValue::write_at_offset(address_domain, base, offset, e3)
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<(E0, E1, E2, E3)> {
        let e0 = E0StorageValue::read_at_offset(address_domain, base, offset)?;
        let offset = E0StorageValue::size();
        let e1 = E1StorageValue::read_at_offset(address_domain, base, offset)?;
        let offset = offset + E1StorageValue::size();
        let e2 = E2StorageValue::read_at_offset(address_domain, base, offset)?;
        let offset = offset + E2StorageValue::size();
        let e3 = E3StorageValue::read_at_offset(address_domain, base, offset)?;
        Result::Ok((e0, e1, e2, e3))
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: (E0, E1, E2, E3)
    ) -> SyscallResult<()> {
        let (e0, e1, e2, e3) = value;
        E0StorageValue::write_at_offset(address_domain, base, offset, e0)?;
        let offset = offset + E0StorageValue::size();
        E1StorageValue::write_at_offset(address_domain, base, offset, e1)?;
        let offset = offset + E1StorageValue::size();
        E2StorageValue::write_at_offset(address_domain, base, offset, e2)?;
        let offset = offset + E2StorageValue::size();
        E3StorageValue::write_at_offset(address_domain, base, offset, e3)
    }
    #[inline(always)]
    fn size() -> u8 {
        E0StorageValue::size()
            + E1StorageValue::size()
            + E2StorageValue::size()
            + E3StorageValue::size()
    }
}
