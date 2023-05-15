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

#[inline(always)]
fn update_base(ref base: StorageBaseAddress, offset: u8) -> StorageBaseAddress {
    let current = base;
    base =
        storage_base_address_from_felt252(
            storage_address_from_base_and_offset(base, offset).into()
        );
    current
}

trait StorageAccess<T> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<T>;
    fn write(address_domain: u32, base: StorageBaseAddress, value: T) -> SyscallResult<()>;
    fn read_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress
    ) -> SyscallResult<T>;
    fn write_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress, value: T
    ) -> SyscallResult<()>;
}

impl StorageAccessFelt252 of StorageAccess<felt252> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<felt252> {
        storage_read_syscall(address_domain, storage_address_from_base(base))
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: felt252) -> SyscallResult<()> {
        storage_write_syscall(address_domain, storage_address_from_base(base), value)
    }
    #[inline(always)]
    fn read_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress
    ) -> SyscallResult<felt252> {
        StorageAccessFelt252::read(address_domain, update_base(ref base, 1_u8))
    }
    #[inline(always)]
    fn write_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress, value: felt252
    ) -> SyscallResult<()> {
        StorageAccessFelt252::write(address_domain, update_base(ref base, 1_u8), value)
    }
}

impl StorageAccessBool of StorageAccess<bool> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<bool> {
        Result::Ok(StorageAccess::<felt252>::read(address_domain, base)? != 0)
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: bool) -> SyscallResult<()> {
        StorageAccess::<felt252>::write(address_domain, base, if value {
            1
        } else {
            0
        })
    }
    fn read_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress
    ) -> SyscallResult<bool> {
        StorageAccessBool::read(address_domain, update_base(ref base, 1_u8))
    }
    #[inline(always)]
    fn write_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress, value: bool
    ) -> SyscallResult<()> {
        StorageAccessBool::write(address_domain, update_base(ref base, 1_u8), value)
    }
}

impl StorageAccessU8 of StorageAccess<u8> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<u8> {
        Result::Ok(
            StorageAccess::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('StorageAccessU8 - non u8')
        )
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: u8) -> SyscallResult<()> {
        StorageAccess::<felt252>::write(address_domain, base, value.into())
    }
    fn read_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress
    ) -> SyscallResult<u8> {
        StorageAccessU8::read(address_domain, update_base(ref base, 1_u8))
    }
    fn write_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress, value: u8
    ) -> SyscallResult<()> {
        StorageAccessU8::write(address_domain, update_base(ref base, 1_u8), value)
    }
}

impl StorageAccessU16 of StorageAccess<u16> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<u16> {
        Result::Ok(
            StorageAccess::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('StorageAccessU16 - non u16')
        )
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: u16) -> SyscallResult<()> {
        StorageAccess::<felt252>::write(address_domain, base, value.into())
    }
    fn read_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress
    ) -> SyscallResult<u16> {
        StorageAccessU16::read(address_domain, update_base(ref base, 1_u8))
    }
    fn write_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress, value: u16
    ) -> SyscallResult<()> {
        StorageAccessU16::write(address_domain, update_base(ref base, 1_u8), value)
    }
}

impl StorageAccessU32 of StorageAccess<u32> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<u32> {
        Result::Ok(
            StorageAccess::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('StorageAccessU32 - non u32')
        )
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: u32) -> SyscallResult<()> {
        StorageAccess::<felt252>::write(address_domain, base, value.into())
    }
    fn read_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress
    ) -> SyscallResult<u32> {
        StorageAccessU32::read(address_domain, update_base(ref base, 1_u8))
    }
    fn write_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress, value: u32
    ) -> SyscallResult<()> {
        StorageAccessU32::write(address_domain, update_base(ref base, 1_u8), value)
    }
}

impl StorageAccessU64 of StorageAccess<u64> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<u64> {
        Result::Ok(
            StorageAccess::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('StorageAccessU64 - non u64')
        )
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: u64) -> SyscallResult<()> {
        StorageAccess::<felt252>::write(address_domain, base, value.into())
    }
    fn read_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress
    ) -> SyscallResult<u64> {
        StorageAccessU64::read(address_domain, update_base(ref base, 1_u8))
    }
    fn write_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress, value: u64
    ) -> SyscallResult<()> {
        StorageAccessU64::write(address_domain, update_base(ref base, 1_u8), value)
    }
}

impl StorageAccessU128 of StorageAccess<u128> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<u128> {
        Result::Ok(
            StorageAccess::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('StorageAccessU128 - non u128')
        )
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: u128) -> SyscallResult<()> {
        StorageAccess::<felt252>::write(address_domain, base, value.into())
    }
    fn read_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress
    ) -> SyscallResult<u128> {
        StorageAccessU128::read(address_domain, update_base(ref base, 1_u8))
    }
    fn write_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress, value: u128
    ) -> SyscallResult<()> {
        StorageAccessU128::write(address_domain, update_base(ref base, 1_u8), value)
    }
}

impl StorageAccessU256 of StorageAccess<u256> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<u256> {
        Result::Ok(
            u256 {
                low: StorageAccess::<u128>::read(address_domain, base)?,
                high: storage_read_syscall(
                    address_domain, storage_address_from_base_and_offset(base, 1_u8)
                )?
                    .try_into()
                    .expect('StorageAccessU256 - non u256')
            }
        )
    }
    fn write(address_domain: u32, base: StorageBaseAddress, value: u256) -> SyscallResult<()> {
        StorageAccess::<u128>::write(address_domain, base, value.low)?;
        storage_write_syscall(
            address_domain, storage_address_from_base_and_offset(base, 1_u8), value.high.into()
        )
    }
    fn read_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress
    ) -> SyscallResult<u256> {
        StorageAccessU256::read(address_domain, update_base(ref base, 2_u8))
    }
    fn write_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress, value: u256
    ) -> SyscallResult<()> {
        StorageAccessU256::write(address_domain, update_base(ref base, 2_u8), value)
    }
}

impl StorageAccessStorageAddress of StorageAccess<StorageAddress> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<StorageAddress> {
        Result::Ok(
            StorageAccess::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('Non StorageAddress')
        )
    }
    #[inline(always)]
    fn write(
        address_domain: u32, base: StorageBaseAddress, value: StorageAddress
    ) -> SyscallResult<()> {
        StorageAccess::<felt252>::write(address_domain, base, value.into())
    }
    fn read_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress
    ) -> SyscallResult<StorageAddress> {
        StorageAccessStorageAddress::read(address_domain, update_base(ref base, 1_u8))
    }
    fn write_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress, value: StorageAddress
    ) -> SyscallResult<()> {
        StorageAccessStorageAddress::write(address_domain, update_base(ref base, 1_u8), value)
    }
}

impl StorageAccessContractAddress of StorageAccess<ContractAddress> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<ContractAddress> {
        Result::Ok(
            StorageAccess::<felt252>::read(address_domain, base)?
                .try_into()
                .expect('Non ContractAddress')
        )
    }
    #[inline(always)]
    fn write(
        address_domain: u32, base: StorageBaseAddress, value: ContractAddress
    ) -> SyscallResult<()> {
        StorageAccess::<felt252>::write(address_domain, base, value.into())
    }
    fn read_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress
    ) -> SyscallResult<ContractAddress> {
        StorageAccessContractAddress::read(address_domain, update_base(ref base, 1_u8))
    }
    fn write_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress, value: ContractAddress
    ) -> SyscallResult<()> {
        StorageAccessContractAddress::write(address_domain, update_base(ref base, 1_u8), value)
    }
}

impl StorageAccessClassHash of StorageAccess<ClassHash> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<ClassHash> {
        Result::Ok(
            StorageAccess::<felt252>::read(address_domain, base)?.try_into().expect('Non ClassHash')
        )
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: ClassHash) -> SyscallResult<()> {
        StorageAccess::<felt252>::write(address_domain, base, value.into())
    }
    fn read_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress
    ) -> SyscallResult<ClassHash> {
        StorageAccessClassHash::read(address_domain, update_base(ref base, 1_u8))
    }
    fn write_consecutive_internal(
        address_domain: u32, ref base: StorageBaseAddress, value: ClassHash
    ) -> SyscallResult<()> {
        StorageAccessClassHash::write(address_domain, update_base(ref base, 1_u8), value)
    }
}
