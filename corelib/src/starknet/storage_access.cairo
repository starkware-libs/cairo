use traits::Into;
use traits::TryInto;
use option::OptionTrait;
use starknet::SyscallResult;
use starknet::syscalls::storage_read_syscall;
use starknet::syscalls::storage_write_syscall;
use starknet::contract_address::ContractAddress;
use starknet::contract_address::FeltTryIntoContractAddress;
use starknet::contract_address::ContractAddressIntoFelt;
use starknet::class_hash::ClassHash;
use starknet::class_hash::FeltTryIntoClassHash;
use starknet::class_hash::ClassHashIntoFelt;

#[derive(Copy, Drop)]
extern type StorageAddress;

#[derive(Copy, Drop)]
extern type StorageBaseAddress;

// Storage.
extern fn storage_base_address_const<const address>() -> StorageBaseAddress nopanic;
extern fn storage_base_address_from_felt(
    addr: felt
) -> StorageBaseAddress implicits(RangeCheck) nopanic;

extern fn storage_address_to_felt(address: StorageAddress) -> felt nopanic;
extern fn storage_address_from_base_and_offset(
    base: StorageBaseAddress, offset: u8
) -> StorageAddress nopanic;

extern fn storage_address_from_base(base: StorageBaseAddress) -> StorageAddress nopanic;

extern fn storage_address_try_from_felt(
    address: felt
) -> Option<StorageAddress> implicits(RangeCheck) nopanic;

impl FeltTryIntoStorageAddress of TryInto::<felt, StorageAddress> {
    fn try_into(self: felt) -> Option<StorageAddress> {
        storage_address_try_from_felt(self)
    }
}
impl StorageAddressIntoFelt of Into::<StorageAddress, felt> {
    fn into(self: StorageAddress) -> felt {
        storage_address_to_felt(self)
    }
}

impl StorageAddressSerde of serde::Serde::<StorageAddress> {
    fn serialize(ref serialized: Array<felt>, input: StorageAddress) {
        serde::Serde::serialize(ref serialized, storage_address_to_felt(input));
    }
    fn deserialize(ref serialized: Span<felt>) -> Option<StorageAddress> {
        Option::Some(storage_address_try_from_felt(serde::Serde::deserialize(ref serialized)?)?)
    }
}

trait StorageAccess<T> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult<T>;
    fn write(address_domain: felt, base: StorageBaseAddress, value: T) -> SyscallResult<()>;
}

impl StorageAccessFelt of StorageAccess::<felt> {
    #[inline(always)]
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult<felt> {
        storage_read_syscall(address_domain, storage_address_from_base(base))
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: felt) -> SyscallResult<()> {
        storage_write_syscall(address_domain, storage_address_from_base(base), value)
    }
}

impl StorageAccessBool of StorageAccess::<bool> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult<bool> {
        Result::Ok(StorageAccess::<felt>::read(address_domain, base)? != 0)
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: bool) -> SyscallResult<()> {
        StorageAccess::<felt>::write(address_domain, base, if value {
            1
        } else {
            0
        })
    }
}

impl StorageAccessU8 of StorageAccess::<u8> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult<u8> {
        Result::Ok(
            StorageAccess::<felt>::read(
                address_domain, base
            )?.try_into().expect('StorageAccessU8 - non u8')
        )
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: u8) -> SyscallResult<()> {
        StorageAccess::<felt>::write(address_domain, base, value.into())
    }
}

impl StorageAccessU16 of StorageAccess::<u16> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult<u16> {
        Result::Ok(
            StorageAccess::<felt>::read(
                address_domain, base
            )?.try_into().expect('StorageAccessU16 - non u16')
        )
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: u16) -> SyscallResult<()> {
        StorageAccess::<felt>::write(address_domain, base, value.into())
    }
}

impl StorageAccessU32 of StorageAccess::<u32> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult<u32> {
        Result::Ok(
            StorageAccess::<felt>::read(
                address_domain, base
            )?.try_into().expect('StorageAccessU32 - non u32')
        )
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: u32) -> SyscallResult<()> {
        StorageAccess::<felt>::write(address_domain, base, value.into())
    }
}

impl StorageAccessU64 of StorageAccess::<u64> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult<u64> {
        Result::Ok(
            StorageAccess::<felt>::read(
                address_domain, base
            )?.try_into().expect('StorageAccessU64 - non u64')
        )
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: u64) -> SyscallResult<()> {
        StorageAccess::<felt>::write(address_domain, base, value.into())
    }
}

impl StorageAccessU128 of StorageAccess::<u128> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult<u128> {
        Result::Ok(
            StorageAccess::<felt>::read(
                address_domain, base
            )?.try_into().expect('StorageAccessU128 - non u128')
        )
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: u128) -> SyscallResult<()> {
        StorageAccess::<felt>::write(address_domain, base, value.into())
    }
}

impl StorageAccessU256 of StorageAccess::<u256> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult<u256> {
        Result::Ok(
            u256 {
                low: StorageAccess::<u128>::read(address_domain, base)?,
                high: storage_read_syscall(
                    address_domain, storage_address_from_base_and_offset(base, 1_u8)
                )?.try_into().expect('StorageAccessU256 - non u256')
            }
        )
    }
    fn write(address_domain: felt, base: StorageBaseAddress, value: u256) -> SyscallResult<()> {
        StorageAccess::<u128>::write(address_domain, base, value.low)?;
        storage_write_syscall(
            address_domain, storage_address_from_base_and_offset(base, 1_u8), value.high.into()
        )
    }
}

impl StorageAccessStorageAddress of StorageAccess::<StorageAddress> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult<StorageAddress> {
        Result::Ok(
            StorageAccess::<felt>::read(
                address_domain, base
            )?.try_into().expect('Non StorageAddress')
        )
    }
    #[inline(always)]
    fn write(
        address_domain: felt, base: StorageBaseAddress, value: StorageAddress
    ) -> SyscallResult<()> {
        StorageAccess::<felt>::write(address_domain, base, value.into())
    }
}

impl StorageAccessContractAddress of StorageAccess::<ContractAddress> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult<ContractAddress> {
        Result::Ok(
            StorageAccess::<felt>::read(
                address_domain, base
            )?.try_into().expect('Non ContractAddress')
        )
    }
    #[inline(always)]
    fn write(
        address_domain: felt, base: StorageBaseAddress, value: ContractAddress
    ) -> SyscallResult<()> {
        StorageAccess::<felt>::write(address_domain, base, value.into())
    }
}

impl StorageAccessClassHash of StorageAccess::<ClassHash> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult<ClassHash> {
        Result::Ok(
            StorageAccess::<felt>::read(address_domain, base)?.try_into().expect('Non ClassHash')
        )
    }
    #[inline(always)]
    fn write(
        address_domain: felt, base: StorageBaseAddress, value: ClassHash
    ) -> SyscallResult<()> {
        StorageAccess::<felt>::write(address_domain, base, value.into())
    }
}
