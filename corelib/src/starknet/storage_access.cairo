use core::array::ArrayTrait;
use core::traits::{Into, TryInto};
use core::option::OptionTrait;
use starknet::{
    SyscallResult, syscalls::{storage_read_syscall, storage_write_syscall},
    contract_address::{ContractAddress, Felt252TryIntoContractAddress, ContractAddressIntoFelt252},
    class_hash::{ClassHash, Felt252TryIntoClassHash, ClassHashIntoFelt252}
};
use core::serde::Serde;

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

impl StorageAddressSerde of Serde<StorageAddress> {
    fn serialize(self: @StorageAddress, ref output: Array<felt252>) {
        storage_address_to_felt252(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<StorageAddress> {
        Option::Some(
            storage_address_try_from_felt252(Serde::<felt252>::deserialize(ref serialized)?)?
        )
    }
}

/// Trait for types that can be used as a value in Starknet storage variables.
trait Store<T> {
    /// Reads a value from storage from domain `address_domain` and base address `base`.
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<T>;
    /// Writes a value to storage to domain `address_domain` and base address `base`.
    fn write(address_domain: u32, base: StorageBaseAddress, value: T) -> SyscallResult<()>;
    /// Reads a value from storage from domain `address_domain` and base address `base` at offset
    /// `offset`.
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<T>;
    /// Writes a value to storage to domain `address_domain` and base address `base` at offset
    /// `offset`.
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: T
    ) -> SyscallResult<()>;
    fn size() -> u8;
}

/// Trait for easier implementation of `Store` used for packing and unpacking values into values
/// that already implement `Store`, and having `Store` implemented using this conversion.
trait StorePacking<T, PackedT> {
    /// Packs a value of type `T` into a value of type `PackedT`.
    fn pack(value: T) -> PackedT;
    /// Unpacks a value of type `PackedT` into a value of type `T`.
    fn unpack(value: PackedT) -> T;
}

impl StoreUsingPacking<
    T, PackedT, impl TPacking: StorePacking<T, PackedT>, impl PackedTStore: Store<PackedT>
> of Store<T> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<T> {
        Result::Ok(TPacking::unpack(PackedTStore::read(address_domain, base)?))
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: T) -> SyscallResult<()> {
        PackedTStore::write(address_domain, base, TPacking::pack(value))
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<T> {
        Result::Ok(TPacking::unpack(PackedTStore::read_at_offset(address_domain, base, offset)?))
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: T
    ) -> SyscallResult<()> {
        PackedTStore::write_at_offset(address_domain, base, offset, TPacking::pack(value))
    }
    #[inline(always)]
    fn size() -> u8 {
        PackedTStore::size()
    }
}

impl StoreFelt252 of Store<felt252> {
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

impl StorePackingBool of StorePacking<bool, felt252> {
    fn pack(value: bool) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> bool {
        value != 0
    }
}

impl StorePackingU8 of StorePacking<u8, felt252> {
    fn pack(value: u8) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> u8 {
        value.try_into().expect('StoreU8 - non u8')
    }
}

impl StorePackingI8 of StorePacking<i8, felt252> {
    fn pack(value: i8) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> i8 {
        value.try_into().expect('StoreI8 - non i8')
    }
}

impl StorePackingU16 of StorePacking<u16, felt252> {
    fn pack(value: u16) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> u16 {
        value.try_into().expect('StoreU16 - non u16')
    }
}

impl StorePackingI16 of StorePacking<i16, felt252> {
    fn pack(value: i16) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> i16 {
        value.try_into().expect('StoreI16 - non i16')
    }
}

impl StorePackingU32 of StorePacking<u32, felt252> {
    fn pack(value: u32) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> u32 {
        value.try_into().expect('StoreU32 - non u32')
    }
}

impl StorePackingI32 of StorePacking<i32, felt252> {
    fn pack(value: i32) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> i32 {
        value.try_into().expect('StoreI32 - non i32')
    }
}

impl StorePackingU64 of StorePacking<u64, felt252> {
    fn pack(value: u64) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> u64 {
        value.try_into().expect('StoreU64 - non u64')
    }
}

impl StorePackingI64 of StorePacking<i64, felt252> {
    fn pack(value: i64) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> i64 {
        value.try_into().expect('StoreI64 - non i64')
    }
}

impl StorePackingU128 of StorePacking<u128, felt252> {
    fn pack(value: u128) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> u128 {
        value.try_into().expect('StoreU128 - non u128')
    }
}

impl StorePackingI128 of StorePacking<i128, felt252> {
    fn pack(value: i128) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> i128 {
        value.try_into().expect('StoreI128 - non i128')
    }
}

impl StorePackingBytes31 of StorePacking<bytes31, felt252> {
    fn pack(value: bytes31) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> bytes31 {
        value.try_into().expect('StoreBytes31 - non bytes31')
    }
}

impl StorePackingStorageAddress of StorePacking<StorageAddress, felt252> {
    fn pack(value: StorageAddress) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> StorageAddress {
        value.try_into().expect('Non StorageAddress')
    }
}

impl StorePackingContractAddress of StorePacking<ContractAddress, felt252> {
    fn pack(value: ContractAddress) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> ContractAddress {
        value.try_into().expect('Non ContractAddress')
    }
}

impl StorePackingClassHash of StorePacking<ClassHash, felt252> {
    fn pack(value: ClassHash) -> felt252 {
        value.into()
    }
    #[inline]
    fn unpack(value: felt252) -> ClassHash {
        value.try_into().expect('Non ClassHash')
    }
}

impl TupleSize0Store of Store<()> {
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

impl TupleSize1Store<E0, impl E0Store: Store<E0>, +Drop<E0>> of Store<(E0,)> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<(E0,)> {
        Result::Ok((E0Store::read(address_domain, base)?,))
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: (E0,)) -> SyscallResult<()> {
        let (e0,) = value;
        E0Store::write(address_domain, base, e0)
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<(E0,)> {
        Result::Ok((E0Store::read_at_offset(address_domain, base, offset)?,))
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: (E0,)
    ) -> SyscallResult<()> {
        let (e0,) = value;
        E0Store::write_at_offset(address_domain, base, offset, e0)
    }
    #[inline(always)]
    fn size() -> u8 {
        E0Store::size()
    }
}

impl TupleSize2Store<
    E0, E1, impl E0Store: Store<E0>, +Drop<E0>, impl E1Store: Store<E1>, +Drop<E1>
> of Store<(E0, E1)> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<(E0, E1)> {
        let e0 = E0Store::read(address_domain, base)?;
        let e1 = E1Store::read_at_offset(address_domain, base, E0Store::size())?;
        Result::Ok((e0, e1))
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: (E0, E1)) -> SyscallResult<()> {
        let (e0, e1) = value;
        E0Store::write(address_domain, base, e0)?;
        E1Store::write_at_offset(address_domain, base, E0Store::size(), e1)
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, mut offset: u8
    ) -> SyscallResult<(E0, E1)> {
        let e0 = E0Store::read_at_offset(address_domain, base, offset)?;
        offset += E0Store::size();
        let e1 = E1Store::read_at_offset(address_domain, base, offset)?;
        Result::Ok((e0, e1))
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, mut offset: u8, value: (E0, E1)
    ) -> SyscallResult<()> {
        let (e0, e1) = value;
        E0Store::write_at_offset(address_domain, base, offset, e0)?;
        offset += E0Store::size();
        E1Store::write_at_offset(address_domain, base, offset, e1)
    }
    #[inline(always)]
    fn size() -> u8 {
        E0Store::size() + E1Store::size()
    }
}

impl TupleSize3Store<
    E0,
    E1,
    E2,
    impl E0Store: Store<E0>,
    +Drop<E0>,
    impl E1Store: Store<E1>,
    +Drop<E1>,
    impl E2Store: Store<E2>,
    +Drop<E2>
> of Store<(E0, E1, E2)> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<(E0, E1, E2)> {
        let e0 = E0Store::read(address_domain, base)?;
        let mut offset = E0Store::size();
        let e1 = E1Store::read_at_offset(address_domain, base, offset)?;
        offset += E1Store::size();
        let e2 = E2Store::read_at_offset(address_domain, base, offset)?;
        Result::Ok((e0, e1, e2))
    }
    #[inline(always)]
    fn write(
        address_domain: u32, base: StorageBaseAddress, value: (E0, E1, E2)
    ) -> SyscallResult<()> {
        let (e0, e1, e2) = value;
        E0Store::write(address_domain, base, e0)?;
        let mut offset = E0Store::size();
        E1Store::write_at_offset(address_domain, base, offset, e1)?;
        offset += E1Store::size();
        E2Store::write_at_offset(address_domain, base, offset, e2)
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, mut offset: u8
    ) -> SyscallResult<(E0, E1, E2)> {
        let e0 = E0Store::read_at_offset(address_domain, base, offset)?;
        offset += E0Store::size();
        let e1 = E1Store::read_at_offset(address_domain, base, offset)?;
        offset += E1Store::size();
        let e2 = E2Store::read_at_offset(address_domain, base, offset)?;
        Result::Ok((e0, e1, e2))
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, mut offset: u8, value: (E0, E1, E2)
    ) -> SyscallResult<()> {
        let (e0, e1, e2) = value;
        E0Store::write_at_offset(address_domain, base, offset, e0)?;
        offset += E0Store::size();
        E1Store::write_at_offset(address_domain, base, offset, e1)?;
        offset += E1Store::size();
        E2Store::write_at_offset(address_domain, base, offset, e2)
    }
    #[inline(always)]
    fn size() -> u8 {
        E0Store::size() + E1Store::size() + E2Store::size()
    }
}

impl TupleSize4Store<
    E0,
    E1,
    E2,
    E3,
    impl E0Store: Store<E0>,
    +Drop<E0>,
    impl E1Store: Store<E1>,
    +Drop<E1>,
    impl E2Store: Store<E2>,
    +Drop<E2>,
    impl E3Store: Store<E3>,
    +Drop<E3>
> of Store<(E0, E1, E2, E3)> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<(E0, E1, E2, E3)> {
        let e0 = E0Store::read(address_domain, base)?;
        let mut offset = E0Store::size();
        let e1 = E1Store::read_at_offset(address_domain, base, offset)?;
        offset += E1Store::size();
        let e2 = E2Store::read_at_offset(address_domain, base, offset)?;
        offset += E2Store::size();
        let e3 = E3Store::read_at_offset(address_domain, base, offset)?;
        Result::Ok((e0, e1, e2, e3))
    }
    #[inline(always)]
    fn write(
        address_domain: u32, base: StorageBaseAddress, value: (E0, E1, E2, E3)
    ) -> SyscallResult<()> {
        let (e0, e1, e2, e3) = value;
        E0Store::write(address_domain, base, e0)?;
        let mut offset = E0Store::size();
        E1Store::write_at_offset(address_domain, base, offset, e1)?;
        offset += E1Store::size();
        E2Store::write_at_offset(address_domain, base, offset, e2)?;
        offset += E2Store::size();
        E3Store::write_at_offset(address_domain, base, offset, e3)
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, mut offset: u8
    ) -> SyscallResult<(E0, E1, E2, E3)> {
        let e0 = E0Store::read_at_offset(address_domain, base, offset)?;
        offset += E0Store::size();
        let e1 = E1Store::read_at_offset(address_domain, base, offset)?;
        offset += E1Store::size();
        let e2 = E2Store::read_at_offset(address_domain, base, offset)?;
        offset += E2Store::size();
        let e3 = E3Store::read_at_offset(address_domain, base, offset)?;
        Result::Ok((e0, e1, e2, e3))
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, mut offset: u8, value: (E0, E1, E2, E3)
    ) -> SyscallResult<()> {
        let (e0, e1, e2, e3) = value;
        E0Store::write_at_offset(address_domain, base, offset, e0)?;
        offset += E0Store::size();
        E1Store::write_at_offset(address_domain, base, offset, e1)?;
        offset += E1Store::size();
        E2Store::write_at_offset(address_domain, base, offset, e2)?;
        offset += E2Store::size();
        E3Store::write_at_offset(address_domain, base, offset, e3)
    }
    #[inline(always)]
    fn size() -> u8 {
        E0Store::size() + E1Store::size() + E2Store::size() + E3Store::size()
    }
}


impl ResultStore<T, E, +Store<T>, +Store<E>, +Drop<T>, +Drop<E>> of Store<Result<T, E>> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<Result<T, E>> {
        let idx = Store::<felt252>::read(address_domain, base)?;
        if idx == 0 {
            starknet::SyscallResult::Ok(
                Result::Ok(Store::read_at_offset(address_domain, base, 1_u8)?)
            )
        } else if idx == 1 {
            starknet::SyscallResult::Ok(
                Result::Err(Store::read_at_offset(address_domain, base, 1_u8)?)
            )
        } else {
            starknet::SyscallResult::Err(array!['Incorrect index:'])
        }
    }
    #[inline(always)]
    fn write(
        address_domain: u32, base: StorageBaseAddress, value: Result<T, E>
    ) -> SyscallResult<()> {
        match value {
            Result::Ok(x) => {
                Store::write(address_domain, base, 0)?;
                Store::write_at_offset(address_domain, base, 1_u8, x)?;
            },
            Result::Err(x) => {
                Store::write(address_domain, base, 1)?;
                Store::write_at_offset(address_domain, base, 1_u8, x)?;
            }
        };
        starknet::SyscallResult::Ok(())
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<Result<T, E>> {
        let idx = Store::<felt252>::read_at_offset(address_domain, base, offset)?;
        if idx == 0 {
            starknet::SyscallResult::Ok(
                Result::Ok(Store::read_at_offset(address_domain, base, offset + 1_u8)?)
            )
        } else if idx == 1 {
            starknet::SyscallResult::Ok(
                Result::Err(Store::read_at_offset(address_domain, base, offset + 1_u8)?)
            )
        } else {
            starknet::SyscallResult::Err(array!['Incorrect index:'])
        }
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: Result<T, E>
    ) -> SyscallResult<()> {
        match value {
            Result::Ok(x) => {
                Store::write(address_domain, base, 0)?;
                Store::write_at_offset(address_domain, base, 1_u8, x)?;
            },
            Result::Err(x) => {
                Store::write(address_domain, base, 1)?;
                Store::write_at_offset(address_domain, base, 1_u8, x)?;
            }
        };
        starknet::SyscallResult::Ok(())
    }
    #[inline(always)]
    fn size() -> u8 {
        1 + core::cmp::max(Store::<T>::size(), Store::<E>::size())
    }
}

impl OptionStore<T, +Store<T>, +Drop<T>,> of Store<Option<T>> {
    #[inline(always)]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<Option<T>> {
        let idx = Store::<felt252>::read(address_domain, base)?;
        if idx == 1 {
            starknet::SyscallResult::Ok(
                Option::Some(Store::read_at_offset(address_domain, base, 1_u8)?)
            )
        } else if idx == 0 {
            starknet::SyscallResult::Ok(Option::None)
        } else {
            starknet::SyscallResult::Err(array!['Incorrect index:'])
        }
    }
    #[inline(always)]
    fn write(address_domain: u32, base: StorageBaseAddress, value: Option<T>) -> SyscallResult<()> {
        match value {
            Option::Some(x) => {
                Store::write(address_domain, base, 1)?;
                Store::write_at_offset(address_domain, base, 1_u8, x)?;
            },
            Option::None(_) => { Store::write(address_domain, base, 0)?; }
        };
        starknet::SyscallResult::Ok(())
    }
    #[inline(always)]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8
    ) -> SyscallResult<Option<T>> {
        let idx = Store::<felt252>::read_at_offset(address_domain, base, offset)?;
        if idx == 1 {
            starknet::SyscallResult::Ok(
                Option::Some(Store::read_at_offset(address_domain, base, offset + 1_u8)?)
            )
        } else if idx == 0 {
            starknet::SyscallResult::Ok(Option::None)
        } else {
            starknet::SyscallResult::Err(array!['Incorrect index:'])
        }
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: Option<T>
    ) -> SyscallResult<()> {
        match value {
            Option::Some(x) => {
                Store::write(address_domain, base, 1)?;
                Store::write_at_offset(address_domain, base, 1_u8, x)?;
            },
            Option::None(x) => { Store::write(address_domain, base, 0)?; }
        };
        starknet::SyscallResult::Ok(())
    }
    #[inline(always)]
    fn size() -> u8 {
        1 + Store::<T>::size()
    }
}
