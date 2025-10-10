//! Storage access primitives for Starknet contract storage.
//!
//! This module provides abstractions over the system calls for reading from and writing to Starknet
//! contract storage. It includes traits and implementations for storing various data types
//! efficiently.
//!
//! # Storage Architecture
//!
//! * Storage addresses range from `[0, 2^251)`
//! * Base addresses can be combined with offsets, allowing storage of up to 255 values sequentially
//! * Multiple storage domains can be supported, each with its own set of storage space.
//! Currently, only the domain `0` is supported. Values stored in domain `0` are committed to
//! Ethereum as part of the state diffs.
//!
//! # Core Components
//!
//! * [`StorageAddress`]: Represents a specific storage location
//! * [`StorageBaseAddress`]: Base address that can be combined with offsets
//! * [`Store<T>`]: Core trait for types that can be stored in contract storage
//! * [`StorePacking<T,P>`]: Trait for efficient packing/unpacking of values
//!
//! Generally, you don't need to implement the [`Store`] trait yourself. Most types of the core
//! library, at the exception of collection types, implement the [`Store`] trait - and thus, you can
//! derive the [`Store`] trait for your own types, as long as they don't contain any collections.

use core::RangeCheck;
use core::array::ArrayTrait;
use core::byte_array::ByteArrayTrait;
use core::option::OptionTrait;
use core::serde::Serde;
use core::traits::{Into, TryInto};
#[allow(unused_imports)]
use starknet::SyscallResult;
#[allow(unused_imports)]
use starknet::class_hash::{ClassHash, ClassHashIntoFelt252, Felt252TryIntoClassHash};
#[allow(unused_imports)]
use starknet::contract_address::{
    ContractAddress, ContractAddressIntoFelt252, Felt252TryIntoContractAddress,
};
#[allow(unused_imports)]
use starknet::syscalls::{storage_read_syscall, storage_write_syscall};

/// Represents the address of a storage value in a Starknet contract.
/// The value range of this type is `[0, 2**251)`.
pub extern type StorageAddress;

impl StorageAddressCopy of Copy<StorageAddress>;
impl StorageAddressDrop of Drop<StorageAddress>;

/// Represents a base storage address that can be combined with offsets.
/// The value range of this type is `[0, 2**251 - 256)`.
pub extern type StorageBaseAddress;

impl StorageBaseAddressCopy of Copy<StorageBaseAddress>;
impl StorageBaseAddressDrop of Drop<StorageBaseAddress>;

/// Returns a `StorageBaseAddress` given a constant `felt252` value.
///
/// The value is validated to be in the range `[0, 2**251 - 256)` at compile time.
///
/// # Examples
///
/// ```
/// use starknet::storage_access::storage_base_address_const;
///
/// let base_address = storage_base_address_const::<0>();
/// ```
pub extern fn storage_base_address_const<const address: felt252>() -> StorageBaseAddress nopanic;

/// Returns a `StorageBaseAddress` given a `felt252` value.
///
/// Wraps around the value if it is not in the range `[0, 2**251 - 256)`.
pub extern fn storage_base_address_from_felt252(
    addr: felt252,
) -> StorageBaseAddress implicits(RangeCheck) nopanic;

pub(crate) extern fn storage_address_to_felt252(address: StorageAddress) -> felt252 nopanic;

/// Sums the base address and the offset to return a storage address.
pub extern fn storage_address_from_base_and_offset(
    base: StorageBaseAddress, offset: u8,
) -> StorageAddress nopanic;

/// Converts a `StorageBaseAddress` into a `StorageAddress`.
///
/// This should be used through the high-level `Into` trait.
pub extern fn storage_address_from_base(base: StorageBaseAddress) -> StorageAddress nopanic;

pub(crate) extern fn storage_address_try_from_felt252(
    address: felt252,
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
        Some(storage_address_try_from_felt252(Serde::<felt252>::deserialize(ref serialized)?)?)
    }
}

impl StorageBaseAddressIntoFelt252 of Into<StorageBaseAddress, felt252> {
    fn into(self: StorageBaseAddress) -> felt252 {
        storage_address_to_felt252(storage_address_from_base(self))
    }
}

impl DebugStorageAddress = core::fmt::into_felt252_based::DebugImpl<StorageAddress>;
impl DebugStorageBaseAddress of core::fmt::Debug<StorageBaseAddress> {
    fn fmt(self: @StorageBaseAddress, ref f: core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        DebugStorageAddress::fmt(@storage_address_from_base(*self), ref f)
    }
}

impl LowerHexStorageAddress = core::fmt::into_felt252_based::LowerHexImpl<StorageAddress>;
impl LowerHexStorageBaseAddress of core::fmt::LowerHex<StorageBaseAddress> {
    fn fmt(self: @StorageBaseAddress, ref f: core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        LowerHexStorageAddress::fmt(@storage_address_from_base(*self), ref f)
    }
}

/// Trait for types that can be stored in Starknet contract storage.
///
/// The `Store` trait enables types to be stored in and retrieved from Starknet's contract storage.
/// Cairo implements `Store` for most primitive types. However, collection types (arrays, dicts,
/// etc.) do not implement `Store` directly. Instead, use specialized storage types, such as [`Vec`]
/// or [`Map`].
///
/// [`Map`]: starknet::storage::Map
/// [`Vec`]: starknet::storage::Vec
///
/// # Derivation
///
/// To make a type storable in contract storage, simply derive the `Store` trait:
///
/// ```
/// #[derive(Drop, starknet::Store)]
/// struct Sizes {
///     tiny: u8,    // 8 bits
///     small: u32,  // 32 bits
///     medium: u64, // 64 bits
/// }
/// ```
///
/// This allows the `Size` struct to be stored in a contract's storage.
///
/// There's no real reason to implement this trait yourself, as it can be trivially derived.
/// For efficiency purposes, consider manually implementing [`StorePacking`] to optimize storage
/// usage.
pub trait Store<T> {
    /// Reads a value from storage at the given domain and base address.
    ///
    /// # Arguments
    ///
    /// * `address_domain` - The storage domain (currently only 0 is supported)
    /// * `base` - The base storage address to read from
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<T>;

    /// Writes a value to storage at the given domain and base address.
    ///
    /// # Arguments
    ///
    /// * `address_domain` - The storage domain (currently only 0 is supported)
    /// * `base` - The base storage address to write to
    /// * `value` - The value to store
    fn write(address_domain: u32, base: StorageBaseAddress, value: T) -> SyscallResult<()>;

    /// Reads a value from storage at a base address plus an offset.
    ///
    /// # Arguments
    ///
    /// * `address_domain` - The storage domain (currently only 0 is supported)
    /// * `base` - The base storage address
    /// * `offset` - The offset from the base address where the value should be read
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8,
    ) -> SyscallResult<T>;

    /// Writes a value to storage at a base address plus an offset.
    ///
    /// # Arguments
    ///
    /// * `address_domain` - The storage domain (currently only 0 is supported)
    /// * `base` - The base storage address
    /// * `offset` - The offset from the base address where the value should be written
    /// * `value` - The value to store
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: T,
    ) -> SyscallResult<()>;

    /// Returns the size in storage for this type.
    ///
    /// This is bounded to 255, as the offset is a u8. As such, a single type can only take up to
    /// 255 slots in storage.
    fn size() -> u8;

    /// Clears the storage area by writing zeroes to it.
    ///
    /// # Arguments
    ///
    /// * `address_domain` - The storage domain
    /// * `base` - The base storage address to start clearing
    /// * `offset` - The offset from the base address where clearing should start
    ///
    /// The operation writes zeroes to storage starting from the specified base address and offset,
    /// and continues for the size of the type as determined by the `size()` function.
    #[inline]
    fn scrub(
        address_domain: u32, base: StorageBaseAddress, offset: u8,
    ) -> SyscallResult<
        (),
    > {
        let mut result = Result::Ok(());
        let mut offset = offset;
        for _ in 0..Self::size() {
            if let Result::Err(err) =
                storage_write_syscall(
                    address_domain, storage_address_from_base_and_offset(base, offset), 0,
                ) {
                result = Result::Err(err);
                break;
            }
            offset += 1;
        }
        result
    }
}

/// Trait for efficient packing of values into optimized storage representations.
///
/// This trait enables bit-packing of complex types into simpler storage types to reduce gas costs
/// by minimizing the number of storage slots used. When a type implements `StorePacking`, the
/// compiler automatically uses [`StoreUsingPacking`] to handle storage operations. As such, a type
/// cannot implement both `Store` and `StorePacking`.
///
/// # Storage Optimization
///
/// Each storage slot in Starknet is a `felt252`, and storage operations are expensive. By packing
/// multiple values into fewer slots, you can significantly reduce gas costs. For example:
/// - Multiple small integers can be packed into a single `felt252`
/// - Structs with several fields can be compressed into a single storage slot
///
/// # Implementation Requirements
///
/// To implement `StorePacking`, ensure that the `PackedT` type implements [`Store`]. The packed
/// representation must preserve all necessary information to allow unpacking back to the original
/// type. Additionally, the `pack` and `unpack` operations must be reversible, meaning that packing
/// followed by unpacking should return the original value.
///
/// # Example
///
/// Packing multiple integer fields into a single storage slot:
///
/// ```
/// use starknet::storage_access::StorePacking;
///
/// #[derive(Drop)]
/// struct Sizes {
///     tiny: u8,    // 8 bits
///     small: u32,  // 32 bits
///     medium: u64, // 64 bits
/// }
///
/// const TWO_POW_8: u128 = 0x100;
/// const TWO_POW_40: u128 = 0x10000000000;
///
/// impl SizesStorePacking of StorePacking<Sizes, u128> {
///     fn pack(value: Sizes) -> u128 {
///         value.tiny.into() +
///         (value.small.into() * TWO_POW_8) +
///         (value.medium.into() * TWO_POW_40)
///     }
///
///     fn unpack(value: u128) -> Sizes {
///         let tiny = value & 0xff;
///         let small = (value / TWO_POW_8) & 0xffffffff;
///         let medium = (value / TWO_POW_40);
///
///         Sizes {
///             tiny: tiny.try_into().unwrap(),
///             small: small.try_into().unwrap(),
///             medium: medium.try_into().unwrap(),
///         }
///     }
/// }
/// ```
///
/// By implementing `StorePacking` for `Sizes`, the `Sizes` will be stored in its packed form,
/// using a single storage slot instead of 3. When retrieved, it will automatically be unpacked back
/// into the original type.
pub trait StorePacking<T, PackedT> {
    /// Packs a value into its optimized storage representation.
    fn pack(value: T) -> PackedT;

    /// Unpacks a storage representation back into the original type.
    fn unpack(value: PackedT) -> T;
}

impl StoreUsingPacking<
    T, PackedT, impl TPacking: StorePacking<T, PackedT>, impl PackedTStore: Store<PackedT>,
> of Store<T> {
    #[inline]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<T> {
        Ok(TPacking::unpack(PackedTStore::read(address_domain, base)?))
    }

    #[inline]
    fn write(address_domain: u32, base: StorageBaseAddress, value: T) -> SyscallResult<()> {
        PackedTStore::write(address_domain, base, TPacking::pack(value))
    }

    #[inline]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8,
    ) -> SyscallResult<T> {
        Ok(TPacking::unpack(PackedTStore::read_at_offset(address_domain, base, offset)?))
    }

    #[inline]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: T,
    ) -> SyscallResult<()> {
        PackedTStore::write_at_offset(address_domain, base, offset, TPacking::pack(value))
    }

    #[inline]
    fn size() -> u8 {
        PackedTStore::size()
    }
}

impl StoreFelt252 of Store<felt252> {
    #[inline]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<felt252> {
        storage_read_syscall(address_domain, storage_address_from_base(base))
    }

    #[inline]
    fn write(address_domain: u32, base: StorageBaseAddress, value: felt252) -> SyscallResult<()> {
        storage_write_syscall(address_domain, storage_address_from_base(base), value)
    }

    #[inline]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8,
    ) -> SyscallResult<felt252> {
        storage_read_syscall(address_domain, storage_address_from_base_and_offset(base, offset))
    }

    #[inline]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: felt252,
    ) -> SyscallResult<()> {
        storage_write_syscall(
            address_domain, storage_address_from_base_and_offset(base, offset), value,
        )
    }

    #[inline]
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

impl StorePackingU256 of StorePacking<u256, (u128, u128)> {
    fn pack(value: u256) -> (u128, u128) {
        (value.low, value.high)
    }

    #[inline]
    fn unpack(value: (u128, u128)) -> u256 {
        let (low, high) = value;
        u256 { low, high }
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

impl StorePackingNonZero<T, +TryInto<T, NonZero<T>>> of StorePacking<NonZero<T>, T> {
    fn pack(value: NonZero<T>) -> T {
        value.into()
    }

    #[inline]
    fn unpack(value: T) -> NonZero<T> {
        value.try_into().expect('StoreNonZero - zero value')
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

/// Store implementation for a tuple of size 0.
impl TupleSize0Store of Store<()> {
    #[inline]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<()> {
        Ok(())
    }

    #[inline]
    fn write(address_domain: u32, base: StorageBaseAddress, value: ()) -> SyscallResult<()> {
        Ok(())
    }

    #[inline]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8,
    ) -> SyscallResult<()> {
        Ok(())
    }

    #[inline]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: (),
    ) -> SyscallResult<()> {
        Ok(())
    }

    #[inline]
    fn size() -> u8 {
        0
    }
}

/// Store packing for tuples of size 1.
impl StorePackingTuple1<T> of StorePacking<(T,), T> {
    fn pack(value: (T,)) -> T {
        let (value,) = value;
        value
    }

    fn unpack(value: T) -> (T,) {
        (value,)
    }
}

/// Store packing for small fixed sized arrays.
impl StorePackingFixedSizedArray0<T> of StorePacking<[T; 0], ()> {
    fn pack(value: [T; 0]) -> () {
        let [] = value;
        ()
    }

    #[inline]
    fn unpack(value: ()) -> [T; 0] {
        []
    }
}

/// Store packing for fixed sized arrays of size 1.
impl StorePackingFixedSizedArray1<T> of StorePacking<[T; 1], T> {
    fn pack(value: [T; 1]) -> T {
        let [value] = value;
        value
    }

    fn unpack(value: T) -> [T; 1] {
        [value]
    }
}

/// Store implementation for a tuple of size 2 and more.
impl TupleNextStore<
    T,
    impl TH: core::metaprogramming::TupleSplit<T>,
    impl HeadStore: Store<TH::Head>,
    impl RestStore: Store<TH::Rest>,
    +Drop<TH::Head>,
    +Drop<TH::Rest>,
    // The following bound is to allow the recursion to be more efficient at size 1.
    +core::metaprogramming::TupleSplit<TH::Rest>,
> of Store<T> {
    #[inline]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<T> {
        let head = HeadStore::read(address_domain, base)?;
        let rest = RestStore::read_at_offset(address_domain, base, HeadStore::size())?;
        Ok(TH::reconstruct(head, rest))
    }

    #[inline]
    fn write(address_domain: u32, base: StorageBaseAddress, value: T) -> SyscallResult<()> {
        let (head, rest) = TH::split_head(value);
        HeadStore::write(address_domain, base, head)?;
        RestStore::write_at_offset(address_domain, base, HeadStore::size(), rest)
    }

    #[inline]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8,
    ) -> SyscallResult<T> {
        let head = HeadStore::read_at_offset(address_domain, base, offset)?;
        let rest = RestStore::read_at_offset(address_domain, base, offset + HeadStore::size())?;
        Ok(TH::reconstruct(head, rest))
    }

    #[inline]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: T,
    ) -> SyscallResult<()> {
        let (head, rest) = TH::split_head(value);
        HeadStore::write_at_offset(address_domain, base, offset, head)?;
        RestStore::write_at_offset(address_domain, base, offset + HeadStore::size(), rest)
    }

    #[inline]
    fn size() -> u8 {
        HeadStore::size() + RestStore::size()
    }
}

impl ResultStore<T, E, +Store<T>, +Store<E>, +Drop<T>, +Drop<E>> of Store<Result<T, E>> {
    #[inline]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<Result<T, E>> {
        let idx = Store::<felt252>::read(address_domain, base)?;
        if idx == 0 {
            starknet::SyscallResult::Ok(Ok(Store::read_at_offset(address_domain, base, 1_u8)?))
        } else if idx == 1 {
            starknet::SyscallResult::Ok(Err(Store::read_at_offset(address_domain, base, 1_u8)?))
        } else {
            starknet::SyscallResult::Err(array!['Incorrect index:'])
        }
    }

    #[inline]
    fn write(
        address_domain: u32, base: StorageBaseAddress, value: Result<T, E>,
    ) -> SyscallResult<()> {
        match value {
            Ok(x) => {
                Store::write(address_domain, base, 0)?;
                Store::write_at_offset(address_domain, base, 1_u8, x)?;
            },
            Err(x) => {
                Store::write(address_domain, base, 1)?;
                Store::write_at_offset(address_domain, base, 1_u8, x)?;
            },
        }
        starknet::SyscallResult::Ok(())
    }

    #[inline]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8,
    ) -> SyscallResult<Result<T, E>> {
        let idx = Store::<felt252>::read_at_offset(address_domain, base, offset)?;
        if idx == 0 {
            starknet::SyscallResult::Ok(
                Ok(Store::read_at_offset(address_domain, base, offset + 1_u8)?),
            )
        } else if idx == 1 {
            starknet::SyscallResult::Ok(
                Err(Store::read_at_offset(address_domain, base, offset + 1_u8)?),
            )
        } else {
            starknet::SyscallResult::Err(array!['Incorrect index:'])
        }
    }

    #[inline]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: Result<T, E>,
    ) -> SyscallResult<()> {
        match value {
            Ok(x) => {
                Store::write_at_offset(address_domain, base, offset, 0)?;
                Store::write_at_offset(address_domain, base, offset + 1_u8, x)?;
            },
            Err(x) => {
                Store::write_at_offset(address_domain, base, offset, 0)?;
                Store::write_at_offset(address_domain, base, offset + 1_u8, x)?;
            },
        }
        starknet::SyscallResult::Ok(())
    }

    #[inline]
    fn size() -> u8 {
        1 + core::cmp::max(Store::<T>::size(), Store::<E>::size())
    }
}

impl OptionStore<T, +Store<T>, +Drop<T>> of Store<Option<T>> {
    #[inline]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<Option<T>> {
        let idx = Store::<felt252>::read(address_domain, base)?;
        if idx == 1 {
            starknet::SyscallResult::Ok(Some(Store::read_at_offset(address_domain, base, 1_u8)?))
        } else if idx == 0 {
            starknet::SyscallResult::Ok(None)
        } else {
            starknet::SyscallResult::Err(array!['Incorrect index:'])
        }
    }

    #[inline]
    fn write(address_domain: u32, base: StorageBaseAddress, value: Option<T>) -> SyscallResult<()> {
        match value {
            Some(x) => {
                Store::write(address_domain, base, 1)?;
                Store::write_at_offset(address_domain, base, 1_u8, x)?;
            },
            None(_) => { Store::write(address_domain, base, 0)?; },
        }
        starknet::SyscallResult::Ok(())
    }

    #[inline]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8,
    ) -> SyscallResult<Option<T>> {
        let idx = Store::<felt252>::read_at_offset(address_domain, base, offset)?;
        if idx == 1 {
            starknet::SyscallResult::Ok(
                Some(Store::read_at_offset(address_domain, base, offset + 1_u8)?),
            )
        } else if idx == 0 {
            starknet::SyscallResult::Ok(None)
        } else {
            starknet::SyscallResult::Err(array!['Incorrect index:'])
        }
    }

    #[inline]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: Option<T>,
    ) -> SyscallResult<()> {
        match value {
            Some(x) => {
                Store::write_at_offset(address_domain, base, offset, 1)?;
                Store::write_at_offset(address_domain, base, offset + 1_u8, x)?;
            },
            None(_x) => { Store::write_at_offset(address_domain, base, offset, 0)?; },
        }
        starknet::SyscallResult::Ok(())
    }

    #[inline]
    fn size() -> u8 {
        1 + Store::<T>::size()
    }
}

/// Store for a `ByteArray`.
///
/// The layout of a `ByteArray` in storage is as follows:
/// * Only the length in bytes is stored in the original address where the byte array is logically
///   stored.
/// * The actual data is stored in chunks of 256 `bytes31`s in another place in storage
///   determined by the hash of:
///   - The address storing the length of the array.
///   - The chunk index.
///   - The short string `ByteArray`.
impl ByteArrayStore of Store<ByteArray> {
    #[inline]
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<ByteArray> {
        inner_read_byte_array(address_domain, storage_address_from_base(base))
    }

    #[inline]
    fn write(address_domain: u32, base: StorageBaseAddress, value: ByteArray) -> SyscallResult<()> {
        inner_write_byte_array(address_domain, storage_address_from_base(base), value)
    }

    #[inline]
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8,
    ) -> SyscallResult<ByteArray> {
        inner_read_byte_array(address_domain, storage_address_from_base_and_offset(base, offset))
    }

    #[inline]
    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: ByteArray,
    ) -> SyscallResult<()> {
        inner_write_byte_array(
            address_domain, storage_address_from_base_and_offset(base, offset), value,
        )
    }

    #[inline]
    fn size() -> u8 {
        1
    }
}

/// Returns a pointer to the `chunk`'th chunk of the byte array at `address`.
/// The pointer is the `Poseidon` hash of:
/// * `address` - The address of the ByteArray (where the length is stored).
/// * `chunk` - The index of the chunk.
/// * The short string `ByteArray` is used as the capacity argument of the sponge construction
///   (domain separation).
fn inner_byte_array_pointer(address: StorageAddress, chunk: felt252) -> StorageBaseAddress {
    let (r, _, _) = core::poseidon::hades_permutation(address.into(), chunk, 'ByteArray'_felt252);
    storage_base_address_from_felt252(r)
}

/// Reads a byte array from storage from domain `address_domain` and address `address`.
/// The length of the byte array is read from `address` at domain `address_domain`.
/// For more info read the documentation of `ByteArrayStore`.
fn inner_read_byte_array(address_domain: u32, address: StorageAddress) -> SyscallResult<ByteArray> {
    let Some::<usize>(len) = starknet::syscalls::storage_read_syscall(address_domain, address)?
        .try_into() else {
        return Err(array!['Invalid ByteArray length']);
    };
    let (mut remaining_full_words, pending_word_len) = crate::byte_array::len_parts(len);
    let mut chunk = 0;
    let mut chunk_base = inner_byte_array_pointer(address, chunk);
    let mut index_in_chunk = 0_u8;
    let mut result: ByteArray = Default::default();
    loop {
        if remaining_full_words == 0 {
            break;
        }
        let value = starknet::syscalls::storage_read_syscall(
            address_domain, storage_address_from_base_and_offset(chunk_base, index_in_chunk),
        )?;
        let Some::<bytes31>(value) = value.try_into() else {
            return Err(array!['Invalid value']);
        };
        result.data.append(value);
        remaining_full_words -= 1;
        index_in_chunk = match core::integer::u8_overflowing_add(index_in_chunk, 1) {
            Ok(x) => x,
            Err(_) => {
                // After reading 256 `bytes31`s `index_in_chunk` will overflow and we move to the
                // next chunk.
                chunk += 1;
                chunk_base = inner_byte_array_pointer(address, chunk);
                0
            },
        };
    }
    if pending_word_len != 0 {
        let pending_word = starknet::syscalls::storage_read_syscall(
            address_domain, storage_address_from_base_and_offset(chunk_base, index_in_chunk),
        )?;
        if !core::byte_array::is_valid_pending_word(pending_word, pending_word_len) {
            return Err(array!['Invalid pending word']);
        }
        result.pending_word = pending_word;
        result.pending_word_len = pending_word_len;
    }
    Ok(result)
}

/// Writes a byte array to storage to domain `address_domain` and address `address`.
/// The length of the byte array is written to `address` at domain `address_domain`.
/// For more info read the documentation of `ByteArrayStore`.
fn inner_write_byte_array(
    address_domain: u32, address: StorageAddress, value: ByteArray,
) -> SyscallResult<()> {
    let len = value.len();
    starknet::syscalls::storage_write_syscall(address_domain, address, len.into())?;
    let mut full_words = value.data.span();
    let mut chunk = 0;
    let mut chunk_base = inner_byte_array_pointer(address, chunk);
    let mut index_in_chunk = 0_u8;
    loop {
        let curr_value = match full_words.pop_front() {
            Some(x) => x,
            None => { break Ok(()); },
        };
        match starknet::syscalls::storage_write_syscall(
            address_domain,
            storage_address_from_base_and_offset(chunk_base, index_in_chunk),
            (*curr_value).into(),
        ) {
            Ok(_) => {},
            Err(err) => { break Err(err); },
        }
        index_in_chunk = match core::integer::u8_overflowing_add(index_in_chunk, 1) {
            Ok(x) => x,
            Err(_) => {
                // After writing 256 `byte31`s `index_in_chunk` will overflow and we move to the
                // next chunk.
                chunk += 1;
                chunk_base = inner_byte_array_pointer(address, chunk);
                0
            },
        };
    }?;
    if value.pending_word_len != 0 {
        starknet::syscalls::storage_write_syscall(
            address_domain,
            storage_address_from_base_and_offset(chunk_base, index_in_chunk),
            value.pending_word,
        )?;
    }
    Ok(())
}
