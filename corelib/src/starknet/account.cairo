//! Account module defining the [`Call`] struct and the [`AccountContract`] trait.
//!
//! The `Call` struct represents a call to a contract, with the following fields:
//! - `to`: The address of the contract to call.
//! - `selector`: The entry point selector in the called contract.
//! - `calldata`: The calldata to pass to the entry point.
//!
//! The `AccountContract` trait defines the standard interface for account contracts. It assumes
//! that the calldata for invoke transactions is an `Array<Call>`, following the SNIP6 standard.
//!
//! Implementing this trait allows contracts to function as account contracts in the Starknet
//! network, supporting class declarations and batched call execution.

use starknet::ContractAddress;

/// A struct representing a call to a contract.
#[derive(Drop, Copy, Serde, Debug)]
pub struct Call {
    /// The address of the contract to call.
    pub to: ContractAddress,
    /// The entry point selector in the called contract.
    pub selector: felt252,
    /// The calldata to pass to the entry point.
    pub calldata: Span<felt252>,
}

/// A trait for account contracts that support class declarations (only `__validate__` and
/// `__execute__` are mandatory for an account).
///
/// This trait assumes that the calldata for invoke transactions is `Array<Call>`.
/// This is the network standard following SNIP6.
/// It is not enforced by Starknet, but deviating from the standard interface may lead to
/// incompatibility with standard tooling.
#[starknet::interface]
pub trait AccountContract<TContractState> {
    /// An entry point that is called to check if the account is willing to pay for the declaration
    /// of the class with the given hash.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the declaration.
    fn __validate_declare__(self: @TContractState, class_hash: felt252) -> felt252;

    /// An entry point that is called to check if the account is willing to pay for
    /// executing a given set of calls.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the execution, in which case `__execute__` will be called on the same set of calls.
    fn __validate__(ref self: TContractState, calls: Array<Call>) -> felt252;

    /// An entry point that is called to execute a given set of calls.
    /// This entry point should block the deprecated v0 invoke transactions as they do not go
    /// through the `__validate__` entry point.
    fn __execute__(ref self: TContractState, calls: Array<Call>) -> Array<Span<felt252>>;
}


// Generate code.

impl AccountContractDispatcherStore<> of starknet::Store<AccountContractDispatcher> {
    fn read(
        address_domain: u32, base: starknet::storage_access::StorageBaseAddress,
    ) -> starknet::SyscallResult<AccountContractDispatcher> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let contract_address = core::internal::InferDestruct::<
            starknet::ContractAddress,
        > {
            value: starknet::Store::<
                starknet::ContractAddress,
            >::read(__store_derive_address_domain__, __store_derive_base__)?,
        };
        starknet::SyscallResult::Ok(
            AccountContractDispatcher { contract_address: contract_address.value },
        )
    }
    fn write(
        address_domain: u32,
        base: starknet::storage_access::StorageBaseAddress,
        value: AccountContractDispatcher,
    ) -> starknet::SyscallResult<()> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let AccountContractDispatcher { contract_address } = value;
        let contract_address = core::internal::InferDestruct::<
            starknet::ContractAddress,
        > { value: contract_address };
        starknet::Store::<
            starknet::ContractAddress,
        >::write(__store_derive_address_domain__, __store_derive_base__, contract_address.value)?;
        starknet::SyscallResult::Ok(())
    }
    fn read_at_offset(
        address_domain: u32, base: starknet::storage_access::StorageBaseAddress, offset: u8,
    ) -> starknet::SyscallResult<AccountContractDispatcher> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let __store_derive_offset__ = offset;
        let contract_address = core::internal::InferDestruct::<
            starknet::ContractAddress,
        > {
            value: starknet::Store::<
                starknet::ContractAddress,
            >::read_at_offset(
                __store_derive_address_domain__, __store_derive_base__, __store_derive_offset__,
            )?,
        };
        starknet::SyscallResult::Ok(
            AccountContractDispatcher { contract_address: contract_address.value },
        )
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32,
        base: starknet::storage_access::StorageBaseAddress,
        offset: u8,
        value: AccountContractDispatcher,
    ) -> starknet::SyscallResult<()> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let __store_derive_offset__ = offset;
        let AccountContractDispatcher { contract_address } = value;
        let contract_address = core::internal::InferDestruct::<
            starknet::ContractAddress,
        > { value: contract_address };
        starknet::Store::<
            starknet::ContractAddress,
        >::write_at_offset(
            __store_derive_address_domain__,
            __store_derive_base__,
            __store_derive_offset__,
            contract_address.value,
        )?;
        starknet::SyscallResult::Ok(())
    }
    #[inline(always)]
    fn size() -> u8 {
        starknet::Store::<starknet::ContractAddress>::size()
    }
}

#[derive(Drop, Copy)]
#[doc(hidden)]
pub struct AccountContractDispatcherSubPointers {
    pub contract_address: starknet::storage::StoragePointer<starknet::ContractAddress>,
}
#[doc(hidden)]
impl AccountContractDispatcherSubPointersImpl of starknet::storage::SubPointers<
    AccountContractDispatcher,
> {
    type SubPointersType = AccountContractDispatcherSubPointers;
    fn sub_pointers(
        self: starknet::storage::StoragePointer<AccountContractDispatcher>,
    ) -> AccountContractDispatcherSubPointers {
        let base_address = self.__storage_pointer_address__;
        let mut current_offset = self.__storage_pointer_offset__;
        let __contract_address_value__ = starknet::storage::StoragePointer {
            __storage_pointer_address__: base_address, __storage_pointer_offset__: current_offset,
        };
        AccountContractDispatcherSubPointers { contract_address: __contract_address_value__ }
    }
}
#[derive(Drop, Copy)]
#[doc(hidden)]
pub struct AccountContractDispatcherSubPointersMut {
    pub contract_address: starknet::storage::StoragePointer<
        starknet::storage::Mutable<starknet::ContractAddress>,
    >,
}
#[doc(hidden)]
impl AccountContractDispatcherSubPointersMutImpl of starknet::storage::SubPointersMut<
    AccountContractDispatcher,
> {
    type SubPointersType = AccountContractDispatcherSubPointersMut;
    fn sub_pointers_mut(
        self: starknet::storage::StoragePointer<
            starknet::storage::Mutable<AccountContractDispatcher>,
        >,
    ) -> AccountContractDispatcherSubPointersMut {
        let base_address = self.__storage_pointer_address__;
        let mut current_offset = self.__storage_pointer_offset__;
        let __contract_address_value__ = starknet::storage::StoragePointer {
            __storage_pointer_address__: base_address, __storage_pointer_offset__: current_offset,
        };
        AccountContractDispatcherSubPointersMut { contract_address: __contract_address_value__ }
    }
}

impl AccountContractLibraryDispatcherStore<> of starknet::Store<AccountContractLibraryDispatcher> {
    fn read(
        address_domain: u32, base: starknet::storage_access::StorageBaseAddress,
    ) -> starknet::SyscallResult<AccountContractLibraryDispatcher> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let class_hash = core::internal::InferDestruct::<
            starknet::ClassHash,
        > {
            value: starknet::Store::<
                starknet::ClassHash,
            >::read(__store_derive_address_domain__, __store_derive_base__)?,
        };
        starknet::SyscallResult::Ok(
            AccountContractLibraryDispatcher { class_hash: class_hash.value },
        )
    }
    fn write(
        address_domain: u32,
        base: starknet::storage_access::StorageBaseAddress,
        value: AccountContractLibraryDispatcher,
    ) -> starknet::SyscallResult<()> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let AccountContractLibraryDispatcher { class_hash } = value;
        let class_hash = core::internal::InferDestruct::<starknet::ClassHash> { value: class_hash };
        starknet::Store::<
            starknet::ClassHash,
        >::write(__store_derive_address_domain__, __store_derive_base__, class_hash.value)?;
        starknet::SyscallResult::Ok(())
    }
    fn read_at_offset(
        address_domain: u32, base: starknet::storage_access::StorageBaseAddress, offset: u8,
    ) -> starknet::SyscallResult<AccountContractLibraryDispatcher> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let __store_derive_offset__ = offset;
        let class_hash = core::internal::InferDestruct::<
            starknet::ClassHash,
        > {
            value: starknet::Store::<
                starknet::ClassHash,
            >::read_at_offset(
                __store_derive_address_domain__, __store_derive_base__, __store_derive_offset__,
            )?,
        };
        starknet::SyscallResult::Ok(
            AccountContractLibraryDispatcher { class_hash: class_hash.value },
        )
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32,
        base: starknet::storage_access::StorageBaseAddress,
        offset: u8,
        value: AccountContractLibraryDispatcher,
    ) -> starknet::SyscallResult<()> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let __store_derive_offset__ = offset;
        let AccountContractLibraryDispatcher { class_hash } = value;
        let class_hash = core::internal::InferDestruct::<starknet::ClassHash> { value: class_hash };
        starknet::Store::<
            starknet::ClassHash,
        >::write_at_offset(
            __store_derive_address_domain__,
            __store_derive_base__,
            __store_derive_offset__,
            class_hash.value,
        )?;
        starknet::SyscallResult::Ok(())
    }
    #[inline(always)]
    fn size() -> u8 {
        starknet::Store::<starknet::ClassHash>::size()
    }
}

#[derive(Drop, Copy)]
#[doc(hidden)]
pub struct AccountContractLibraryDispatcherSubPointers {
    pub class_hash: starknet::storage::StoragePointer<starknet::ClassHash>,
}
#[doc(hidden)]
impl AccountContractLibraryDispatcherSubPointersImpl of starknet::storage::SubPointers<
    AccountContractLibraryDispatcher,
> {
    type SubPointersType = AccountContractLibraryDispatcherSubPointers;
    fn sub_pointers(
        self: starknet::storage::StoragePointer<AccountContractLibraryDispatcher>,
    ) -> AccountContractLibraryDispatcherSubPointers {
        let base_address = self.__storage_pointer_address__;
        let mut current_offset = self.__storage_pointer_offset__;
        let __class_hash_value__ = starknet::storage::StoragePointer {
            __storage_pointer_address__: base_address, __storage_pointer_offset__: current_offset,
        };
        AccountContractLibraryDispatcherSubPointers { class_hash: __class_hash_value__ }
    }
}
#[derive(Drop, Copy)]
#[doc(hidden)]
pub struct AccountContractLibraryDispatcherSubPointersMut {
    pub class_hash: starknet::storage::StoragePointer<
        starknet::storage::Mutable<starknet::ClassHash>,
    >,
}
#[doc(hidden)]
impl AccountContractLibraryDispatcherSubPointersMutImpl of starknet::storage::SubPointersMut<
    AccountContractLibraryDispatcher,
> {
    type SubPointersType = AccountContractLibraryDispatcherSubPointersMut;
    fn sub_pointers_mut(
        self: starknet::storage::StoragePointer<
            starknet::storage::Mutable<AccountContractLibraryDispatcher>,
        >,
    ) -> AccountContractLibraryDispatcherSubPointersMut {
        let base_address = self.__storage_pointer_address__;
        let mut current_offset = self.__storage_pointer_offset__;
        let __class_hash_value__ = starknet::storage::StoragePointer {
            __storage_pointer_address__: base_address, __storage_pointer_offset__: current_offset,
        };
        AccountContractLibraryDispatcherSubPointersMut { class_hash: __class_hash_value__ }
    }
}

impl AccountContractSafeLibraryDispatcherStore<> of starknet::Store<
    AccountContractSafeLibraryDispatcher,
> {
    fn read(
        address_domain: u32, base: starknet::storage_access::StorageBaseAddress,
    ) -> starknet::SyscallResult<AccountContractSafeLibraryDispatcher> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let class_hash = core::internal::InferDestruct::<
            starknet::ClassHash,
        > {
            value: starknet::Store::<
                starknet::ClassHash,
            >::read(__store_derive_address_domain__, __store_derive_base__)?,
        };
        starknet::SyscallResult::Ok(
            AccountContractSafeLibraryDispatcher { class_hash: class_hash.value },
        )
    }
    fn write(
        address_domain: u32,
        base: starknet::storage_access::StorageBaseAddress,
        value: AccountContractSafeLibraryDispatcher,
    ) -> starknet::SyscallResult<()> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let AccountContractSafeLibraryDispatcher { class_hash } = value;
        let class_hash = core::internal::InferDestruct::<starknet::ClassHash> { value: class_hash };
        starknet::Store::<
            starknet::ClassHash,
        >::write(__store_derive_address_domain__, __store_derive_base__, class_hash.value)?;
        starknet::SyscallResult::Ok(())
    }
    fn read_at_offset(
        address_domain: u32, base: starknet::storage_access::StorageBaseAddress, offset: u8,
    ) -> starknet::SyscallResult<AccountContractSafeLibraryDispatcher> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let __store_derive_offset__ = offset;
        let class_hash = core::internal::InferDestruct::<
            starknet::ClassHash,
        > {
            value: starknet::Store::<
                starknet::ClassHash,
            >::read_at_offset(
                __store_derive_address_domain__, __store_derive_base__, __store_derive_offset__,
            )?,
        };
        starknet::SyscallResult::Ok(
            AccountContractSafeLibraryDispatcher { class_hash: class_hash.value },
        )
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32,
        base: starknet::storage_access::StorageBaseAddress,
        offset: u8,
        value: AccountContractSafeLibraryDispatcher,
    ) -> starknet::SyscallResult<()> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let __store_derive_offset__ = offset;
        let AccountContractSafeLibraryDispatcher { class_hash } = value;
        let class_hash = core::internal::InferDestruct::<starknet::ClassHash> { value: class_hash };
        starknet::Store::<
            starknet::ClassHash,
        >::write_at_offset(
            __store_derive_address_domain__,
            __store_derive_base__,
            __store_derive_offset__,
            class_hash.value,
        )?;
        starknet::SyscallResult::Ok(())
    }
    #[inline(always)]
    fn size() -> u8 {
        starknet::Store::<starknet::ClassHash>::size()
    }
}

#[derive(Drop, Copy)]
#[doc(hidden)]
pub struct AccountContractSafeLibraryDispatcherSubPointers {
    pub class_hash: starknet::storage::StoragePointer<starknet::ClassHash>,
}
#[doc(hidden)]
impl AccountContractSafeLibraryDispatcherSubPointersImpl of starknet::storage::SubPointers<
    AccountContractSafeLibraryDispatcher,
> {
    type SubPointersType = AccountContractSafeLibraryDispatcherSubPointers;
    fn sub_pointers(
        self: starknet::storage::StoragePointer<AccountContractSafeLibraryDispatcher>,
    ) -> AccountContractSafeLibraryDispatcherSubPointers {
        let base_address = self.__storage_pointer_address__;
        let mut current_offset = self.__storage_pointer_offset__;
        let __class_hash_value__ = starknet::storage::StoragePointer {
            __storage_pointer_address__: base_address, __storage_pointer_offset__: current_offset,
        };
        AccountContractSafeLibraryDispatcherSubPointers { class_hash: __class_hash_value__ }
    }
}
#[derive(Drop, Copy)]
#[doc(hidden)]
pub struct AccountContractSafeLibraryDispatcherSubPointersMut {
    pub class_hash: starknet::storage::StoragePointer<
        starknet::storage::Mutable<starknet::ClassHash>,
    >,
}
#[doc(hidden)]
impl AccountContractSafeLibraryDispatcherSubPointersMutImpl of starknet::storage::SubPointersMut<
    AccountContractSafeLibraryDispatcher,
> {
    type SubPointersType = AccountContractSafeLibraryDispatcherSubPointersMut;
    fn sub_pointers_mut(
        self: starknet::storage::StoragePointer<
            starknet::storage::Mutable<AccountContractSafeLibraryDispatcher>,
        >,
    ) -> AccountContractSafeLibraryDispatcherSubPointersMut {
        let base_address = self.__storage_pointer_address__;
        let mut current_offset = self.__storage_pointer_offset__;
        let __class_hash_value__ = starknet::storage::StoragePointer {
            __storage_pointer_address__: base_address, __storage_pointer_offset__: current_offset,
        };
        AccountContractSafeLibraryDispatcherSubPointersMut { class_hash: __class_hash_value__ }
    }
}

impl AccountContractSafeDispatcherStore<> of starknet::Store<AccountContractSafeDispatcher> {
    fn read(
        address_domain: u32, base: starknet::storage_access::StorageBaseAddress,
    ) -> starknet::SyscallResult<AccountContractSafeDispatcher> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let contract_address = core::internal::InferDestruct::<
            starknet::ContractAddress,
        > {
            value: starknet::Store::<
                starknet::ContractAddress,
            >::read(__store_derive_address_domain__, __store_derive_base__)?,
        };
        starknet::SyscallResult::Ok(
            AccountContractSafeDispatcher { contract_address: contract_address.value },
        )
    }
    fn write(
        address_domain: u32,
        base: starknet::storage_access::StorageBaseAddress,
        value: AccountContractSafeDispatcher,
    ) -> starknet::SyscallResult<()> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let AccountContractSafeDispatcher { contract_address } = value;
        let contract_address = core::internal::InferDestruct::<
            starknet::ContractAddress,
        > { value: contract_address };
        starknet::Store::<
            starknet::ContractAddress,
        >::write(__store_derive_address_domain__, __store_derive_base__, contract_address.value)?;
        starknet::SyscallResult::Ok(())
    }
    fn read_at_offset(
        address_domain: u32, base: starknet::storage_access::StorageBaseAddress, offset: u8,
    ) -> starknet::SyscallResult<AccountContractSafeDispatcher> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let __store_derive_offset__ = offset;
        let contract_address = core::internal::InferDestruct::<
            starknet::ContractAddress,
        > {
            value: starknet::Store::<
                starknet::ContractAddress,
            >::read_at_offset(
                __store_derive_address_domain__, __store_derive_base__, __store_derive_offset__,
            )?,
        };
        starknet::SyscallResult::Ok(
            AccountContractSafeDispatcher { contract_address: contract_address.value },
        )
    }
    #[inline(always)]
    fn write_at_offset(
        address_domain: u32,
        base: starknet::storage_access::StorageBaseAddress,
        offset: u8,
        value: AccountContractSafeDispatcher,
    ) -> starknet::SyscallResult<()> {
        let __store_derive_address_domain__ = address_domain;
        let __store_derive_base__ = base;
        let __store_derive_offset__ = offset;
        let AccountContractSafeDispatcher { contract_address } = value;
        let contract_address = core::internal::InferDestruct::<
            starknet::ContractAddress,
        > { value: contract_address };
        starknet::Store::<
            starknet::ContractAddress,
        >::write_at_offset(
            __store_derive_address_domain__,
            __store_derive_base__,
            __store_derive_offset__,
            contract_address.value,
        )?;
        starknet::SyscallResult::Ok(())
    }
    #[inline(always)]
    fn size() -> u8 {
        starknet::Store::<starknet::ContractAddress>::size()
    }
}

#[derive(Drop, Copy)]
#[doc(hidden)]
pub struct AccountContractSafeDispatcherSubPointers {
    pub contract_address: starknet::storage::StoragePointer<starknet::ContractAddress>,
}
#[doc(hidden)]
impl AccountContractSafeDispatcherSubPointersImpl of starknet::storage::SubPointers<
    AccountContractSafeDispatcher,
> {
    type SubPointersType = AccountContractSafeDispatcherSubPointers;
    fn sub_pointers(
        self: starknet::storage::StoragePointer<AccountContractSafeDispatcher>,
    ) -> AccountContractSafeDispatcherSubPointers {
        let base_address = self.__storage_pointer_address__;
        let mut current_offset = self.__storage_pointer_offset__;
        let __contract_address_value__ = starknet::storage::StoragePointer {
            __storage_pointer_address__: base_address, __storage_pointer_offset__: current_offset,
        };
        AccountContractSafeDispatcherSubPointers { contract_address: __contract_address_value__ }
    }
}
#[derive(Drop, Copy)]
#[doc(hidden)]
pub struct AccountContractSafeDispatcherSubPointersMut {
    pub contract_address: starknet::storage::StoragePointer<
        starknet::storage::Mutable<starknet::ContractAddress>,
    >,
}
#[doc(hidden)]
impl AccountContractSafeDispatcherSubPointersMutImpl of starknet::storage::SubPointersMut<
    AccountContractSafeDispatcher,
> {
    type SubPointersType = AccountContractSafeDispatcherSubPointersMut;
    fn sub_pointers_mut(
        self: starknet::storage::StoragePointer<
            starknet::storage::Mutable<AccountContractSafeDispatcher>,
        >,
    ) -> AccountContractSafeDispatcherSubPointersMut {
        let base_address = self.__storage_pointer_address__;
        let mut current_offset = self.__storage_pointer_offset__;
        let __contract_address_value__ = starknet::storage::StoragePointer {
            __storage_pointer_address__: base_address, __storage_pointer_offset__: current_offset,
        };
        AccountContractSafeDispatcherSubPointersMut { contract_address: __contract_address_value__ }
    }
}

pub trait AccountContractDispatcherTrait<T> {
    /// An entry point that is called to check if the account is willing to pay for the declaration
    /// of the class with the given hash.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the declaration.
    fn __validate_declare__(self: T, class_hash: felt252) -> felt252;

    /// An entry point that is called to check if the account is willing to pay for
    /// executing a given set of calls.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the execution, in which case `__execute__` will be called on the same set of calls.
    fn __validate__(self: T, calls: Array<Call>) -> felt252;

    /// An entry point that is called to execute a given set of calls.
    /// This entry point should block the deprecated v0 invoke transactions as they do not go
    /// through the `__validate__` entry point.
    fn __execute__(self: T, calls: Array<Call>) -> Array<Span<felt252>>;
}

#[derive(Copy, Drop, Serde)]
pub struct AccountContractDispatcher {
    pub contract_address: starknet::ContractAddress,
}

impl AccountContractDispatcherImpl of AccountContractDispatcherTrait<AccountContractDispatcher> {
    /// An entry point that is called to check if the account is willing to pay for the declaration
    /// of the class with the given hash.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the declaration.
    fn __validate_declare__(self: AccountContractDispatcher, class_hash: felt252) -> felt252 {
        let mut __calldata__ = core::traits::Default::default();
        core::serde::Serde::<felt252>::serialize(@class_hash, ref __calldata__);

        let mut __dispatcher_return_data__ = starknet::syscalls::call_contract_syscall(
            self.contract_address,
            0x289da278a8dc833409cabfdad1581e8e7d40e42dcaed693fa4008dcdb4963b3,
            core::array::ArrayTrait::span(@__calldata__),
        );
        let mut __dispatcher_return_data__ = starknet::SyscallResultTrait::unwrap_syscall(
            __dispatcher_return_data__,
        );
        core::option::OptionTrait::expect(
            core::serde::Serde::<felt252>::deserialize(ref __dispatcher_return_data__),
            'Returned data too short',
        )
    }

    /// An entry point that is called to check if the account is willing to pay for
    /// executing a given set of calls.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the execution, in which case `__execute__` will be called on the same set of calls.
    fn __validate__(self: AccountContractDispatcher, calls: Array<Call>) -> felt252 {
        let mut __calldata__ = core::traits::Default::default();
        core::serde::Serde::<Array<Call>>::serialize(@calls, ref __calldata__);

        let mut __dispatcher_return_data__ = starknet::syscalls::call_contract_syscall(
            self.contract_address,
            0x162da33a4585851fe8d3af3c2a9c60b557814e221e0d4f30ff0b2189d9c7775,
            core::array::ArrayTrait::span(@__calldata__),
        );
        let mut __dispatcher_return_data__ = starknet::SyscallResultTrait::unwrap_syscall(
            __dispatcher_return_data__,
        );
        core::option::OptionTrait::expect(
            core::serde::Serde::<felt252>::deserialize(ref __dispatcher_return_data__),
            'Returned data too short',
        )
    }

    /// An entry point that is called to execute a given set of calls.
    /// This entry point should block the deprecated v0 invoke transactions as they do not go
    /// through the `__validate__` entry point.
    fn __execute__(self: AccountContractDispatcher, calls: Array<Call>) -> Array<Span<felt252>> {
        let mut __calldata__ = core::traits::Default::default();
        core::serde::Serde::<Array<Call>>::serialize(@calls, ref __calldata__);

        let mut __dispatcher_return_data__ = starknet::syscalls::call_contract_syscall(
            self.contract_address,
            0x15d40a3d6ca2ac30f4031e42be28da9b056fef9bb7357ac5e85627ee876e5ad,
            core::array::ArrayTrait::span(@__calldata__),
        );
        let mut __dispatcher_return_data__ = starknet::SyscallResultTrait::unwrap_syscall(
            __dispatcher_return_data__,
        );
        core::option::OptionTrait::expect(
            core::serde::Serde::<Array<Span<felt252>>>::deserialize(ref __dispatcher_return_data__),
            'Returned data too short',
        )
    }
}

#[derive(Copy, Drop, Serde)]
pub struct AccountContractLibraryDispatcher {
    pub class_hash: starknet::ClassHash,
}

impl AccountContractLibraryDispatcherImpl of AccountContractDispatcherTrait<
    AccountContractLibraryDispatcher,
> {
    /// An entry point that is called to check if the account is willing to pay for the declaration
    /// of the class with the given hash.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the declaration.
    fn __validate_declare__(
        self: AccountContractLibraryDispatcher, class_hash: felt252,
    ) -> felt252 {
        let mut __calldata__ = core::traits::Default::default();
        core::serde::Serde::<felt252>::serialize(@class_hash, ref __calldata__);

        let mut __dispatcher_return_data__ = starknet::syscalls::library_call_syscall(
            self.class_hash,
            0x289da278a8dc833409cabfdad1581e8e7d40e42dcaed693fa4008dcdb4963b3,
            core::array::ArrayTrait::span(@__calldata__),
        );
        let mut __dispatcher_return_data__ = starknet::SyscallResultTrait::unwrap_syscall(
            __dispatcher_return_data__,
        );
        core::option::OptionTrait::expect(
            core::serde::Serde::<felt252>::deserialize(ref __dispatcher_return_data__),
            'Returned data too short',
        )
    }

    /// An entry point that is called to check if the account is willing to pay for
    /// executing a given set of calls.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the execution, in which case `__execute__` will be called on the same set of calls.
    fn __validate__(self: AccountContractLibraryDispatcher, calls: Array<Call>) -> felt252 {
        let mut __calldata__ = core::traits::Default::default();
        core::serde::Serde::<Array<Call>>::serialize(@calls, ref __calldata__);

        let mut __dispatcher_return_data__ = starknet::syscalls::library_call_syscall(
            self.class_hash,
            0x162da33a4585851fe8d3af3c2a9c60b557814e221e0d4f30ff0b2189d9c7775,
            core::array::ArrayTrait::span(@__calldata__),
        );
        let mut __dispatcher_return_data__ = starknet::SyscallResultTrait::unwrap_syscall(
            __dispatcher_return_data__,
        );
        core::option::OptionTrait::expect(
            core::serde::Serde::<felt252>::deserialize(ref __dispatcher_return_data__),
            'Returned data too short',
        )
    }

    /// An entry point that is called to execute a given set of calls.
    /// This entry point should block the deprecated v0 invoke transactions as they do not go
    /// through the `__validate__` entry point.
    fn __execute__(
        self: AccountContractLibraryDispatcher, calls: Array<Call>,
    ) -> Array<Span<felt252>> {
        let mut __calldata__ = core::traits::Default::default();
        core::serde::Serde::<Array<Call>>::serialize(@calls, ref __calldata__);

        let mut __dispatcher_return_data__ = starknet::syscalls::library_call_syscall(
            self.class_hash,
            0x15d40a3d6ca2ac30f4031e42be28da9b056fef9bb7357ac5e85627ee876e5ad,
            core::array::ArrayTrait::span(@__calldata__),
        );
        let mut __dispatcher_return_data__ = starknet::SyscallResultTrait::unwrap_syscall(
            __dispatcher_return_data__,
        );
        core::option::OptionTrait::expect(
            core::serde::Serde::<Array<Span<felt252>>>::deserialize(ref __dispatcher_return_data__),
            'Returned data too short',
        )
    }
}

pub trait AccountContractSafeDispatcherTrait<T> {
    #[unstable(feature: "safe_dispatcher")]
    /// An entry point that is called to check if the account is willing to pay for the declaration
    /// of the class with the given hash.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the declaration.
    fn __validate_declare__(self: T, class_hash: felt252) -> starknet::SyscallResult<felt252>;
    #[unstable(feature: "safe_dispatcher")]
    /// An entry point that is called to check if the account is willing to pay for
    /// executing a given set of calls.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the execution, in which case `__execute__` will be called on the same set of calls.
    fn __validate__(self: T, calls: Array<Call>) -> starknet::SyscallResult<felt252>;
    #[unstable(feature: "safe_dispatcher")]
    /// An entry point that is called to execute a given set of calls.
    /// This entry point should block the deprecated v0 invoke transactions as they do not go
    /// through the `__validate__` entry point.
    fn __execute__(self: T, calls: Array<Call>) -> starknet::SyscallResult<Array<Span<felt252>>>;
}

#[derive(Copy, Drop, Serde)]
pub struct AccountContractSafeLibraryDispatcher {
    pub class_hash: starknet::ClassHash,
}

impl AccountContractSafeLibraryDispatcherImpl of AccountContractSafeDispatcherTrait<
    AccountContractSafeLibraryDispatcher,
> {
    /// An entry point that is called to check if the account is willing to pay for the declaration
    /// of the class with the given hash.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the declaration.
    fn __validate_declare__(
        self: AccountContractSafeLibraryDispatcher, class_hash: felt252,
    ) -> starknet::SyscallResult<felt252> {
        let mut __calldata__ = core::traits::Default::default();
        core::serde::Serde::<felt252>::serialize(@class_hash, ref __calldata__);

        let mut __dispatcher_return_data__ = starknet::syscalls::library_call_syscall(
            self.class_hash,
            0x289da278a8dc833409cabfdad1581e8e7d40e42dcaed693fa4008dcdb4963b3,
            core::array::ArrayTrait::span(@__calldata__),
        );
        let mut __dispatcher_return_data__ = __dispatcher_return_data__?;
        Result::Ok(
            core::option::OptionTrait::expect(
                core::serde::Serde::<felt252>::deserialize(ref __dispatcher_return_data__),
                'Returned data too short',
            ),
        )
    }

    /// An entry point that is called to check if the account is willing to pay for
    /// executing a given set of calls.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the execution, in which case `__execute__` will be called on the same set of calls.
    fn __validate__(
        self: AccountContractSafeLibraryDispatcher, calls: Array<Call>,
    ) -> starknet::SyscallResult<felt252> {
        let mut __calldata__ = core::traits::Default::default();
        core::serde::Serde::<Array<Call>>::serialize(@calls, ref __calldata__);

        let mut __dispatcher_return_data__ = starknet::syscalls::library_call_syscall(
            self.class_hash,
            0x162da33a4585851fe8d3af3c2a9c60b557814e221e0d4f30ff0b2189d9c7775,
            core::array::ArrayTrait::span(@__calldata__),
        );
        let mut __dispatcher_return_data__ = __dispatcher_return_data__?;
        Result::Ok(
            core::option::OptionTrait::expect(
                core::serde::Serde::<felt252>::deserialize(ref __dispatcher_return_data__),
                'Returned data too short',
            ),
        )
    }

    /// An entry point that is called to execute a given set of calls.
    /// This entry point should block the deprecated v0 invoke transactions as they do not go
    /// through the `__validate__` entry point.
    fn __execute__(
        self: AccountContractSafeLibraryDispatcher, calls: Array<Call>,
    ) -> starknet::SyscallResult<Array<Span<felt252>>> {
        let mut __calldata__ = core::traits::Default::default();
        core::serde::Serde::<Array<Call>>::serialize(@calls, ref __calldata__);

        let mut __dispatcher_return_data__ = starknet::syscalls::library_call_syscall(
            self.class_hash,
            0x15d40a3d6ca2ac30f4031e42be28da9b056fef9bb7357ac5e85627ee876e5ad,
            core::array::ArrayTrait::span(@__calldata__),
        );
        let mut __dispatcher_return_data__ = __dispatcher_return_data__?;
        Result::Ok(
            core::option::OptionTrait::expect(
                core::serde::Serde::<
                    Array<Span<felt252>>,
                >::deserialize(ref __dispatcher_return_data__),
                'Returned data too short',
            ),
        )
    }
}


#[derive(Copy, Drop, Serde)]
pub struct AccountContractSafeDispatcher {
    pub contract_address: starknet::ContractAddress,
}

impl AccountContractSafeDispatcherImpl of AccountContractSafeDispatcherTrait<
    AccountContractSafeDispatcher,
> {
    /// An entry point that is called to check if the account is willing to pay for the declaration
    /// of the class with the given hash.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the declaration.
    fn __validate_declare__(
        self: AccountContractSafeDispatcher, class_hash: felt252,
    ) -> starknet::SyscallResult<felt252> {
        let mut __calldata__ = core::traits::Default::default();
        core::serde::Serde::<felt252>::serialize(@class_hash, ref __calldata__);

        let mut __dispatcher_return_data__ = starknet::syscalls::call_contract_syscall(
            self.contract_address,
            0x289da278a8dc833409cabfdad1581e8e7d40e42dcaed693fa4008dcdb4963b3,
            core::array::ArrayTrait::span(@__calldata__),
        );
        let mut __dispatcher_return_data__ = __dispatcher_return_data__?;
        Result::Ok(
            core::option::OptionTrait::expect(
                core::serde::Serde::<felt252>::deserialize(ref __dispatcher_return_data__),
                'Returned data too short',
            ),
        )
    }

    /// An entry point that is called to check if the account is willing to pay for
    /// executing a given set of calls.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the execution, in which case `__execute__` will be called on the same set of calls.
    fn __validate__(
        self: AccountContractSafeDispatcher, calls: Array<Call>,
    ) -> starknet::SyscallResult<felt252> {
        let mut __calldata__ = core::traits::Default::default();
        core::serde::Serde::<Array<Call>>::serialize(@calls, ref __calldata__);

        let mut __dispatcher_return_data__ = starknet::syscalls::call_contract_syscall(
            self.contract_address,
            0x162da33a4585851fe8d3af3c2a9c60b557814e221e0d4f30ff0b2189d9c7775,
            core::array::ArrayTrait::span(@__calldata__),
        );
        let mut __dispatcher_return_data__ = __dispatcher_return_data__?;
        Result::Ok(
            core::option::OptionTrait::expect(
                core::serde::Serde::<felt252>::deserialize(ref __dispatcher_return_data__),
                'Returned data too short',
            ),
        )
    }

    /// An entry point that is called to execute a given set of calls.
    /// This entry point should block the deprecated v0 invoke transactions as they do not go
    /// through the `__validate__` entry point.
    fn __execute__(
        self: AccountContractSafeDispatcher, calls: Array<Call>,
    ) -> starknet::SyscallResult<Array<Span<felt252>>> {
        let mut __calldata__ = core::traits::Default::default();
        core::serde::Serde::<Array<Call>>::serialize(@calls, ref __calldata__);

        let mut __dispatcher_return_data__ = starknet::syscalls::call_contract_syscall(
            self.contract_address,
            0x15d40a3d6ca2ac30f4031e42be28da9b056fef9bb7357ac5e85627ee876e5ad,
            core::array::ArrayTrait::span(@__calldata__),
        );
        let mut __dispatcher_return_data__ = __dispatcher_return_data__?;
        Result::Ok(
            core::option::OptionTrait::expect(
                core::serde::Serde::<
                    Array<Span<felt252>>,
                >::deserialize(ref __dispatcher_return_data__),
                'Returned data too short',
            ),
        )
    }
}
