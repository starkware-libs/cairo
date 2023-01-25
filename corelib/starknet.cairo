extern type System;
#[derive(Copy, Drop)]
extern type StorageBaseAddress;
#[derive(Copy, Drop)]
extern type StorageAddress;
#[derive(Copy, Drop)]
extern type ContractAddress;

// An Helper function to force the inclusion of `System` in the list of implicits.
fn use_system_implicit() implicits(System) {}

// Storage.
extern fn storage_base_address_const<address>() -> StorageBaseAddress nopanic;
extern fn storage_base_address_from_felt(
    addr: felt
) -> StorageBaseAddress implicits(RangeCheck) nopanic;
extern fn storage_address_from_base_and_offset(
    base: StorageBaseAddress, offset: u8
) -> StorageAddress nopanic;
extern fn storage_address_from_base(base: StorageBaseAddress) -> StorageAddress nopanic;

// Only address_domain 0 is currently supported.
// This parameter is going to be used to access address spaces with different
// data availability guarantees.
extern fn storage_read_syscall(
    address_domain: felt, address: StorageAddress, 
) -> SyscallResult::<felt> implicits(GasBuiltin, System) nopanic;
extern fn storage_write_syscall(
    address_domain: felt, address: StorageAddress, value: felt
) -> SyscallResult::<()> implicits(GasBuiltin, System) nopanic;

// Interoperability.
extern fn contract_address_const<address>() -> ContractAddress nopanic;
extern fn call_contract_syscall(
    address: ContractAddress, calldata: Array::<felt>
) -> SyscallResult::<Array::<felt>> implicits(GasBuiltin, System) nopanic;

// Events.
extern fn emit_event_syscall(
    keys: Array::<felt>, data: Array::<felt>
) -> SyscallResult::<()> implicits(GasBuiltin, System) nopanic;

// Getters.
extern fn get_caller_address_syscall() -> SyscallResult::<felt> implicits(
    GasBuiltin, System
) nopanic;

fn get_caller_address() -> felt {
    get_caller_address_syscall().unwrap_syscall()
}

trait StorageAccess<T> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult::<T>;
    fn write(address_domain: felt, base: StorageBaseAddress, value: T) -> SyscallResult::<()>;
}

impl StorageAccessFelt of StorageAccess::<felt> {
    #[inline(always)]
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult::<felt> {
        storage_read_syscall(address_domain, storage_address_from_base(base))
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: felt) -> SyscallResult::<()> {
        storage_write_syscall(address_domain, storage_address_from_base(base), value)
    }
}

impl StorageAccessBool of StorageAccess::<bool> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult::<bool> {
        Result::Ok(StorageAccess::<felt>::read(address_domain, base)? != 0)
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: bool) -> SyscallResult::<()> {
        StorageAccess::<felt>::write(address_domain, base, if value {
            1
        } else {
            0
        })
    }
}

impl StorageAccessU8 of StorageAccess::<u8> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> Result::<u8, Array::<felt>> {
        Result::Ok(u8_from_felt(StorageAccess::<felt>::read(address_domain, base)?))
    }
    #[inline(always)]
    fn write(
        address_domain: felt, base: StorageBaseAddress, value: u8
    ) -> Result::<(), Array::<felt>> {
        StorageAccess::<felt>::write(address_domain, base, u8_to_felt(value))
    }
}

impl StorageAccessU128 of StorageAccess::<u128> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult::<u128> {
        Result::Ok(u128_from_felt(StorageAccess::<felt>::read(address_domain, base)?))
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: u128) -> SyscallResult::<()> {
        StorageAccess::<felt>::write(address_domain, base, u128_to_felt(value))
    }
}

impl StorageAccessU256 of StorageAccess::<u256> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult::<u256> {
        Result::Ok(
            u256 {
                low: StorageAccess::<u128>::read(address_domain, base)?,
                high: u128_from_felt(
                    storage_read_syscall(
                        address_domain, storage_address_from_base_and_offset(base, 1_u8)
                    )?
                )
            }
        )
    }
    fn write(address_domain: felt, base: StorageBaseAddress, value: u256) -> SyscallResult::<()> {
        StorageAccess::<u128>::write(address_domain, base, value.low)?;
        storage_write_syscall(
            address_domain,
            storage_address_from_base_and_offset(base, 1_u8),
            u128_to_felt(value.high)
        )
    }
}

/// The result type for a syscall.
type SyscallResult<T> = Result::<T, Array::<felt>>;

trait SyscallResultTrait<T> {
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, panics with the revert reason.
    fn unwrap_syscall(self: SyscallResult::<T>) -> T;
}
impl SyscallResultTraitImpl<T> of SyscallResultTrait::<T> {
    fn unwrap_syscall(self: SyscallResult::<T>) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(revert_reason) => {
                panic(revert_reason)
            },
        }
    }
}
