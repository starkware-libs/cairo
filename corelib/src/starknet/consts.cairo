use starknet::{ClassHash, ContractAddress};

/// A number that can be represented by 251 bits.
///
/// The actual underlying type for `starknet::ContractAddress` and `starknet::ClassHash`.
pub type u251 =
    core::internal::bounded_int::BoundedInt<
        0, 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    >;

/// A helper trait for getting the values of `starknet` constants.
trait ConstHelper<T> {
    /// Returns the value of the constant.
    fn value<const VALUE: u251>() -> T nopanic;
}

/// Returns the value of a `starknet` constant.
///
/// Currently is only supported for `starknet::ClassHash` and `starknet::ContractAddress`.
pub fn const_value<const VALUE: u251, T, impl Helper: ConstHelper<T>>() -> T nopanic {
    Helper::value::<VALUE>()
}

extern fn class_hash_const<const address: u251>() -> ClassHash nopanic;
impl ClassHashConstHelper of ConstHelper<ClassHash> {
    fn value<const VALUE: u251>() -> ClassHash nopanic {
        class_hash_const::<VALUE>()
    }
}

extern fn contract_address_const<const address: u251>() -> ContractAddress nopanic;
impl ContractAddressConstHelper of ConstHelper<ContractAddress> {
    fn value<const VALUE: u251>() -> ContractAddress nopanic {
        contract_address_const::<VALUE>()
    }
}
