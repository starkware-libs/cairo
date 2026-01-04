use core::to_byte_array::FormatAsByteArray;
use starknet::{ContractAddress, SyscallResult};

/// Converts panic data into a string (ByteArray).
///
/// `panic_data` is expected to be a valid serialized byte array with an extra
/// felt252 at the beginning, which is the BYTE_ARRAY_MAGIC.
pub fn panic_data_to_byte_array(panic_data: Array<felt252>) -> ByteArray {
    let mut panic_data = panic_data.span();

    // Remove BYTE_ARRAY_MAGIC from the panic data.
    panic_data.pop_front().expect('Empty panic data provided');

    match Serde::<ByteArray>::deserialize(ref panic_data) {
        Option::Some(string) => string,
        Option::None => { #[allow(panic)]
        panic!("Failed to deserialize panic data.") },
    }
}

/// Converts a `felt252` to a `base16` string padded to 66 characters including the `0x` prefix.
pub fn to_base_16_string(value: felt252) -> ByteArray {
    let mut string = value.format_as_byte_array(16);
    let mut padding = 64 - string.len();

    while padding != 0 {
        string = "0" + string;
        padding -= 1;
    }
    format!("0x{}", string)
}

/// Converts a `felt252` to a `base16` (hexadecimal) string without padding, but including the `0x`
/// prefix.
/// We need this because Starknet Foundry has a way of representing addresses and selectors that
/// does not include 0's after `0x`.
pub fn to_base_16_string_no_padding(value: felt252) -> ByteArray {
    let string = value.format_as_byte_array(16);
    format!("0x{}", string)
}

/// A helper trait that enables any value that can be converted to `felt252` to be represented
/// as a `base16` string (including the `0x` prefix).
#[generate_trait]
pub impl IntoBase16String<T, +Into<T, felt252>> of IntoBase16StringTrait<T> {
    fn into_base_16_string(self: T) -> ByteArray {
        to_base_16_string(self.into())
    }

    fn into_base_16_string_no_padding(self: T) -> ByteArray {
        to_base_16_string_no_padding(self.into())
    }
}

/// Asserts that the syscall result of a call failed with an "Entrypoint not found" error,
/// following the Starknet Foundry emitted error format.
pub fn assert_entrypoint_not_found_error<T, +Drop<T>>(
    result: SyscallResult<T>, selector: felt252, contract_address: ContractAddress,
) {
    if let Result::Err(panic_data) = result {
        let expected_panic_message = format!(
            "Entry point selector {} not found in contract {}",
            selector.into_base_16_string_no_padding(),
            contract_address.into_base_16_string_no_padding(),
        );
        let actual_panic_message = panic_data_to_byte_array(panic_data);
        assert!(
            actual_panic_message == expected_panic_message,
            "Got unexpected panic message: {actual_panic_message}",
        );
    } else {
        panic!("{selector} call was expected to fail, but succeeded");
    }
}
