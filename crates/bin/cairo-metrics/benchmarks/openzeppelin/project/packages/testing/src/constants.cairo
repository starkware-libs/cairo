use starknet::{ClassHash, ContractAddress};

pub type EthPublicKey = starknet::secp256k1::Secp256k1Point;

//
// Numeric values
//

pub const QUERY_OFFSET: felt252 = 0x100000000000000000000000000000000; // 2**128
pub const QUERY_VERSION: felt252 =
    0x100000000000000000000000000000001; // QUERY_OFFSET + MIN_TRANSACTION_VERSION
pub const DECIMALS: u8 = 18;
pub const SUPPLY: u256 = 2_000;
pub const VALUE: u256 = 300;
pub const FELT_VALUE: felt252 = 'FELT_VALUE';
pub const ROLE: felt252 = 'ROLE';
pub const TIMESTAMP: u64 = 1704067200; // 2024-01-01 00:00:00 UTC
pub const BLOCK_NUMBER: u64 = 1234567;
pub const OTHER_ROLE: felt252 = 'OTHER_ROLE';
pub const CHAIN_ID: felt252 = 'CHAIN_ID';
pub const TOKEN_ID: u256 = 21;
pub const TOKEN_ID_2: u256 = 121;
pub const TOKEN_VALUE: u256 = 42;
pub const TOKEN_VALUE_2: u256 = 142;
pub const PUBKEY: felt252 = 'PUBKEY';
pub const NEW_PUBKEY: felt252 = 0x26da8d11938b76025862be14fdb8b28438827f73e75e86f7bfa38b196951fa7;
pub const DAPP_NAME: felt252 = 'DAPP_NAME';
pub const DAPP_VERSION: felt252 = 'DAPP_VERSION';
pub const SALT: felt252 = 'SALT';
pub const SUCCESS: felt252 = 'SUCCESS';
pub const FAILURE: felt252 = 'FAILURE';
pub const MIN_TRANSACTION_VERSION: felt252 = 1;
pub const TRANSACTION_HASH: felt252 = 'TRANSACTION_HASH';

pub fn NAME() -> ByteArray {
    "NAME"
}

pub fn SYMBOL() -> ByteArray {
    "SYMBOL"
}

pub fn BASE_URI() -> ByteArray {
    "https://api.example.com/v1/"
}

pub fn BASE_URI_2() -> ByteArray {
    "https://api.example.com/v2/"
}

//
// Contract addresses
//

pub const ADMIN: ContractAddress = 'ADMIN'.as_address();
pub const AUTHORIZED: ContractAddress = 'AUTHORIZED'.as_address();
pub const ZERO: ContractAddress = 0.as_address();
pub const CALLER: ContractAddress = 'CALLER'.as_address();
pub const OWNER: ContractAddress = 'OWNER'.as_address();
pub const NEW_OWNER: ContractAddress = 'NEW_OWNER'.as_address();
pub const OTHER: ContractAddress = 'OTHER'.as_address();
pub const OTHER_ADMIN: ContractAddress = 'OTHER_ADMIN'.as_address();
pub const SPENDER: ContractAddress = 'SPENDER'.as_address();
pub const RECIPIENT: ContractAddress = 'RECIPIENT'.as_address();
pub const OPERATOR: ContractAddress = 'OPERATOR'.as_address();
pub const DELEGATOR: ContractAddress = 'DELEGATOR'.as_address();
pub const DELEGATEE: ContractAddress = 'DELEGATEE'.as_address();
pub const TIMELOCK: ContractAddress = 'TIMELOCK'.as_address();
pub const VOTES_TOKEN: ContractAddress = 'VOTES_TOKEN'.as_address();
pub const ALICE: ContractAddress = 'ALICE'.as_address();
pub const BOB: ContractAddress = 'BOB'.as_address();
pub const CHARLIE: ContractAddress = 'CHARLIE'.as_address();

pub const CLASS_HASH_ZERO: ClassHash = 0.try_into().unwrap();

//
// Data arrays
//

pub fn DATA(success: bool) -> Span<felt252> {
    let value = if success {
        SUCCESS
    } else {
        FAILURE
    };
    array![value].span()
}

pub fn EMPTY_DATA() -> Span<felt252> {
    array![].span()
}

//
// Signing keys
//

pub mod stark {
    use crate::signing::{StarkKeyPair, get_stark_keys_from};

    pub fn KEY_PAIR() -> StarkKeyPair {
        get_stark_keys_from('PRIVATE_KEY')
    }

    pub fn KEY_PAIR_2() -> StarkKeyPair {
        get_stark_keys_from('PRIVATE_KEY_2')
    }
}

pub mod secp256k1 {
    use crate::signing::{Secp256k1KeyPair, get_secp256k1_keys_from};

    pub fn KEY_PAIR() -> Secp256k1KeyPair {
        let private_key = u256 { low: 'PRIVATE_LOW', high: 'PRIVATE_HIGH' };
        get_secp256k1_keys_from(private_key)
    }

    pub fn KEY_PAIR_2() -> Secp256k1KeyPair {
        let private_key = u256 { low: 'PRIVATE_LOW_2', high: 'PRIVATE_HIGH_2' };
        get_secp256k1_keys_from(private_key)
    }
}

pub mod secp256r1 {
    use crate::signing::{Secp256r1KeyPair, get_secp256r1_keys_from};

    pub fn KEY_PAIR() -> Secp256r1KeyPair {
        let private_key = u256 { low: 'PRIVATE_LOW', high: 'PRIVATE_HIGH' };
        get_secp256r1_keys_from(private_key)
    }

    pub fn KEY_PAIR_2() -> Secp256r1KeyPair {
        let private_key = u256 { low: 'PRIVATE_LOW_2', high: 'PRIVATE_HIGH_2' };
        get_secp256r1_keys_from(private_key)
    }
}

//
// Helpers
//

#[generate_trait]
pub impl AsAddressImpl of AsAddressTrait {
    /// Converts a felt252 to a ContractAddress as a constant function.
    ///
    /// Requirements:
    ///
    /// - `value` must be a valid contract address.
    const fn as_address(self: felt252) -> ContractAddress {
        self.try_into().expect('Invalid contract address')
    }
}
