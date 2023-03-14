use traits::Into;
use starknet::ContractAddressIntoFelt252;

extern type Pedersen;

extern fn pedersen(a: felt252, b: felt252) -> felt252 implicits(Pedersen) nopanic;

trait LegacyHash<T> {
    fn hash(state: felt252, value: T) -> felt252;
}

impl LegacyHashFelt252 of LegacyHash::<felt252> {
    fn hash(state: felt252, value: felt252) -> felt252 {
        pedersen(state, value)
    }
}

impl LegacyHashBool of LegacyHash::<bool> {
    fn hash(state: felt252, value: bool) -> felt252 {
        LegacyHash::<felt252>::hash(state, if value {
            1
        } else {
            0
        })
    }
}

impl LegacyHashU8 of LegacyHash::<u8> {
    fn hash(state: felt252, value: u8) -> felt252 {
        LegacyHash::<felt252>::hash(state, value.into())
    }
}

impl LegacyHashU16 of LegacyHash::<u16> {
    fn hash(state: felt252, value: u16) -> felt252 {
        LegacyHash::<felt252>::hash(state, value.into())
    }
}

impl LegacyHashU32 of LegacyHash::<u32> {
    fn hash(state: felt252, value: u32) -> felt252 {
        LegacyHash::<felt252>::hash(state, value.into())
    }
}

impl LegacyHashU64 of LegacyHash::<u64> {
    fn hash(state: felt252, value: u64) -> felt252 {
        LegacyHash::<felt252>::hash(state, value.into())
    }
}

impl LegacyHashU128 of LegacyHash::<u128> {
    fn hash(state: felt252, value: u128) -> felt252 {
        LegacyHash::<felt252>::hash(state, value.into())
    }
}

impl LegacyHashU256 of LegacyHash::<u256> {
    fn hash(state: felt252, value: u256) -> felt252 {
        let state = LegacyHash::<u128>::hash(state, value.low);
        LegacyHash::<u128>::hash(state, value.high)
    }
}

impl LegacyHashContractAddress of LegacyHash::<ContractAddress> {
    fn hash(state: felt252, value: ContractAddress) -> felt252 {
        LegacyHash::<felt252>::hash(state, value.into())
    }
}

// TODO(orizi): Move to generic impl.
impl LegacyHashFelt252Pair of LegacyHash::<(felt252, felt252)> {
    fn hash(state: felt252, pair: (felt252, felt252)) -> felt252 {
        let (first, second) = pair;
        let state = LegacyHash::hash(state, first);
        LegacyHash::hash(state, second)
    }
}

impl LegacyHashContractAddressPair of LegacyHash::<(ContractAddress, ContractAddress)> {
    fn hash(state: felt252, pair: (ContractAddress, ContractAddress)) -> felt252 {
        let (first, second) = pair;
        let state = LegacyHash::hash(state, first);
        LegacyHash::hash(state, second)
    }
}
