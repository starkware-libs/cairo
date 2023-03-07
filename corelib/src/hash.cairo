use traits::Into;
use starknet::ContractAddressIntoFelt;

extern type Pedersen;

extern fn pedersen(a: felt, b: felt) -> felt implicits(Pedersen) nopanic;

trait LegacyHash<T> {
    fn hash(state: felt, value: T) -> felt;
}

impl LegacyHashFelt of LegacyHash::<felt> {
    fn hash(state: felt, value: felt) -> felt {
        pedersen(state, value)
    }
}

impl LegacyHashBool of LegacyHash::<bool> {
    fn hash(state: felt, value: bool) -> felt {
        LegacyHash::<felt>::hash(state, if value {
            1
        } else {
            0
        })
    }
}

impl LegacyHashU8 of LegacyHash::<u8> {
    fn hash(state: felt, value: u8) -> felt {
        LegacyHash::<felt>::hash(state, value.into())
    }
}

impl LegacyHashU16 of LegacyHash::<u16> {
    fn hash(state: felt, value: u16) -> felt {
        LegacyHash::<felt>::hash(state, value.into())
    }
}

impl LegacyHashU32 of LegacyHash::<u32> {
    fn hash(state: felt, value: u32) -> felt {
        LegacyHash::<felt>::hash(state, value.into())
    }
}

impl LegacyHashU64 of LegacyHash::<u64> {
    fn hash(state: felt, value: u64) -> felt {
        LegacyHash::<felt>::hash(state, value.into())
    }
}

impl LegacyHashU128 of LegacyHash::<u128> {
    fn hash(state: felt, value: u128) -> felt {
        LegacyHash::<felt>::hash(state, value.into())
    }
}

impl LegacyHashU256 of LegacyHash::<u256> {
    fn hash(state: felt, value: u256) -> felt {
        let state = LegacyHash::<u128>::hash(state, value.low);
        LegacyHash::<u128>::hash(state, value.high)
    }
}

impl LegacyHashContractAddress of LegacyHash::<ContractAddress> {
    fn hash(state: felt, value: ContractAddress) -> felt {
        LegacyHash::<felt>::hash(state, value.into())
    }
}

// TODO(orizi): Move to generic impl.
impl LegacyHashFeltPair of LegacyHash::<(felt, felt)> {
    fn hash(state: felt, pair: (felt, felt)) -> felt {
        let (first, second) = pair;
        let state = LegacyHash::hash(state, first);
        LegacyHash::hash(state, second)
    }
}

impl LegacyHashContractAddressPair of LegacyHash::<(ContractAddress, ContractAddress)> {
    fn hash(state: felt, pair: (ContractAddress, ContractAddress)) -> felt {
        let (first, second) = pair;
        let state = LegacyHash::hash(state, first);
        LegacyHash::hash(state, second)
    }
}
