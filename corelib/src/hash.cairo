use traits::Into;
use starknet::ContractAddressIntoFelt;
use starknet::ContractAddress;

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

impl LegacyHashContractAddress of LegacyHash::<starknet::ContractAddress> {
    fn hash(state: felt, value: starknet::ContractAddress) -> felt {
        LegacyHash::<felt>::hash(state, value.into())
    }
}

impl TupleSize0LegacyHash of LegacyHash::<()> {
    fn hash(state: felt, value: ()) -> felt {
        state
    }
}

impl TupleSize1LegacyHash<E0, impl E0LegacyHash: LegacyHash::<E0>> of LegacyHash::<(E0, )> {
    fn hash(state: felt, value: (E0, )) -> felt {
        let (e0, ) = value;
        E0LegacyHash::hash(state, e0)
    }
}

impl TupleSize2LegacyHash<E0,
E1,
impl E0LegacyHash: LegacyHash::<E0>,
impl E1LegacyHash: LegacyHash::<E1>> of LegacyHash::<(E0, E1)> {
    fn hash(state: felt, value: (E0, E1, )) -> felt {
        let (e0, e1) = value;
        let state = E0LegacyHash::hash(state, e0);
        E1LegacyHash::hash(state, e1)
    }
}

impl TupleSize3LegacyHash<E0,
E1,
E2,
impl E0LegacyHash: LegacyHash::<E0>,
impl E1LegacyHash: LegacyHash::<E1>,
impl E2LegacyHash: LegacyHash::<E2>> of LegacyHash::<(E0, E1, E2)> {
    fn hash(state: felt, value: (E0, E1, E2)) -> felt {
        let (e0, e1, e2) = value;
        let state = E0LegacyHash::hash(state, e0);
        let state = E1LegacyHash::hash(state, e1);
        E2LegacyHash::hash(state, e2)
    }
}

impl TupleSize4LegacyHash<E0,
E1,
E2,
E3,
impl E0LegacyHash: LegacyHash::<E0>,
impl E1LegacyHash: LegacyHash::<E1>,
impl E2LegacyHash: LegacyHash::<E2>,
impl E3LegacyHash: LegacyHash::<E3>> of LegacyHash::<(E0, E1, E2, E3)> {
    fn hash(state: felt, value: (E0, E1, E2, E3)) -> felt {
        let (e0, e1, e2, e3) = value;
        let state = E0LegacyHash::hash(state, e0);
        let state = E1LegacyHash::hash(state, e1);
        let state = E2LegacyHash::hash(state, e2);
        E3LegacyHash::hash(state, e3)
    }
}
