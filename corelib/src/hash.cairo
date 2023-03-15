use traits::Into;
use starknet::ContractAddressIntoFelt252;
use starknet::ContractAddress;

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

impl LegacyHashContractAddress of LegacyHash::<starknet::ContractAddress> {
    fn hash(state: felt252, value: starknet::ContractAddress) -> felt252 {
        LegacyHash::<felt252>::hash(state, value.into())
    }
}

impl TupleSize0LegacyHash of LegacyHash::<()> {
    fn hash(state: felt252, value: ()) -> felt252 {
        state
    }
}

impl TupleSize1LegacyHash<E0, impl E0LegacyHash: LegacyHash::<E0>> of LegacyHash::<(E0, )> {
    fn hash(state: felt252, value: (E0, )) -> felt252 {
        let (e0, ) = value;
        E0LegacyHash::hash(state, e0)
    }
}

impl TupleSize2LegacyHash<E0,
E1,
impl E0LegacyHash: LegacyHash::<E0>,
impl E1LegacyHash: LegacyHash::<E1>> of LegacyHash::<(E0, E1)> {
    fn hash(state: felt252, value: (E0, E1, )) -> felt252 {
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
    fn hash(state: felt252, value: (E0, E1, E2)) -> felt252 {
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
    fn hash(state: felt252, value: (E0, E1, E2, E3)) -> felt252 {
        let (e0, e1, e2, e3) = value;
        let state = E0LegacyHash::hash(state, e0);
        let state = E1LegacyHash::hash(state, e1);
        let state = E2LegacyHash::hash(state, e2);
        E3LegacyHash::hash(state, e3)
    }
}
