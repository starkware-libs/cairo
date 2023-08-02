use traits::Into;
use starknet::ContractAddress;

/// A trait for hash state accumulators.
trait HashStateTrait<S> {
    fn update(self: S, value: felt252) -> S;
    fn finalize(self: S) -> felt252;
}

/// A trait for values that can be hashed.
trait Hash<T, S, impl SHashState: HashStateTrait<S>> {
    /// Updates the hash state with the given value.
    fn update_state(state: S, value: T) -> S;
}

/// Trait for hashing values.
/// Used for backwards compatibility.
/// NOTE: Implement `Hash` instead of this trait if possible.
trait LegacyHash<T> {
    fn hash(state: felt252, value: T) -> felt252;
}

/// Implementation of `LegacyHash` for types that have `Hash` for backwards compatibility.
impl LegacyHashForHash<
    T, impl THash: Hash<T, pedersen::HashState, pedersen::HashStateImpl>
> of LegacyHash<T> {
    #[inline(always)]
    fn hash(state: felt252, value: T) -> felt252 {
        THash::update_state(pedersen::HashState { state }, value).state
    }
}

/// Extension trait for hash state accumulators.
trait HashStateExTrait<S, T> {
    /// Updates the hash state with the given value.
    fn update_with(self: S, value: T) -> S;
}

impl HashStateEx<
    S, impl SHashState: HashStateTrait<S>, T, impl THash: Hash<T, S, SHashState>
> of HashStateExTrait<S, T> {
    #[inline(always)]
    fn update_with(self: S, value: T) -> S {
        THash::update_state(self, value)
    }
}

impl HashFelt252<S, impl SHashState: HashStateTrait<S>> of Hash<felt252, S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: felt252) -> S {
        state.update(value)
    }
}

impl HashBool<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<bool, S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: bool) -> S {
        state.update(value.into())
    }
}

impl HashU8<S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>> of Hash<u8, S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: u8) -> S {
        state.update(value.into())
    }
}

impl HashU16<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<u16, S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: u16) -> S {
        state.update(value.into())
    }
}

impl HashU32<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<u32, S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: u32) -> S {
        state.update(value.into())
    }
}

impl HashU64<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<u64, S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: u64) -> S {
        state.update(value.into())
    }
}

impl HashU128<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<u128, S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: u128) -> S {
        state.update(value.into())
    }
}

impl HashU256<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<u256, S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: u256) -> S {
        state.update_with(value.low).update_with(value.high)
    }
}

impl HashContractAddress<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<starknet::ContractAddress, S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: ContractAddress) -> S {
        state.update(value.into())
    }
}

impl TupleSize0Hash<S, impl SHashState: HashStateTrait<S>> of Hash<(), S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: ()) -> S {
        state
    }
}

impl TupleSize1Hash<
    E0, S, impl E0Hash: Hash<E0, S, SHashState>, impl SHashState: HashStateTrait<S>
> of Hash<(E0,), S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: (E0,)) -> S {
        let (e0,) = value;
        state.update_with(e0)
    }
}

impl TupleSize2Hash<
    E0,
    E1,
    S,
    impl SHashState: HashStateTrait<S>,
    impl E0Hash: Hash<E0, S, SHashState>,
    impl E1Hash: Hash<E1, S, SHashState>,
    impl E0Drop: Drop<E0>,
    impl E1Drop: Drop<E1>,
> of Hash<(E0, E1), S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: (E0, E1,)) -> S {
        let (e0, e1) = value;
        state.update_with(e0).update_with(e1)
    }
}

impl TupleSize3Hash<
    E0,
    E1,
    E2,
    S,
    impl SHashState: HashStateTrait<S>,
    impl E0Hash: Hash<E0, S, SHashState>,
    impl E1Hash: Hash<E1, S, SHashState>,
    impl E2Hash: Hash<E2, S, SHashState>,
    impl E0Drop: Drop<E0>,
    impl E1Drop: Drop<E1>,
    impl E2Drop: Drop<E2>,
> of Hash<(E0, E1, E2), S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: (E0, E1, E2)) -> S {
        let (e0, e1, e2) = value;
        state.update_with(e0).update_with(e1).update_with(e2)
    }
}

impl TupleSize4Hash<
    E0,
    E1,
    E2,
    E3,
    S,
    impl SHashState: HashStateTrait<S>,
    impl E0Hash: Hash<E0, S, SHashState>,
    impl E1Hash: Hash<E1, S, SHashState>,
    impl E2Hash: Hash<E2, S, SHashState>,
    impl E3Hash: Hash<E3, S, SHashState>,
    impl E0Drop: Drop<E0>,
    impl E1Drop: Drop<E1>,
    impl E2Drop: Drop<E2>,
    impl E3Drop: Drop<E3>,
> of Hash<(E0, E1, E2, E3), S, SHashState> {
    #[inline(always)]
    fn update_state(state: S, value: (E0, E1, E2, E3)) -> S {
        let (e0, e1, e2, e3) = value;
        state.update_with(e0).update_with(e1).update_with(e2).update_with(e3)
    }
}
