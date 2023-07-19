use traits::Into;
use starknet::ContractAddress;

extern type Pedersen;

extern fn pedersen(a: felt252, b: felt252) -> felt252 implicits(Pedersen) nopanic;

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

/// State for Pedersen hash.
#[derive(Copy, Drop)]
struct PedersenHashState {
    state: felt252, 
}

#[generate_trait]
impl PedersenHashStateTraitImpl of PedersenHashStateTrait {
    /// Creates a state from a base value.
    #[inline(always)]
    fn new(base: felt252) -> PedersenHashState {
        PedersenHashState { state: base }
    }
}

impl PedersenHashStateImpl of HashStateTrait<PedersenHashState> {
    #[inline(always)]
    fn update(self: PedersenHashState, value: felt252) -> PedersenHashState {
        PedersenHashState { state: pedersen(self.state, value) }
    }

    #[inline(always)]
    fn finalize(self: PedersenHashState) -> felt252 {
        self.state
    }
}

/// State for Poseidon hash.
#[derive(Copy, Drop)]
struct PoseidonHashState {
    s0: felt252,
    s1: felt252,
    s2: felt252,
    odd: bool,
}

#[generate_trait]
impl PoseidonHashStateTraitImpl of PoseidonHashStateTrait {
    /// Creates an initial state.
    fn new() -> PoseidonHashState {
        PoseidonHashState { s0: 0, s1: 0, s2: 0, odd: false }
    }
}

impl PoseidonHashStateImpl of HashStateTrait<PoseidonHashState> {
    #[inline(always)]
    fn update(self: PoseidonHashState, value: felt252) -> PoseidonHashState {
        if self.odd {
            let (s0, s1, s2) = poseidon::hades_permutation(self.s0, self.s1 + value, self.s2);
            PoseidonHashState { s0, s1, s2, odd: false }
        } else {
            PoseidonHashState { s0: self.s0 + value, s1: self.s1, s2: self.s2, odd: true }
        }
    }

    #[inline(always)]
    fn finalize(self: PoseidonHashState) -> felt252 {
        let (s0, s1) = if self.odd {
            (self.s0, self.s1 + 1)
        } else {
            (self.s0 + 1, self.s1)
        };
        let (r, _, _) = poseidon::hades_permutation(s0, s1, self.s2);
        r
    }
}

impl HashFelt252<S, impl SHashState: HashStateTrait<S>> of Hash<felt252, S, SHashState> {
    fn update_state(state: S, value: felt252) -> S {
        state.update(value)
    }
}

impl HashBool<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<bool, S, SHashState> {
    fn update_state(state: S, value: bool) -> S {
        state.update(value.into())
    }
}

impl HashU8<S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>> of Hash<u8, S, SHashState> {
    fn update_state(state: S, value: u8) -> S {
        state.update(value.into())
    }
}

impl HashU16<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<u16, S, SHashState> {
    fn update_state(state: S, value: u16) -> S {
        state.update(value.into())
    }
}

impl HashU32<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<u32, S, SHashState> {
    fn update_state(state: S, value: u32) -> S {
        state.update(value.into())
    }
}

impl HashU64<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<u64, S, SHashState> {
    fn update_state(state: S, value: u64) -> S {
        state.update(value.into())
    }
}

impl HashU128<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<u128, S, SHashState> {
    fn update_state(state: S, value: u128) -> S {
        state.update(value.into())
    }
}

impl HashU256<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<u256, S, SHashState> {
    fn update_state(state: S, value: u256) -> S {
        state.update_with(value.low).update_with(value.high)
    }
}

impl HashContractAddress<
    S, impl SHashState: HashStateTrait<S>, impl SDrop: Drop<S>
> of Hash<starknet::ContractAddress, S, SHashState> {
    fn update_state(state: S, value: ContractAddress) -> S {
        state.update(value.into())
    }
}

impl TupleSize0Hash<S, impl SHashState: HashStateTrait<S>> of Hash<(), S, SHashState> {
    fn update_state(state: S, value: ()) -> S {
        state
    }
}

impl TupleSize1Hash<
    E0, S, impl E0Hash: Hash<E0, S, SHashState>, impl SHashState: HashStateTrait<S>
> of Hash<(E0, ), S, SHashState> {
    fn update_state(state: S, value: (E0, )) -> S {
        let (e0, ) = value;
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
    fn update_state(state: S, value: (E0, E1, )) -> S {
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
    fn update_state(state: S, value: (E0, E1, E2, E3)) -> S {
        let (e0, e1, e2, e3) = value;
        state.update_with(e0).update_with(e1).update_with(e2).update_with(e3)
    }
}
