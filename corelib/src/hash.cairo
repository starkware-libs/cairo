use traits::Into;

/// A trait for hash state accumulators.
trait HashStateTrait<S> {
    fn update(self: S, value: felt252) -> S;
    fn finalize(self: S) -> felt252;
}

/// A trait for values that can be hashed.
trait Hash<T, S, +HashStateTrait<S>> {
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
impl LegacyHashForHash<T, +Hash<T, pedersen::HashState>> of LegacyHash<T> {
    #[inline(always)]
    fn hash(state: felt252, value: T) -> felt252 {
        pedersen::HashState { state }.update_with(value).state
    }
}

/// Extension trait for hash state accumulators.
trait HashStateExTrait<S, T> {
    /// Updates the hash state with the given value.
    fn update_with(self: S, value: T) -> S;
}

impl HashStateEx<S, +HashStateTrait<S>, T, +Hash<T, S>> of HashStateExTrait<S, T> {
    #[inline(always)]
    fn update_with(self: S, value: T) -> S {
        Hash::update_state(self, value)
    }
}

impl HashFelt252<S, +HashStateTrait<S>> of Hash<felt252, S> {
    #[inline(always)]
    fn update_state(state: S, value: felt252) -> S {
        state.update(value)
    }
}

/// Impl for `Hash` for types that can be converted into `felt252` using the `Into` trait.
/// Usage example:
/// ```ignore
/// impl MyTypeHash<S, +HashStateTrait<S>, +Drop<S>> =
///     core::hash::into_felt252_based::HashImpl<MyType, S>;`
/// ```
mod into_felt252_based {
    impl HashImpl<
        T, S, +Into<T, felt252>, +super::HashStateTrait<S>, +Drop<S>
    > of super::Hash<T, S> {
        #[inline(always)]
        fn update_state(state: S, value: T) -> S {
            state.update(value.into())
        }
    }
}

impl HashBool<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<bool, S>;
impl HashU8<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<u8, S>;
impl HashU16<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<u16, S>;
impl HashU32<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<u32, S>;
impl HashU64<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<u64, S>;
impl HashU128<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<u128, S>;
impl HashI8<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<i8, S>;
impl HashI16<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<i16, S>;
impl HashI32<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<i32, S>;
impl HashI64<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<i64, S>;
impl HashI128<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<i128, S>;

impl TupleSize0Hash<S, +HashStateTrait<S>> of Hash<(), S> {
    #[inline(always)]
    fn update_state(state: S, value: ()) -> S {
        state
    }
}

impl TupleSize1Hash<E0, S, +Hash<E0, S>, +HashStateTrait<S>> of Hash<(E0,), S> {
    #[inline(always)]
    fn update_state(state: S, value: (E0,)) -> S {
        let (e0,) = value;
        state.update_with(e0)
    }
}

impl TupleSize2Hash<
    E0, E1, S, +HashStateTrait<S>, +Hash<E0, S>, +Hash<E1, S>, +Drop<E0>, +Drop<E1>,
> of Hash<(E0, E1), S> {
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
    +HashStateTrait<S>,
    +Hash<E0, S>,
    +Hash<E1, S>,
    +Hash<E2, S>,
    +Drop<E0>,
    +Drop<E1>,
    +Drop<E2>,
> of Hash<(E0, E1, E2), S> {
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
    +HashStateTrait<S>,
    +Hash<E0, S>,
    +Hash<E1, S>,
    +Hash<E2, S>,
    +Hash<E3, S>,
    +Drop<E0>,
    +Drop<E1>,
    +Drop<E2>,
    +Drop<E3>,
> of Hash<(E0, E1, E2, E3), S> {
    #[inline(always)]
    fn update_state(state: S, value: (E0, E1, E2, E3)) -> S {
        let (e0, e1, e2, e3) = value;
        state.update_with(e0).update_with(e1).update_with(e2).update_with(e3)
    }
}
