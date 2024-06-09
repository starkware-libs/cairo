use core::traits::Into;

/// A trait for hash state accumulators.
pub trait HashStateTrait<S> {
    #[must_use]
    fn update(self: S, value: felt252) -> S;
    #[must_use]
    fn finalize(self: S) -> felt252;
}

/// A trait for values that can be hashed.
pub trait Hash<T, S, +HashStateTrait<S>> {
    /// Updates the hash state with the given value.
    #[must_use]
    fn update_state(state: S, value: T) -> S;
}

/// Trait for hashing values.
/// Used for backwards compatibility.
/// NOTE: Implement `Hash` instead of this trait if possible.
pub trait LegacyHash<T> {
    #[must_use]
    fn hash(state: felt252, value: T) -> felt252;
}

/// Implementation of `LegacyHash` for types that have `Hash` for backwards compatibility.
impl LegacyHashForHash<T, +Hash<T, core::pedersen::HashState>> of LegacyHash<T> {
    #[inline(always)]
    fn hash(state: felt252, value: T) -> felt252 {
        core::pedersen::HashState { state }.update_with(value).state
    }
}

/// Extension trait for hash state accumulators.
pub trait HashStateExTrait<S, T> {
    /// Updates the hash state with the given value.
    #[must_use]
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
pub mod into_felt252_based {
    pub impl HashImpl<
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

impl FixedSizedArray0Hash<T, S, +HashStateTrait<S>, +Drop<T>> of Hash<[T; 0], S> {
    #[inline(always)]
    fn update_state(state: S, value: [T; 0]) -> S {
        state
    }
}

impl TupleNextHash<
    T,
    S,
    +HashStateTrait<S>,
    impl TH: core::metaprogramming::TupleSplit<T>,
    +Hash<TH::Head, S>,
    +Hash<TH::Rest, S>,
    +Drop<TH::Rest>,
> of Hash<T, S> {
    #[inline(always)]
    fn update_state(state: S, value: T) -> S {
        let (head, rest) = TH::split_head(value);
        state.update_with(head).update_with(rest)
    }
}
