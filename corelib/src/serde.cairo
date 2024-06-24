use core::array::ArrayTrait;
use core::array::SpanTrait;

pub trait Serde<T> {
    fn serialize(self: @T, ref output: Array<felt252>);
    fn deserialize(ref serialized: Span<felt252>) -> Option<T>;
}

/// Tuple style structs `Serde` implementation.
impl SerdeTuple<
    T,
    impl TSF: core::metaprogramming::TupleSnapForward<T>,
    impl Serialize: SerializeTuple<TSF::SnapForward>,
    impl Deserialize: DeserializeTuple<T>,
> of Serde<T> {
    fn serialize(self: @T, ref output: Array<felt252>) {
        Serialize::serialize(TSF::snap_forward(self), ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<T> {
        Deserialize::deserialize(ref serialized)
    }
}

/// Helper trait for serializing tuple style structs.
trait SerializeTuple<T> {
    fn serialize(value: T, ref output: Array<felt252>);
}

/// Implementation of `SerializeTuple` for snapshots of types with `Serde` implementation.
impl SerdeBasedSerializeTuple<T, +Serde<T>> of SerializeTuple<@T> {
    fn serialize(value: @T, ref output: Array<felt252>) {
        Serde::<T>::serialize(value, ref output);
    }
}

/// Helper trait for deserializing tuple style structs.
trait DeserializeTuple<T> {
    fn deserialize(ref serialized: Span<felt252>) -> Option<T>;
}

/// Base implementation of `SerializeTuple` for tuples.
impl SerializeTupleBaseTuple of SerializeTuple<()> {
    fn serialize(value: (), ref output: Array<felt252>) {}
}

/// Base implementation of `DeserializeTuple` for tuples.
impl DeserializeTupleBaseTuple of DeserializeTuple<()> {
    fn deserialize(ref serialized: Span<felt252>) -> Option<()> {
        Option::Some(())
    }
}

/// Base implementation of `SerializeTuple` for fixed sized arrays.
impl SerializeTupleBaseFixedSizedArray<T> of SerializeTuple<[@T; 0]> {
    fn serialize(value: [@T; 0], ref output: Array<felt252>) {}
}

/// Base implementation of `DeserializeTuple` for fixed sized arrays.
impl DeserializeTupleBaseFixedSizedArray<T> of DeserializeTuple<[T; 0]> {
    fn deserialize(ref serialized: Span<felt252>) -> Option<[T; 0]> {
        Option::Some([])
    }
}

/// Recursive implementation of `SerializeTuple` for tuple style structs.
impl SerializeTupleNext<
    T,
    impl TS: core::metaprogramming::TupleSplit<T>,
    +SerializeTuple<TS::Head>,
    +SerializeTuple<TS::Rest>,
    +Drop<TS::Rest>,
> of SerializeTuple<T> {
    fn serialize(value: T, ref output: Array<felt252>) {
        let (head, rest) = TS::split_head(value);
        SerializeTuple::<TS::Head>::serialize(head, ref output);
        SerializeTuple::<TS::Rest>::serialize(rest, ref output);
    }
}

/// Recursive implementation of `DeserializeTuple` for tuple style structs.
impl DeserializeTupleNext<
    T,
    impl TS: core::metaprogramming::TupleSplit<T>,
    +Serde<TS::Head>,
    +DeserializeTuple<TS::Rest>,
    +Drop<TS::Head>,
> of DeserializeTuple<T> {
    fn deserialize(ref serialized: Span<felt252>) -> Option<T> {
        let head = Serde::<TS::Head>::deserialize(ref serialized)?;
        let rest = DeserializeTuple::<TS::Rest>::deserialize(ref serialized)?;
        Option::Some(TS::reconstruct(head, rest))
    }
}

/// Impl for `Serde` for types that can be converted into `felt252` using the `Into` trait and from
/// `felt252` using the `TryInto` trait.
/// Usage example:
/// ```ignore
/// impl MyTypeSerde = core::serde::into_felt252_based::SerdeImpl<MyType>;`
/// ```
pub mod into_felt252_based {
    use core::traits::{Into, TryInto};
    use core::array::ArrayTrait;
    pub impl SerdeImpl<T, +Copy<T>, +Into<T, felt252>, +TryInto<felt252, T>> of super::Serde<T> {
        #[inline(always)]
        fn serialize(self: @T, ref output: Array<felt252>) {
            output.append((*self).into());
        }
        #[inline(always)]
        fn deserialize(ref serialized: Span<felt252>) -> Option<T> {
            Option::Some((*serialized.pop_front()?).try_into()?)
        }
    }
}
