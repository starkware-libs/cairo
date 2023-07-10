use array::ArrayTrait;
use serde::Serde;
use array::SpanTrait;

enum Option<T> {
    Some: T,
    None,
}

impl OptionSerde<T, impl TSerde: Serde<T>, impl TDrop: Drop<T>> of Serde<Option<T>> {
    fn serialize(self: @Option<T>, ref output: Array<felt252>) {
        match self {
            Option::Some(x) => {
                0.serialize(ref output);
                x.serialize(ref output)
            },
            Option::None => 1.serialize(ref output),
        }
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<Option<T>> {
        let variant = *serialized.pop_front()?;
        if variant == 0 {
            Option::Some(Option::Some(Serde::<T>::deserialize(ref serialized)?))
        } else if variant == 1 {
            Option::Some(Option::None)
        } else {
            Option::None
        }
    }
}

trait OptionTrait<T> {
    /// If `val` is `Option::Some(x)`, returns `x`. Otherwise, panics with `err`.
    fn expect(self: Option<T>, err: felt252) -> T;
    /// If `val` is `Option::Some(x)`, returns `x`. Otherwise, panics.
    fn unwrap(self: Option<T>) -> T;
    /// Returns `true` if the `Option` is `Option::Some`.
    fn is_some(self: @Option<T>) -> bool;
    /// Returns `true` if the `Option` is `Option::None`.
    fn is_none(self: @Option<T>) -> bool;
}
impl OptionTraitImpl<T> of OptionTrait<T> {
    #[inline(always)]
    fn expect(self: Option<T>, err: felt252) -> T {
        match self {
            Option::Some(x) => x,
            Option::None => panic_with_felt252(err),
        }
    }
    #[inline(always)]
    fn unwrap(self: Option<T>) -> T {
        self.expect('Option::unwrap failed.')
    }
    #[inline(always)]
    fn is_some(self: @Option<T>) -> bool {
        match self {
            Option::Some(_) => true,
            Option::None => false,
        }
    }
    #[inline(always)]
    fn is_none(self: @Option<T>) -> bool {
        match self {
            Option::Some(_) => false,
            Option::None => true,
        }
    }
}

// Impls for generic types.
impl OptionCopy<T, impl TCopy: Copy<T>> of Copy<Option<T>>;
impl OptionDrop<T, impl TDrop: Drop<T>> of Drop<Option<T>>;
