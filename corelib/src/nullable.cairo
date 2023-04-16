use traits::Default;
use traits::Felt252DictValue;

extern type Nullable<T>;

enum FromNullableResult<T> {
    Null: (),
    NotNull: Box<T>,
}

extern fn null<T>() -> Nullable<T> nopanic;
extern fn nullable_from_box<T>(value: Box<T>) -> Nullable<T> nopanic;
extern fn match_nullable<T>(value: Nullable<T>) -> FromNullableResult<T> nopanic;

// Impls for generic types
impl NullableCopy<T, impl TCopy: Copy<T>> of Copy<Nullable<T>>;
impl NullableDrop<T, impl TDrop: Drop<T>> of Drop<Nullable<T>>;

impl NullableDefault<T> of Default<Nullable<T>> {
    #[inline(always)]
    fn default() -> Nullable<T> nopanic {
        null()
    }
}

impl NullableFelt252DictValue<T> of Felt252DictValue<Nullable<T>> {
    #[inline(always)]
    fn zero_default() -> Nullable<T> nopanic {
        null()
    }
}
