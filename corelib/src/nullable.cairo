use box::BoxTrait;
use traits::Default;
use traits::Felt252DictValue;

#[derive(Copy, Drop)]
extern type Nullable<T>;

enum FromNullableResult<T> {
    Null,
    NotNull: Box<T>,
}

extern fn null<T>() -> Nullable<T> nopanic;
extern fn nullable_from_box<T>(value: Box<T>) -> Nullable<T> nopanic;
extern fn match_nullable<T>(value: Nullable<T>) -> FromNullableResult<T> nopanic;

trait NullableTrait<T> {
    fn deref(self: Nullable<T>) -> T;
    fn new(value: T) -> Nullable<T>;
}

impl NullableImpl<T> of NullableTrait<T> {
    fn deref(self: Nullable<T>) -> T {
        match match_nullable(self) {
            FromNullableResult::Null => panic_with_felt252('Attempted to deref null value'),
            FromNullableResult::NotNull(value) => value.unbox(),
        }
    }
    fn new(value: T) -> Nullable<T> {
        nullable_from_box(BoxTrait::new(value))
    }
}

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
