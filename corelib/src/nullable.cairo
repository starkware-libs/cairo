use core::box::BoxTrait;
use core::traits::Default;
use core::traits::Felt252DictValue;

#[derive(Copy, Drop)]
extern type Nullable<T>;

enum FromNullableResult<T> {
    Null,
    NotNull: Box<T>,
}

extern fn null<T>() -> Nullable<T> nopanic;
extern fn nullable_from_box<T>(value: Box<T>) -> Nullable<T> nopanic;
extern fn match_nullable<T>(value: Nullable<T>) -> FromNullableResult<T> nopanic;
extern fn match_nullable_snapshot<T>(value: @Nullable<T>) -> FromNullableResult<@T> nopanic;

#[generate_trait]
impl NullableImpl<T> of NullableTrait<T> {
    fn deref(self: Nullable<T>) -> T {
        match match_nullable(self) {
            FromNullableResult::Null => core::panic_with_felt252('Attempted to deref null value'),
            FromNullableResult::NotNull(value) => value.unbox(),
        }
    }
    fn deref_or<+Drop<T>>(self: Nullable<T>, default: T) -> T {
        match match_nullable(self) {
            FromNullableResult::Null => default,
            FromNullableResult::NotNull(value) => value.unbox(),
        }
    }
    fn new(value: T) -> Nullable<T> {
        nullable_from_box(BoxTrait::new(value))
    }
    fn is_null(self: @Nullable<T>) -> bool {
        match match_nullable_snapshot(self) {
            FromNullableResult::Null => true,
            FromNullableResult::NotNull(_) => false,
        }
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
