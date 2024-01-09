use core::box::BoxTrait;
use core::traits::Default;
use core::traits::Felt252DictValue;

#[derive(Copy, Drop)]
pub extern type Nullable<T>;

pub enum FromNullableResult<T> {
    Null,
    NotNull: Box<T>,
}

pub extern fn null<T>() -> Nullable<T> nopanic;
pub(crate) extern fn nullable_from_box<T>(value: Box<T>) -> Nullable<T> nopanic;
pub extern fn match_nullable<T>(value: Nullable<T>) -> FromNullableResult<T> nopanic;
extern fn nullable_forward_snapshot<T>(value: @Nullable<T>) -> Nullable<@T> nopanic;

#[generate_trait]
pub impl NullableImpl<T> of NullableTrait<T> {
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
    #[must_use]
    fn new(value: T) -> Nullable<T> {
        nullable_from_box(BoxTrait::new(value))
    }
    #[must_use]
    fn is_null(self: @Nullable<T>) -> bool {
        match match_nullable(self.as_snapshot()) {
            FromNullableResult::Null => true,
            FromNullableResult::NotNull(_) => false,
        }
    }
    fn as_snapshot(self: @Nullable<T>) -> Nullable<@T> nopanic {
        nullable_forward_snapshot(self)
    }
}

impl NullableDefault<T> of Default<Nullable<T>> {
    #[inline(always)]
    #[must_use]
    fn default() -> Nullable<T> nopanic {
        null()
    }
}

impl NullableFelt252DictValue<T> of Felt252DictValue<Nullable<T>> {
    #[inline(always)]
    #[must_use]
    fn zero_default() -> Nullable<T> nopanic {
        null()
    }
}

impl NullableDebug<T, impl TDebug: core::fmt::Debug<T>> of core::fmt::Debug<Nullable<T>> {
    fn fmt(self: @Nullable<T>, ref f: core::fmt::Formatter) -> Result<(), core::fmt::Error> {
        match match_nullable(self.as_snapshot()) {
            FromNullableResult::Null => write!(f, "null"),
            FromNullableResult::NotNull(value) => {
                write!(f, "&")?;
                TDebug::fmt(value.unbox(), ref f)
            },
        }
    }
}
