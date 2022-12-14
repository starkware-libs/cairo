extern type Nullable<T>;

enum FromNullableResult<T> {
    Null: (),
    NotNull: Box::<T>,
}

extern fn null<T>() -> Nullable::<T> nopanic;
extern fn into_nullable<T>(value: Box::<T>) -> Nullable::<T> nopanic;
extern fn from_nullable<T>(value: Nullable::<T>) -> FromNullableResult::<T> nopanic;
