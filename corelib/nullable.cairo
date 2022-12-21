extern type Nullable<T>;

enum FromNullableResult<T> { Null: (), NotNull: Box::<T>, }

extern func null<T>() -> Nullable::<T> nopanic;
extern func into_nullable<T>(value: Box::<T>) -> Nullable::<T> nopanic;
extern func from_nullable<T>(value: Nullable::<T>) -> FromNullableResult::<T> nopanic;
