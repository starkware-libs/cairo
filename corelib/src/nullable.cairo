extern type Nullable<T>;

enum FromNullableResult<T> {
    Null: (),
    NotNull: Box<T>,
}

extern fn null<T>() -> Nullable<T> nopanic;
extern fn nullable_from_box<T>(value: Box<T>) -> Nullable<T> nopanic;
extern fn match_nullable<T>(value: Nullable<T>) -> FromNullableResult<T> nopanic;

// Impls for generic types
impl NullableCopy<T, impl TCopy: Copy::<T>> of Copy::<Nullable<T>>;
impl NullableDrop<T, impl TDrop: Drop::<T>> of Drop::<Nullable<T>>;
