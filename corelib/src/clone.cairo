trait Clone<T> {
    fn clone(self: @T) -> T;
}

impl TCopyClone<T, impl TCopy: Copy::<T>> of Clone::<T> {
    fn clone(self: @T) -> T {
        *self
    }
}
