trait Clone<T> {
    fn clone(self: @T) -> T;
}

impl TCopyClone<T, +Copy<T>> of Clone<T> {
    fn clone(self: @T) -> T {
        *self
    }
}
