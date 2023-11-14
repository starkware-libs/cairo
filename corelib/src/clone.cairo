pub trait Clone<T> {
    fn clone(self: @T) -> T;
}

pub impl TCopyClone<T, +Copy<T>> of Clone<T> {
    fn clone(self: @T) -> T {
        *self
    }
}
