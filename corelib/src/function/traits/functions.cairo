pub trait FnOnce<T,Args> {
    type Output;
    fn call(self: T, args: Args) -> Self::Output;
}
