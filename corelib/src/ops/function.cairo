/// The version of the call operator that takes a by-value receiver.
///
/// Instances of `FnOnce` can be called, but might not be callable multiple
/// times. Because of this, if the only thing known about a type is that it
/// implements `FnOnce`, it can only be called once.
///
/// `FnOnce` is implemented automatically by closures that might consume captured
/// variables.
/// ```
pub trait FnOnce<T, Args> {
    /// The returned type after the call operator is used.
    type Output;
    /// Performs the call operation.
    fn call(self: T, args: Args) -> Self::Output;
}
