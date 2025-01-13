//! Function call traits for flexible function invocation.
//!
//! This module defines traits for different styles of function calling in Cairo, providing
//! mechanisms for invoking functions with varying ownership and mutability semantics.
//!
//! The module provides two main traits:
//! - [`FnOnce`]: For functions that consume their captured variables
//! - [`Fn`]: For functions that can be called multiple times using snapshots
//!
//! # Key Concepts
//!
//! - `FnOnce` represents functions that can be called once, consuming the function value
//! - `Fn` represents functions that can be called multiple times through snapshots
//!
//! Both traits are automatically implemented for appropriate closure types. `Fn` implementations
//! can be used where `FnOnce` is expected.
//!
//! # Examples
//!
//! Basic usage with closures using a `map` function that maps over an array using a function:
//!
//! ```
//! // A function that maps over an array using a function
//! #[generate_trait]
//! impl ArrayExt of ArrayExtTrait {
//!     // Needed in Cairo 2.9.2 because of a bug in inlining analysis.
//!     #[inline(never)]
//!     fn map<T, +Drop<T>, F, +Drop<F>, impl func: core::ops::Fn<F, (T,)>, +Drop<func::Output>>(
//!         self: Array<T>, f: F,
//!     ) -> Array<func::Output> {
//!         let mut output: Array<func::Output> = array![];
//!         for elem in self {
//!             output.append(f(elem));
//!         };
//!         output
//!     }
//! }
//!
//! fn main () {
//!    let double = array![1, 2, 3].map(|item: u32| item * 2);
//!    println!("double: {:?}", double);
//! }
//! ```
//!
//! The example demonstrates why [`Fn`] is required for operations like `map`. Since `map` needs to
//! apply the function `f` to each element in the array, the function must be callable multiple
//! times. Using [`FnOnce`] would not work here because:
//! 1. `FnOnce` functions are consumed when called, as they take ownership of their captured
//! variables 2. After the first iteration, `f` would be consumed and unavailable

/// The version of the call operator that takes a by-value receiver.
///
/// Instances of `FnOnce` can be called, but might not be callable multiple
/// times. Because of this, if the only thing known about a type is that it
/// implements `FnOnce`, it can only be called once.
///
/// `FnOnce` is implemented automatically by closures that might consume captured
/// variables.
///
/// ```
/// # Examples
///
/// ```
/// #[inline(never)]
/// fn transform_once<T, +Drop<T>, F, +Drop<F>, impl func: core::ops::FnOnce<F, (T,)>,
/// +Drop<func::Output>>(
///     self: T, f: F,
/// ) -> func::Output {
///       f(self) // `f` can be called only once
/// }
///
/// fn main() {
///     let double = transform_once(10, |number: u8| number * 2);
///     println!("{}", double)
// }
/// ```
pub trait FnOnce<T, Args> {
    /// The returned type after the call operator is used.
    type Output;
    /// Performs the call operation.
    fn call(self: T, args: Args) -> Self::Output;
}

/// An implementation of `FnOnce` when `Fn` is implemented.
/// Makes sure we can always pass an `Fn` to a function that expects an `FnOnce`.
impl FnOnceImpl<T, Args, +Destruct<T>, +Fn<T, Args>> of FnOnce<T, Args> {
    type Output = Fn::<T, Args>::Output;
    fn call(self: T, args: Args) -> Self::Output {
        Fn::call(@self, args)
    }
}

/// The version of the call operator that takes a by-snapshot receiver.
///
/// Instances of `Fn` can be called multiple times.
///
/// `Fn` is implemented automatically by closures that capture only copyable variables.
///
/// # Examples
///
///   ```
/// fn transform_tuple<
///     T, +Drop<T>, F, +Drop<F>, impl func: core::ops::Fn<F, (T,)>, +Drop<func::Output>,
/// >(
///     tuple: (T, T), f: F,
/// ) -> (func::Output, func::Output) {
///     let (a, b) = tuple;
///     (f(a), f(b))
/// }
///
/// fn main() {
///     let double = transform_tuple((2, 4), |number: u8| number * 2);
///     println!("{:?}", double); // (4, 8)
/// }
/// ```
pub trait Fn<T, Args> {
    /// The returned type after the call operator is used.
    type Output;
    /// Performs the call operation.
    fn call(self: @T, args: Args) -> Self::Output;
}

/// Implementation of `Fn` for snapshots.
/// This allows using a snapshot of a type that implements `Fn` as a function.
impl FnSnapshotImpl<T, Args, impl F: Fn<T, Args>> of Fn<@T, Args> {
    type Output = F::Output;
    fn call(self: @@T, args: Args) -> Self::Output {
        F::call(*self, args)
    }
}
