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
/// fn consume_with_relish<
///     F, O, +Drop<F>, +core::ops::FnOnce<F, ()>[Output: O], +core::fmt::Display<O>, +Drop<O>,
/// >(
///     func: F,
/// ) {
///     // `func` consumes its captured variables, so it cannot be run more
///     // than once.
///     println!("Consumed: {}", func());
///
///     println!("Delicious!");
///     // Attempting to invoke `func()` again will throw a `Variable was previously moved.` error
///     for
/// // `func`.
/// }
///
///   let x: ByteArray = "x";
///   let consume_and_return_x = || x;
///   consume_with_relish(consume_and_return_x);
///   // `consume_and_return_x` can no longer be invoked at this point
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
/// Instances of `Fn` can be called repeatedly.
///
/// `Fn` is implemented automatically by closures which only whose captured variable are all `Copy`.
/// Additionally, for any type `F` that implements `Fn`, `@F` implements `Fn`, too.
///
/// Since [`FnOnce`] is implemented for all implementers  of `Fn`, any instance of `Fn` can be used
/// as a parameter where a [`FnOnce`] is expected.
///
/// Use `Fn` as a bound when you want to accept a parameter of function-like type and need to call
/// it repeatedly. If you do not need such strict requirements, use [`FnOnce`] as bounds.
///
/// # Examples
///
/// ## Calling a closure
///
/// ```
/// let square = |x| x * x;
/// assert_eq!(square(5), 25);
/// ```
///
/// ## Using a `Fn` parameter
///
/// ```
/// fn call_with_one<F, +Drop<F>, +core::ops::Fn<F, (usize,)>[Output: usize]>(func: F) -> usize {
///    func(1)
/// }
///
/// let double = |x| x * 2;
/// assert_eq!(call_with_one(double), 2);
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
