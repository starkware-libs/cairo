//! Core traits for various operations.
//!
//! This module provides a collection of essential traits that define common behavior patterns
//! for Cairo types.
//!
//! # Main Categories
//!
//! ## Memory Management
//! - [`Copy`]: Enables value semantics for types
//! - [`Drop`]: Allows values to be safely discarded
//! - [`Destruct`]: Provides custom cleanup behavior for non-droppable types
//! - [`PanicDestruct`]: Handles destruction during panic scenarios
//!
//! ## Arithmetic Operations
//! - [`Add`], [`Sub`], [`Mul`], [`Div`], [`Rem`]: Standard arithmetic operators (`+`, `-`, `*`,
//! `/`, `%`)
//! - [`DivRem`]: Combined division and remainder operation
//! - [`Neg`]: Unary negation (`-`)
//!
//! ## Bitwise Operations
//! - [`BitAnd`], [`BitOr`], [`BitXor`]: Binary bitwise operations (`&`, `|`, `^`)
//! - [`BitNot`]: Unary bitwise complement (`~`)
//!
//! ## Comparison
//! - [`PartialEq`]: Equality comparison (`==`, `!=`)
//! - [`PartialOrd`]: Ordering comparison (`<`, `<=`, `>`, `>=`)
//!
//! ## Type Conversion
//! - [`Into`]: Infallible type conversion
//! - [`TryInto`]: Fallible type conversion
//!
//! ## Utility Traits
//! - [`Default`]: Creation of default values
//! - [`Felt252DictValue`]: Support for dictionary value types

use crate::panics::Panic;

/// A trait for copying values.
///
/// By default, variables in Cairo have 'move semantics', meaning they are moved when used.
/// However, types implementing `Copy` have 'copy semantics', allowing the value to be
/// duplicated instead of moved.
///
/// # Deriving
///
/// This trait can be automatically derived using `#[derive(Copy)]`. Most basic types
/// implement `Copy` by default.
///
/// # Examples
///
/// Without `Copy` (move semantics):
///
/// ```
/// #[derive(Drop)]
/// struct Point {
///     x: u128,
///     y: u128,
/// }
///
/// fn main() {
///     let p1 = Point { x: 5, y: 10 };
///     foo(p1);
///     foo(p1); // error: Variable was previously moved.
/// }
///
/// fn foo(p: Point) {}
/// ```
///
/// With `Copy` (copy semantics):
///
/// ```
/// #[derive(Copy, Drop)]
/// struct Point {
///     x: u128,
///     y: u128,
/// }
///
/// fn main() {
///     let p1 = Point { x: 5, y: 10 };
///     foo(p1);
///     foo(p1); // works: `p1` is copied when passed to `foo`
/// }
///
/// fn foo(p: Point) {}
/// ```
pub trait Copy<T>;

/// A trait for types that can be safely dropped.
///
/// Types implementing `Drop` can be automatically discarded when they go out of scope.
/// The drop operation is a no-op - it simply indicates to the compiler that this type
/// can be safely discarded.
///
/// # Deriving
///
/// This trait can be automatically derived using `#[derive(Drop)]`. All basic types
/// implement `Drop` by default, except for `Felt252Dict`.
///
/// # Examples
///
/// Without `Drop`:
///
/// ```
/// struct Point {
///     x: u128,
///     y: u128,
/// }
///
/// fn foo(p: Point) {} // Error: `p` cannot be dropped
/// ```
///
/// With `Drop`:
///
/// ```
/// #[derive(Drop)]
/// struct Point {
///     x: u128,
///     y: u128,
/// }
///
/// fn foo(p: Point) {} // OK: `p` is dropped at the end of the function
/// ```
pub trait Drop<T>;

impl SnapshotCopy<T> of Copy<@T>;

impl SnapshotDrop<T> of Drop<@T>;

// TODO(spapini): When associated types are supported, support the general trait Add<X, Y>.
/// The addition operator `+`.
///
/// Types implementing this trait support the addition operation via the `+` operator.
///
/// # Examples
///
/// Basic usage with numbers:
///
/// ```
/// assert!(1_u8 + 2_u8 == 3_u8);
/// ```
///
/// Custom implementation for a type:
///
/// ```
/// #[derive(Copy, Drop, PartialEq)]
/// struct Point {
///     x: u32,
///     y: u32,
/// }
///
/// impl PointAdd of Add<Point> {
///     fn add(lhs: Point, rhs: Point) -> Point {
///         Point {
///             x: lhs.x + rhs.x,
///             y: lhs.y + rhs.y,
///         }
///     }
/// }
///
/// let p1 = Point { x: 1, y: 0 };
/// let p2 = Point { x: 2, y: 3 };
/// let p3 = p1 + p2;
/// assert!(p3 == Point { x: 3, y: 3 });
/// ```
pub trait Add<T> {
    /// Performs the `+` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(12 + 1 == 13);
    /// ```
    fn add(lhs: T, rhs: T) -> T;
}

#[deprecated(
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::AddAssign`.", since: "2.7.0",
)]
pub trait AddEq<T> {
    fn add_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Sub<X, Y>.
/// The subtraction operator `-`.
///
/// Types implementing this trait support the subtraction operation via the `-` operator.
///
/// # Examples
///
/// Basic usage with numbers:
///
/// ```
/// assert!(3_u8 - 2_u8 == 1_u8);
/// ```
///
/// Custom implementation for a type:
///
/// ```
/// #[derive(Copy, Drop, PartialEq)]
/// struct Point {
///     x: u32,
///     y: u32,
/// }
///
/// impl PointSub of Sub<Point> {
///     fn sub(lhs: Point, rhs: Point) -> Point {
///         Point {
///             x: lhs.x - rhs.x,
///             y: lhs.y - rhs.y,
///         }
///     }
/// }
///
/// let p1 = Point { x: 2, y: 3 };
/// let p2 = Point { x: 1, y: 0 };
/// let p3 = p1 - p2;
/// assert!(p3 == Point { x: 1, y: 3 });
/// ```
pub trait Sub<T> {
    /// Performs the `-` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(12 - 1 == 11);
    /// ```
    fn sub(lhs: T, rhs: T) -> T;
}

#[deprecated(
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::SubAssign`.", since: "2.7.0",
)]
pub trait SubEq<T> {
    fn sub_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Mul<X, Y>.
/// The multiplication operator `*`.
///
/// Types implementing this trait support the multiplication operation via the `*` operator.
///
/// # Examples
///
/// Basic usage with numbers:
///
/// ```
/// assert!(3_u8 * 2_u8 == 6_u8);
/// ```
///
/// Custom implementation for a type:
///
/// ```
/// #[derive(Copy, Drop, PartialEq)]
/// struct Point {
///     x: u32,
///     y: u32,
/// }
///
/// impl PointMul of Mul<Point> {
///     fn mul(lhs: Point, rhs: Point) -> Point {
///         Point {
///             x: lhs.x * rhs.x,
///             y: lhs.y * rhs.y,
///         }
///     }
/// }
///
/// let p1 = Point { x: 2, y: 3 };
/// let p2 = Point { x: 1, y: 0 };
/// let p3 = p1 * p2;
/// assert!(p3 == Point { x: 2, y: 0 });
/// ```
pub trait Mul<T> {
    /// Performs the `*` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(12 * 2 == 24);
    /// ```
    fn mul(lhs: T, rhs: T) -> T;
}

#[deprecated(
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::MulAssign`.", since: "2.7.0",
)]
pub trait MulEq<T> {
    fn mul_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Div<X, Y>.
/// The division operator `/`.
///
/// Types implementing this trait support the division operation via the `/` operator.
///
/// # Examples
///
/// Basic usage with numbers:
///
/// ```
/// assert!(4_u8 / 2_u8 == 2_u8);
/// ```
///
/// Custom implementation for a type:
///
/// ```
/// #[derive(Copy, Drop, PartialEq)]
/// struct Point {
///     x: u32,
///     y: u32,
/// }
///
/// impl PointDiv of Div<Point> {
///     fn div(lhs: Point, rhs: Point) -> Point {
///         Point {
///             x: lhs.x / rhs.x,
///             y: lhs.y / rhs.y,
///         }
///     }
/// }
///
/// let p1 = Point { x: 2, y: 4 };
/// let p2 = Point { x: 2, y: 2 };
/// let p3 = p1 / p2;
/// assert!(p3 == Point { x: 1, y: 2 });
/// ```
pub trait Div<T> {
    /// Performs the `/` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(12 / 2 == 6);
    /// ```
    fn div(lhs: T, rhs: T) -> T;
}

#[deprecated(
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::DivAssign`.", since: "2.7.0",
)]
pub trait DivEq<T> {
    fn div_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Rem<X, Y>.
/// The remainder operator `%`.
///
/// Types implementing this trait support the remainder operation via the `%` operator.
///
/// # Examples
///
/// Basic usage with numbers:
///
/// ```
/// assert!(3_u8 % 2_u8 == 1_u8);
/// ```
pub trait Rem<T> {
    /// Performs the `%` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(12_u8 % 10_u8 == 2_u8);
    /// ```
    fn rem(lhs: T, rhs: T) -> T;
}

#[deprecated(
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::RemAssign`.", since: "2.7.0",
)]
pub trait RemEq<T> {
    fn rem_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait DivRem<X, Y>.
/// This trait provides a way to compute both division and remainder in a single operation,
/// which can be more efficient than computing them separately.
pub trait DivRem<T> {
    /// Performs the `/` and the `%` operations, returning both the quotient and remainder.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::traits::DivRem;
    ///
    /// assert!(DivRem::div_rem(12_u32, 10) == (1, 2));
    /// ```
    fn div_rem(lhs: T, rhs: NonZero<T>) -> (T, T);
}

/// Trait for comparisons using the equality operator.
///
/// Implementing this trait for types provides the `==` and `!=` operators for
/// those types.
///
/// # Derivable
///
/// This trait can be used with `#[derive]`. When `derive`d on structs, two
/// instances are equal if all fields are equal, and not equal if any fields
/// are not equal. When `derive`d on enums, two instances are equal if they
/// are the same variant and all fields are equal.
///
/// # Examples
///
/// Basic usage with built-in types:
///
/// ```
/// assert!(1 == 1);
/// assert!(1 != 2);
/// ```
///
/// Custom implementation:
///
/// ```
/// #[derive(Copy, Drop)]
/// struct Point {
///     x: u32,
///     y: u32
/// }
///
/// impl PointEq of PartialEq<Point> {
///     fn eq(lhs: @Point, rhs: @Point) -> bool {
///         lhs.x == rhs.x && lhs.y == rhs.y
///     }
/// }
///
/// let p1 = Point { x: 1, y: 2 };
/// let p2 = Point { x: 1, y: 2 };
/// assert!(p1 == p2);
/// assert!(!(p1 != p2));
/// ```
pub trait PartialEq<T> {
    /// Returns whether `lhs` and `rhs` equal, and is used by `==`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(1 == 1);
    /// ```
    fn eq(lhs: @T, rhs: @T) -> bool;

    /// Returns whether `lhs` and `rhs` are not equal, and is used by `!=`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(0 != 1);
    /// ```
    fn ne(lhs: @T, rhs: @T) -> bool {
        !Self::eq(lhs, rhs)
    }
}

impl PartialEqSnap<T, +PartialEq<T>> of PartialEq<@T> {
    fn eq(lhs: @@T, rhs: @@T) -> bool {
        PartialEq::<T>::eq(*lhs, *rhs)
    }
}

// TODO(spapini): When associated types are supported, support the general trait BitAnd<X, Y>.
/// A trait for computing the bitwise AND operator `&`.
///
/// # Examples
///
/// An implementation of `BitAnd` for a wrapper around `bool`.
///
/// ```
/// use core::traits::BitAnd;
///
/// #[derive(Drop, PartialEq)]
/// struct Wrapper {
///     bool: bool,
/// }
///
/// impl BitAndWrapper of BitAnd<Wrapper> {
///     #[inline]
///     fn bitand(lhs: Wrapper, rhs: Wrapper) -> Wrapper {
///         Wrapper { bool: lhs.bool & rhs.bool }
///     }
/// }
///
/// fn main() {
///     assert!(Wrapper { bool: true } & Wrapper { bool: true } == Wrapper { bool: true });
///     assert!(Wrapper { bool: true } & Wrapper { bool: false } == Wrapper { bool: false });
///     assert!(Wrapper { bool: false } & Wrapper { bool: true } == Wrapper { bool: false });
///     assert!(Wrapper { bool: false } & Wrapper { bool: false } == Wrapper { bool: false });
/// }
/// ```
pub trait BitAnd<T> {
    /// Computes the result of the AND operation between two values of the same type.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(1_u8 & 2_u8 == 0);
    /// ```
    fn bitand(lhs: T, rhs: T) -> T;
}

// TODO(spapini): When associated types are supported, support the general trait BitOr<X, Y>.
/// A trait for computing the bitwise OR operator `|`.
///
/// # Examples
///
/// An implementation of `BitOr` for a wrapper around `bool`.
///
/// ```
/// use core::traits::BitOr;
///
/// #[derive(Drop, PartialEq)]
/// struct Wrapper {
///     bool: bool,
/// }
///
/// impl BitOrWrapper of BitOr<Wrapper> {
///     #[inline]
///     fn bitor(lhs: Wrapper, rhs: Wrapper) -> Wrapper {
///         Wrapper { bool: lhs.bool | rhs.bool }
///     }
/// }
///
/// fn main() {
///     assert!(Wrapper { bool: true } | Wrapper { bool: true } == Wrapper { bool: true });
///     assert!(Wrapper { bool: true } | Wrapper { bool: false } == Wrapper { bool: true });
///     assert!(Wrapper { bool: false } | Wrapper { bool: true } == Wrapper { bool: true });
///     assert!(Wrapper { bool: false } | Wrapper { bool: false } == Wrapper { bool: false });
/// }
/// ```
pub trait BitOr<T> {
    /// Computes the result of the OR operation between two values of the same type.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(1_u8 | 2_u8 == 3);
    /// ```
    fn bitor(lhs: T, rhs: T) -> T;
}

// TODO(spapini): When associated types are supported, support the general trait BitXor<X, Y>.
/// A trait for computing the bitwise XOR operator `^`.
///
/// # Examples
///
/// An implementation of `BitXor` for a wrapper around `bool`.
///
/// ```
/// use core::traits::BitXor;
///
/// #[derive(Drop, PartialEq)]
/// struct Wrapper {
///     bool: bool,
/// }
///
/// impl BitXorWrapper of BitXor<Wrapper> {
///     #[inline]
///     fn bitxor(lhs: Wrapper, rhs: Wrapper) -> Wrapper {
///         Wrapper { bool: lhs.bool ^ rhs.bool }
///     }
/// }
///
/// fn main() {
///     assert!(Wrapper { bool: true } ^ Wrapper { bool: true } == Wrapper { bool: false });
///     assert!(Wrapper { bool: true } ^ Wrapper { bool: false } == Wrapper { bool: true });
///     assert!(Wrapper { bool: false } ^ Wrapper { bool: true } == Wrapper { bool: true });
///     assert!(Wrapper { bool: false } ^ Wrapper { bool: false } == Wrapper { bool: false });
/// }
/// ```
pub trait BitXor<T> {
    /// Computes the result of the XOR operation between two values of the same type.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(1_u8 ^ 2_u8 == 3);
    /// ```
    fn bitxor(lhs: T, rhs: T) -> T;
}

/// A trait for computing the bitwise NOT operator `~`.
///
/// # Examples
///
/// An implementation of `BitNot` for a wrapper around `u8`.
///
/// ```
/// use core::traits::BitNot;
///
/// #[derive(Drop, PartialEq)]
/// struct Wrapper {
///     u8: u8,
/// }
///
/// impl BitNotWrapper of BitNot<Wrapper> {
///     #[inline]
///     fn bitnot(a: Wrapper) -> Wrapper {
///         Wrapper { u8: ~a.u8 }
///     }
/// }
///
/// fn main() {
///     assert!(~Wrapper { u8: 0 } == Wrapper { u8 : 255 });
///     assert!(~Wrapper { u8: 1 } == Wrapper { u8 : 254 });
/// }
/// ```
pub trait BitNot<T> {
    /// Computes the result of the NOT operation between two values of the same type.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(~1_u8 == 254);
    /// ```
    fn bitnot(a: T) -> T;
}

/// A trait for types that form a partial order.
///
/// The `lt`, `le`, `gt`, and `ge` methods of this trait can be called using the `<`, `<=`, `>`, and
/// `>=` operators, respectively.
pub trait PartialOrd<T> {
    /// Returns whether `lhs` is lower than `rhs` as a boolean.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(0_u8 < 1_u8);
    /// ```
    fn lt(lhs: T, rhs: T) -> bool;

    /// Returns whether `lhs` is greater than or equal to `rhs` as a boolean.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(1_u8 >= 1_u8);
    /// ```
    fn ge(lhs: T, rhs: T) -> bool {
        !Self::lt(lhs, rhs)
    }

    /// Returns whether `lhs` is greater than `rhs` as a boolean.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(1_u8 > 0_u8);
    /// ```
    fn gt(lhs: T, rhs: T) -> bool {
        Self::lt(rhs, lhs)
    }

    /// Returns whether `lhs` is lower than or equal to `rhs` as a boolean.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(1_u8 <= 1_u8);
    /// ```
    fn le(lhs: T, rhs: T) -> bool {
        Self::ge(rhs, lhs)
    }
}

impl PartialOrdSnap<T, +PartialOrd<T>, +Copy<T>> of PartialOrd<@T> {
    fn le(lhs: @T, rhs: @T) -> bool {
        PartialOrd::<T>::le(*lhs, *rhs)
    }

    fn ge(lhs: @T, rhs: @T) -> bool {
        PartialOrd::<T>::ge(*lhs, *rhs)
    }

    fn lt(lhs: @T, rhs: @T) -> bool {
        PartialOrd::<T>::lt(*lhs, *rhs)
    }

    fn gt(lhs: @T, rhs: @T) -> bool {
        PartialOrd::<T>::gt(*lhs, *rhs)
    }
}

/// A trait for conversion between types where the conversion is guaranteed to succeed.
pub trait Into<T, S> {
    /// Converts a type into another in a safely manner.
    ///
    /// # Examples
    ///
    /// ```
    /// let a: u8 = 1;
    /// let b: u16 = a.into();
    /// ```
    #[must_use]
    fn into(self: T) -> S;
}

impl TIntoT<T> of Into<T, T> {
    fn into(self: T) -> T {
        self
    }
}

/// A trait for conversion between types where the conversion is not guaranteed to succeed.
pub trait TryInto<T, S> {
    /// Converts a type into another and returns an option of the value if the conversion is
    /// successful, `Option::None` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// let a: u16 = 1;
    /// let b: u8 = a.try_into().unwrap();
    /// assert! (b == 1);
    /// ```
    fn try_into(self: T) -> Option<S>;
}

impl TryIntoFromInto<From, To, +Into<From, To>> of TryInto<From, To> {
    fn try_into(self: From) -> Option<To> {
        Option::Some(self.into())
    }
}

/// A trait for computing the unary negation operator `-`.
pub trait Neg<T> {
    /// Performs the unary `-` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// let x: i8 = 1;
    /// assert!(-x == -1);
    /// ```
    fn neg(a: T) -> T;
}

/// A trait for computing the unary logical negation operator `!`.
pub trait Not<T> {
    /// Performs the unary `!` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// let bool = false;
    /// assert!(!bool);
    /// ```
    fn not(a: T) -> T;
}

// The following two traits are for implementing the `[]` operator. Only one should be implemented
// for each type. Both are not consuming of `self`, the first gets a snapshot of the object and
// the second gets a reference.
#[deprecated(
    feature: "deprecated-index-traits", note: "Use `core::ops::index::IndexView`.", since: "2.7.0",
)]
pub trait IndexView<C, I, V> {
    fn index(self: @C, index: I) -> V;
}

#[deprecated(
    feature: "deprecated-index-traits", note: "Use `core::ops::index::Index`.", since: "2.7.0",
)]
pub trait Index<C, I, V> {
    fn index(ref self: C, index: I) -> V;
}

/// A trait that allows for custom destruction behavior of a type.
///
/// Types implementing this trait will have their `destruct` method called
/// automatically when they go out of scope.
///
/// This trait is particularly important for types that need to perform
/// cleanup operations or side-effects when being destroyed, such as
/// the `Felt252Dict` type that needs to be "squashed" before going out of scope.
pub trait Destruct<T> {
    /// Destroys the value, performing any necessary cleanup operations.
    ///
    /// This method is called automatically when the value goes out of scope,
    /// allowing for custom destruction behavior.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    ///
    /// let dict: Felt252Dict<u8> = Default::default();
    /// dict.destruct(); // Manual destruction during execution
    /// ```
    fn destruct(self: T) nopanic;
}

// TODO(spapini): Remove this, it can lead to multiple impls and unwanted Destruct implementation.
impl DestructFromDrop<T, +Drop<T>> of Destruct<T> {
    #[inline]
    fn destruct(self: T) nopanic {}
}

/// A trait that allows for destruction of a value in case of a panic.
///
/// This trait is automatically implemented from the `Destruct` implementation for a type.
pub trait PanicDestruct<T> {
    fn panic_destruct(self: T, ref panic: Panic) nopanic;
}

pub(crate) impl PanicDestructForDestruct<T, +Destruct<T>> of PanicDestruct<T> {
    #[inline]
    fn panic_destruct(self: T, ref panic: Panic) nopanic {
        Destruct::destruct(self);
    }
}

/// A trait for giving a default value to a type.
///
/// This trait is implemented for all primitive types in the core library.
///
/// It is possible to implement it on a custom type using the `#[derive(Default)]` attribute if all
/// its elements already implement `Default`.
///
/// It is also possible to implement it on enum types, declaring the default value of the enum by
/// using the `#[default]` attribute on one of its variants.
pub trait Default<T> {
    /// Creates a default instance for any type.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    ///
    /// #[derive(Default, Drop)]
    /// struct A {
    ///     item1: felt252,
    ///     item2: u64,
    /// }
    ///
    /// #[derive(Default, Drop)]
    /// enum CaseWithDefault {
    ///     A: felt252,
    ///     B: u128,
    ///     #[default]
    ///     C: u64,
    /// }
    ///
    /// let dict: Felt252Dict<u8> = Default::default();
    /// ````
    #[must_use]
    fn default() -> T;
}

impl SnapshotDefault<T, +Default<T>, +Drop<T>> of Default<@T> {
    #[inline]
    fn default() -> @T {
        @Default::default()
    }
}

/// A trait that allows to return default values for types used as values in a dictionary.
///
/// This trait must be implemented for any type that will be stored in a dictionary.
pub trait Felt252DictValue<T> {
    /// Returns the default value for this type as a value in a `Felt252Dict`.
    /// Should be logically equivalent to 0.
    #[must_use]
    fn zero_default() -> T nopanic;
}

pub(crate) impl TupleSize0Copy of Copy<()>;
pub(crate) impl TupleSize0Drop of Drop<()>;
impl TupleNextDrop<
    T,
    impl TH: crate::metaprogramming::TupleSplit<T>,
    +crate::metaprogramming::IsTuple<T>,
    +Drop<TH::Head>,
    +Drop<TH::Rest>,
> of Drop<T>;
impl TupleNextCopy<
    T,
    impl TH: crate::metaprogramming::TupleSplit<T>,
    +crate::metaprogramming::IsTuple<T>,
    +Copy<TH::Head>,
    +Copy<TH::Rest>,
> of Copy<T>;

impl TuplePartialEq<
    T,
    impl TSF: crate::metaprogramming::TupleSnapForward<T>,
    +TuplePartialEqHelper<TSF::SnapForward>,
> of PartialEq<T> {
    fn eq(lhs: @T, rhs: @T) -> bool {
        TuplePartialEqHelper::eq(TSF::snap_forward(lhs), TSF::snap_forward(rhs))
    }
    fn ne(lhs: @T, rhs: @T) -> bool {
        TuplePartialEqHelper::ne(TSF::snap_forward(lhs), TSF::snap_forward(rhs))
    }
}

// A trait helper for implementing `PartialEq` for tuples.
trait TuplePartialEqHelper<T> {
    fn eq(lhs: T, rhs: T) -> bool;
    fn ne(lhs: T, rhs: T) -> bool;
}

impl TuplePartialEqHelperByPartialEq<T, +PartialEq<T>> of TuplePartialEqHelper<@T> {
    fn eq(lhs: @T, rhs: @T) -> bool {
        lhs == rhs
    }
    fn ne(lhs: @T, rhs: @T) -> bool {
        lhs != rhs
    }
}

impl TuplePartialEqHelperBaseTuple of TuplePartialEqHelper<()> {
    fn eq(lhs: (), rhs: ()) -> bool {
        true
    }
    fn ne(lhs: (), rhs: ()) -> bool {
        false
    }
}

impl TuplePartialEqHelperBaseFixedSizedArray<T> of TuplePartialEqHelper<[@T; 0]> {
    fn eq(lhs: [@T; 0], rhs: [@T; 0]) -> bool {
        true
    }
    fn ne(lhs: [@T; 0], rhs: [@T; 0]) -> bool {
        false
    }
}

impl TuplePartialEqHelperNext<
    T,
    impl TS: crate::metaprogramming::TupleSplit<T>,
    +TuplePartialEqHelper<TS::Head>,
    +TuplePartialEqHelper<TS::Rest>,
    +Drop<TS::Rest>,
> of TuplePartialEqHelper<T> {
    fn eq(lhs: T, rhs: T) -> bool {
        let (lhs_head, lhs_rest) = TS::split_head(lhs);
        let (rhs_head, rhs_rest) = TS::split_head(rhs);
        TuplePartialEqHelper::<TS::Head>::eq(lhs_head, rhs_head)
            && TuplePartialEqHelper::<TS::Rest>::eq(lhs_rest, rhs_rest)
    }
    fn ne(lhs: T, rhs: T) -> bool {
        let (lhs_head, lhs_rest) = TS::split_head(lhs);
        let (rhs_head, rhs_rest) = TS::split_head(rhs);
        TuplePartialEqHelper::<TS::Head>::ne(lhs_head, rhs_head)
            || TuplePartialEqHelper::<TS::Rest>::ne(lhs_rest, rhs_rest)
    }
}

impl DefaultTupleBase of Default<()> {
    fn default() -> () {
        ()
    }
}

impl DefaultFixedSizedArray<T> of Default<[T; 0]> {
    fn default() -> [T; 0] {
        []
    }
}

impl DefaultNext<
    T,
    impl TS: crate::metaprogramming::TupleSplit<T>,
    +Default<TS::Head>,
    +Default<TS::Rest>,
    +Drop<TS::Head>,
> of Default<T> {
    fn default() -> T {
        TS::reconstruct(Default::default(), Default::default())
    }
}

impl FixedSizedArrayDrop<T, +Drop<T>, const N: u32> of Drop<[T; N]>;
impl FixedSizedArrayCopy<T, +Copy<T>, const N: u32> of Copy<[T; N]>;

impl TupleNextDestruct<
    T,
    impl TH: crate::metaprogramming::TupleSplit<T>,
    +Destruct<TH::Head>,
    +Destruct<TH::Rest>,
    -Drop<T>,
> of Destruct<T> {
    fn destruct(self: T) nopanic {
        let (_head, _rest) = TH::split_head(self);
    }
}
