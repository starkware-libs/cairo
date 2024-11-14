//! Traits module provides a collection of common traits and related functionality for working with
//! various types.
//!
//! The main components of this module are:
//!
//! - **Copy and Drop Traits**: The `Copy` and `Drop` traits, which define the behavior for
//!   copying and dropping values of a given type, respectively.
//! - **Arithmetic Traits**: Traits for standard arithmetic operations, such as `Add`, `Sub`,
//!   `Mul`, `Div`, `Rem`, and `DivRem`.
//! - **Comparison Traits**: Traits for comparing values, including `PartialEq` and `PartialOrd`.
//! - **Conversion Traits**: Traits for converting between types, including `Into` and `TryInto`.
//! - **Unary Operation Traits**: Traits for unary operations, such as `Neg` and `Not`.
//! - **Index Traits**: Deprecated traits for indexing into collections, replaced by `IndexView`
//!   and `Index` from the Index module.
//! - **Destruct Traits**: Traits for destructing values, including `Destruct` and `PanicDestruct`.
//! - **Default Trait**: The `Default` trait, which provides a way to create a default value of a
//!   given type.
//! - **Felt252Dict Value Trait**: The `Felt252DictValue` trait, which defines the requirements for
//!   values that can be stored in a `Felt252Dict`.
//! - **Tuple Traits**: Implementations of various traits (such as `Copy`, `Drop`, `PartialEq`,
//!   and `Default`) for tuple-like types.
//!
//! These traits and their implementations provide a consistent and type-safe way to work with
//! values, enabling operations such as arithmetic, comparison, conversion, and destruction.

use crate::panics::Panic;

/// `Copy` trait for copying values.
pub trait Copy<T>;

/// `Drop` trait for dropping values.
pub trait Drop<T>;

/// `Copy` implementation for a snapshot of any type.
impl SnapshotCopy<T> of Copy<@T>;

/// `Drop` implementation for a snapshot of any type.
impl SnapshotDrop<T> of Drop<@T>;

// TODO(spapini): When associated types are supported, support the general trait Add<X, Y>.
/// The addition operator `+`.
pub trait Add<T> {
    /// Performs the `+` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// #[derive(Copy, Drop, PartialEq)]
    /// struct Point {
    ///     x: u32,
    ///     y: u32,
    /// }
    ///
    /// impl PointAdd of Add<Point>{
    ///     fn add(lhs: Point, rhs: Point) -> Point {
    ///         Point {
    ///             x: lhs.x + rhs.x,
    ///             y: lhs.y + rhs.y,
    ///         }
    ///     }
    /// }
    ///
    /// fn main(){
    ///     let p1 = Point { x: 1, y: 0 };
    ///     let p2 = Point { x: 2, y: 3 };
    ///     let p3 = p1 + p2;
    ///     assert!(p3 == Point { x: 3, y: 3 });
    /// }
    /// ```
    fn add(lhs: T, rhs: T) -> T;
}

#[deprecated(
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::AddAssign`.", since: "2.7.0"
)]
pub trait AddEq<T> {
    fn add_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Sub<X, Y>.
/// The subtraction operator `-`.
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
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::SubAssign`.", since: "2.7.0"
)]
pub trait SubEq<T> {
    fn sub_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Mul<X, Y>.
/// The multiplication operator `*`.
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
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::MulAssign`.", since: "2.7.0"
)]
pub trait MulEq<T> {
    fn mul_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Div<X, Y>.
/// The division operator `/`.
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
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::DivAssign`.", since: "2.7.0"
)]
pub trait DivEq<T> {
    fn div_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Rem<X, Y>.
/// The remainder operator `%`.
pub trait Rem<T> {
    /// Performs the `%` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(12 % 10 == 2);
    /// ```
    fn rem(lhs: T, rhs: T) -> T;
}

#[deprecated(
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::RemAssign`.", since: "2.7.0"
)]
pub trait RemEq<T> {
    fn rem_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait DivRem<X, Y>.
/// Combination of the division operator `/` and the remainder operator `%`.
pub trait DivRem<T> {
    /// Performs the `/` and the `%` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::traits::DivRem;
    ///
    /// let numerator: u32 = 12;
    /// let denominator: NonZero<u32> = 10;
    /// assert!(DivRem::div_rem(numerator, denominator) == (1, 2));
    /// ```
    fn div_rem(lhs: T, rhs: NonZero<T>) -> (T, T);
}

/// Trait for comparisons using the equality operator.
///
/// Implementing this trait for types provides the `==` and `!=` operators for
/// those types.
pub trait PartialEq<T> {
    /// Returns whether `lhs` and `rhs` equal, and is used by `==`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(1 == 1);
    /// ```
    fn eq(lhs: @T, rhs: @T) -> bool;

    /// Returns whether `lhs` and `rhs` are not equal, and is used by `!=`
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
/// Trait for computing the AND bitwise operation between two values of the same type.
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
/// Trait for computing the OR bitwise operation between two values of the same type.
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
/// Trait for computing the XOR bitwise operation between two values of the same type.
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

/// Trait for computing the NOT bitwise operation between two values of the same type.
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

/// Trait for comparing two values of the same type.
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

/// Trait for safe conversion between types.
pub trait Into<T, S> {
    /// Converts a type into another in safely manner.
    ///
    /// # Exampels
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

/// Trait for fallible conversion between types.
pub trait TryInto<T, S> {
    /// Converts a type into another and returns an option of the value if the conversion is
    /// successful.
    ///
    /// # Panics
    ///
    /// Panics if the conversion is not successful.
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

/// Trait for computing the negated value of a value of any type.
pub trait Neg<T> {
    /// Computes the negation of a given value.
    ///
    /// # Examples
    ///
    /// ```
    /// let x: i8 = 1;
    /// assert!(-x == -1);
    /// ```
    fn neg(a: T) -> T;
}

/// Trait for computing the logically negated value of a value of any type.
pub trait Not<T> {
    /// Computes the logical negation of a given value.
    ///
    /// # Examples
    ///
    /// ```
    /// let bool = false;
    /// assert!(!bool);
    /// ```
    fn not(a: T) -> T;
}

#[deprecated(
    feature: "deprecated-index-traits", note: "Use `core::ops::index::IndexView`.", since: "2.7.0"
)]
pub trait IndexView<C, I, V> {
    fn index(self: @C, index: I) -> V;
}

#[deprecated(
    feature: "deprecated-index-traits", note: "Use `core::ops::index::Index`.", since: "2.7.0"
)]
pub trait Index<C, I, V> {
    fn index(ref self: C, index: I) -> V;
}

/// Trait for destructing a value of any type.
pub trait Destruct<T> {
    /// Manually destruct and removing a type instance out of scope.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    ///
    /// let dict: Felt252Dict<u8> = Default::default();
    /// dict.destruct();
    /// ```
    fn destruct(self: T) nopanic;
}

// TODO(spapini): Remove this, it can lead to multiple impls and unwanted Destruct implementation.
impl DestructFromDrop<T, +Drop<T>> of Destruct<T> {
    #[inline]
    fn destruct(self: T) nopanic {}
}

/// Trait for destructing a value of any type in the case of a panic.
pub trait PanicDestruct<T> {
    fn panic_destruct(self: T, ref panic: Panic) nopanic;
}

pub(crate) impl PanicDestructForDestruct<T, +Destruct<T>> of PanicDestruct<T> {
    #[inline]
    fn panic_destruct(self: T, ref panic: Panic) nopanic {
        Destruct::destruct(self);
    }
}

/// Trait for creating a default value of any type.
pub trait Default<T> {
    /// Creates a default instance for any type.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
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

/// Trait that allows to return default values for types used as values in a dictionary.
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
    +Drop<TH::Rest>
> of Drop<T>;
impl TupleNextCopy<
    T,
    impl TH: crate::metaprogramming::TupleSplit<T>,
    +crate::metaprogramming::IsTuple<T>,
    +Copy<TH::Head>,
    +Copy<TH::Rest>
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
