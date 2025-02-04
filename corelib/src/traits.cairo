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
/// # Examples
///
/// `Add`able types:
///
/// ```
/// assert!(1_u8 + 2_u8 == 3_u8);
/// ```
///
/// Implementing `Add` for a type:
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
/// # Examples
///
/// `Sub`tractable types:
///
/// ```
/// assert!(3_u8 - 2_u8 == 1_u8);
/// ```
///
/// Implementing `Sub` for a type:
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
/// # Examples
///
/// `Mul`tipliable types:
///
/// ```
/// assert!(3_u8 * 2_u8 == 6_u8);
/// ```
///
/// Implementing `Mul` for a type:
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
/// `Div`isible types:
///
/// ```
/// assert!(4_u8 / 2_u8 == 2_u8);
/// ```
///
/// Implementing `Div` for a type:
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
/// Performs truncated division and remainder.
///
/// This trait provides a way to efficiently compute both the quotient and remainder in a single
/// operation. The division truncates towards zero, matching the behavior of the `/` and `%`
/// operators.
///
/// # Examples
///
/// ```
/// assert!(DivRem::div_rem(7_u32, 3) == (2, 1));
/// ```
pub trait DivRem<T> {
    /// Performs the `/` and the `%` operations, returning both the quotient and remainder.
    ///
    /// # Examples
    ///
    /// ```
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
/// # Implementing `PartialEq`
///
/// An example in which two points are equal if their x and y coordinates are equal.
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
/// The bitwise AND operator `&`.
///
/// # Examples
///
/// An implementation of `BitAnd` for a wrapper around `bool`.
///
/// ```
/// use core::traits::BitAnd;
///
/// #[derive(Drop, PartialEq)]
/// struct Scalar {
///     inner: bool,
/// }
///
/// impl BitAndScalar of BitAnd<Scalar> {
///     fn bitand(lhs: Scalar, rhs: Scalar) -> Scalar {
///        Scalar { inner: lhs.inner & rhs.inner }
///     }
/// }
///
/// assert!(Scalar { inner: true } & Scalar { inner: true } == Scalar { inner: true });
/// assert!(Scalar { inner: true } & Scalar { inner: false } == Scalar { inner: false });
/// assert!(Scalar { inner: false } & Scalar { inner: true } == Scalar { inner: false });
/// assert!(Scalar { inner: false } & Scalar { inner: false } == Scalar { inner: false });
/// ```
pub trait BitAnd<T> {
    /// Performs the `&` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(true & false, false);
    /// assert_eq!(5_u8 & 1_u8, 1);
    /// assert_eq!(true & true, true);
    /// assert_eq!(5_u8 & 2_u8, 0);
    fn bitand(lhs: T, rhs: T) -> T;
}

// TODO(spapini): When associated types are supported, support the general trait BitOr<X, Y>.
/// The bitwise OR operator `|`.
///
/// # Examples
///
/// An implementation of `BitOr` for a wrapper around `bool`.
///
/// ```
/// use core::traits::BitOr;
///
/// #[derive(Drop, PartialEq)]
/// struct Scalar {
///     inner: bool,
/// }
///
/// impl BitOrScalar of BitOr<Scalar> {
///     fn bitor(lhs: Scalar, rhs: Scalar) -> Scalar {
///         Scalar { inner: lhs.inner | rhs.inner }
///     }
/// }
///
/// assert!(Scalar { inner: true } | Scalar { inner: true } == Scalar { inner: true });
/// assert!(Scalar { inner: true } | Scalar { inner: false } == Scalar { inner: true });
/// assert!(Scalar { inner: false } | Scalar { inner: true } == Scalar { inner: true });
/// assert!(Scalar { inner: false } | Scalar { inner: false } == Scalar { inner: false });
/// ```
pub trait BitOr<T> {
    /// Performs the `|` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(1_u8 | 2_u8 == 3);
    /// ```
    fn bitor(lhs: T, rhs: T) -> T;
}

// TODO(spapini): When associated types are supported, support the general trait BitXor<X, Y>.
/// The bitwise XOR operator `^`.
///
/// # Examples
///
/// An implementation of `BitXor` for a wrapper around `bool`.
///
/// ```
/// use core::traits::BitXor;
///
/// #[derive(Drop, PartialEq)]
/// struct Scalar {
///     inner: bool,
/// }
///
/// impl BitXorScalar of BitXor<Scalar> {
///     fn bitxor(lhs: Scalar, rhs: Scalar) -> Scalar {
///         Scalar { inner: lhs.inner ^ rhs.inner }
///     }
/// }
///
/// assert!(Scalar { inner: true } ^ Scalar { inner: true } == Scalar { inner: false });
/// assert!(Scalar { inner: true } ^ Scalar { inner: false } == Scalar { inner: true });
/// assert!(Scalar { inner: false } ^ Scalar { inner: true } == Scalar { inner: true });
/// assert!(Scalar { inner: false } ^ Scalar { inner: false } == Scalar { inner: false });
/// ```
pub trait BitXor<T> {
    /// Performs the `^` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(1_u8 ^ 2_u8 == 3);
    /// ```
    fn bitxor(lhs: T, rhs: T) -> T;
}

/// The bitwise NOT operator `~`.
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
///     fn bitnot(a: Wrapper) -> Wrapper {
///         Wrapper { u8: ~a.u8 }
///     }
/// }
///
/// assert!(~Wrapper { u8: 0 } == Wrapper { u8 : 255 });
/// assert!(~Wrapper { u8: 1 } == Wrapper { u8 : 254 });
/// ```
pub trait BitNot<T> {
    /// Performs the `~` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(~1_u8 == 254);
    /// ```
    fn bitnot(a: T) -> T;
}

/// Trait for comparing types that form a [partial
/// order](https://en.wikipedia.org/wiki/Partial_order).
///
/// The `lt`, `le`, `gt`, and `ge` methods of this trait can be called using the `<`, `<=`, `>`, and
/// `>=` operators, respectively.
///
/// PartialOrd is not derivable, but can be implemented manually
///
/// # Implementing `PartialOrd`
///
/// Here's how to implement `PartialOrd` for a custom type. This example implements
/// comparison operations for a 2D point where points are compared based on their
/// squared Euclidean distance from the origin (0,0):
///
/// ```
/// #[derive(Copy, Drop, PartialEq)]
/// struct Point {
///     x: u32,
///     y: u32,
/// }
///
/// impl PointPartialOrd of PartialOrd<Point> {
///     fn lt(lhs: Point, rhs: Point) -> bool {
///         let lhs_dist = lhs.x * lhs.x + lhs.y * lhs.y;
///         let rhs_dist = rhs.x * rhs.x + rhs.y * rhs.y;
///         lhs_dist < rhs_dist
///     }
/// }
///
/// let p1 = Point { x: 1, y: 1 }; // distance = 2
/// let p2 = Point { x: 2, y: 2 }; // distance = 8
///
/// assert!(p1 < p2);
/// assert!(p1 <= p2);
/// assert!(p2 > p1);
/// assert!(p2 >= p1);
/// ```
///
/// Note that only the `lt` method needs to be implemented. The other comparison
/// operations (`le`, `gt`, `ge`) are automatically derived from `lt`. However,
/// you can override them for better performance if needed.
pub trait PartialOrd<T> {
    /// Tests less than (for `self` and `other`) and is used by the `<` operator.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(1 < 1, false);
    /// assert_eq!(1 < 2, true);
    /// assert_eq!(2 < 1, false);
    /// ```
    fn lt(lhs: T, rhs: T) -> bool;

    /// Tests less than or equal to (for `self` and `other`) and is used by the
    /// `<=` operator.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(1 <= 1, true);
    /// assert_eq!(1 <= 2, true);
    /// assert_eq!(2 <= 1, false);
    /// ```
    fn ge(lhs: T, rhs: T) -> bool {
        !Self::lt(lhs, rhs)
    }

    /// Tests greater than (for `self` and `other`) and is used by the `>`
    /// operator.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(1 > 1, false);
    /// assert_eq!(1 > 2, false);
    /// assert_eq!(2 > 1, true);
    /// ```
    fn gt(lhs: T, rhs: T) -> bool {
        Self::lt(rhs, lhs)
    }

    /// Tests greater than or equal to (for `self` and `other`) and is used by
    /// the `>=` operator.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(1 >= 1, true);
    /// assert_eq!(1 >= 2, false);
    /// assert_eq!(2 >= 1, true);
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

/// A value-to-value conversion that consumes the input value.
///
/// **Note: This trait must not fail**. If the conversion can fail, use [`TryInto`].
///
/// # Generic Implementations
///
/// - [`Into`] is reflexive, which means that `Into<T, T>` is implemented
///
/// # Examples
///
/// Converting from RGB components to a packed color value:
///
/// ```
/// #[derive(Copy, Drop, PartialEq)]
/// struct Color {
///     // Packed as 0x00RRGGBB
///     value: u32,
/// }
///
/// impl RGBIntoColor of Into<(u8, u8, u8), Color> {
///     fn into(self: (u8, u8, u8)) -> Color {
///         let (r, g, b) = self;
///         let value = (r.into() * 0x10000_u32) +
///                    (g.into() * 0x100_u32) +
///                    b.into();
///         Color { value }
///     }
/// }
///
/// // Convert RGB(255, 128, 0) to 0x00FF8000
/// let orange: Color = (255_u8, 128_u8, 0_u8).into();
/// assert!(orange == Color { value: 0x00FF8000_u32 });
/// ```
pub trait Into<T, S> {
    /// Converts the input type T into the output type S.
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

/// Simple and safe type conversions that may fail in a controlled way under
/// some circumstances.
///
/// This is useful when you are doing a type conversion that may trivially succeed but may also need
/// special handling. For example, there is no way to convert an [`i64`] into an [`i32`] using the
/// [`Into`] trait, because an [`i64`] may contain a value that an [`i32`] cannot represent and so
/// the conversion would lose data.  This might be handled by truncating the [`i64`] to an [`i32`]
/// or by simply returning [`Bounded::<i32>::MAX`], or by some other method. The [`Into`] trait
/// is intended for perfect conversions, so the `TryInto` trait informs the programmer when a type
/// conversion could go bad and lets them decide how to handle it.
///
/// # Generic Implementations
///
/// - [`TryInto`] is reflexive, which means that `TryInto<T, T>` is implemented
/// - [`TryInto`] is implemented for all types that implement [`Into`]
///
/// # Examples
///
/// Converting chess coordinates (like 'e4') into a validated position:
///
/// ```
/// #[derive(Copy, Drop, PartialEq)]
///  struct Position {
///      file: u8, // Column a-h (0-7)
///      rank: u8, // Row 1-8 (0-7)
///  }
///
///  impl TupleTryIntoPosition of TryInto<(u8, u8), Position> {
///     fn try_into(self: (u8, u8)) -> Option<Position> {
///         let (file_char, rank) = self;
///
///         // Validate rank is between 1 and 8
///         if rank < 1 || rank > 8 {
///             return None;
///         }
///
///         // Validate and convert file character (a-h) to number (0-7)
///         if file_char < 'a' || file_char > 'h' {
///             return None;
///         }
///         let file = file_char - 'a';
///
///         Some(Position {
///             file,
///             rank: rank - 1 // Convert 1-8 (chess notation) to 0-7 (internal index)
///         })
///     }
/// }
///
/// // Valid positions
/// let e4 = ('e', 4).try_into();
/// assert!(e4 == Some(Position { file: 4, rank: 3 }));
///
/// // Invalid positions
/// let invalid_file = ('x', 4).try_into();
/// let invalid_rank = ('a', 9).try_into();
/// assert!(invalid_file == None);
/// assert!(invalid_rank == None);
/// ```
pub trait TryInto<T, S> {
    /// Attempts to convert the input type T into the output type S.
    /// In the event of a conversion error, returns [`None`].
    ///
    /// # Examples
    ///
    /// ```
    /// let a: Option<u8> = 1_u16.try_into();
    /// assert!(a == Some(1));
    /// let b: Option<u8> = 256_u16.try_into();
    /// assert!(b == None);
    /// ```
    fn try_into(self: T) -> Option<S>;
}

impl TryIntoFromInto<From, To, +Into<From, To>> of TryInto<From, To> {
    fn try_into(self: From) -> Option<To> {
        Some(self.into())
    }
}

/// The unary negation operator `-`.
///
/// # Examples
///
/// An implementation of `Neg` for `Sign`, which allows the use of `-` to
/// negate its value.
///
/// ```
/// #[derive(Copy, Drop, PartialEq)]
/// enum Sign {
///     Negative,
///     Zero,
///     Positive,
/// }
///
/// impl SignNeg of Neg<Sign> {
///     fn neg(a: Sign) -> Sign {
///         match a {
///             Sign::Negative => Sign::Positive,
///             Sign::Zero => Sign::Zero,
///             Sign::Positive => Sign::Negative,
///         }
///     }
/// }
///
/// // A negative positive is a negative
/// assert!(-Sign::Positive == Sign::Negative);
/// // A double negative is a positive
/// assert!(-Sign::Negative == Sign::Positive);
/// // Zero is its own negation
/// assert!(-Sign::Zero == Sign::Zero);
/// ```
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

/// The unary logical negation operator `!`.
///
/// # Examples
///
/// An implementation of `Not` for `Answer`, which enables the use of `!` to
/// invert its value.
///
/// ```
/// #[derive(Drop, PartialEq)]
/// enum Answer {
///     Yes,
///     No,
/// }
///
/// impl AnswerNot of Not<Answer> {
///     fn not(a: Answer) -> Answer {
///         match a {
///             Answer::Yes => Answer::No,
///             Answer::No => Answer::Yes,
///         }
///     }
/// }
///
/// assert!(!Answer::Yes == Answer::No);
/// assert!(!Answer::No == Answer::Yes);
/// ```
pub trait Not<T> {
    /// Performs the unary `!` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(!true == false);
    /// assert!(!false == true);
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
/// In Cairo, values must be explicitly handled - they cannot be silently dropped.
/// Types can only go out of scope in two ways:
/// 1. Implement `Drop` - for types that can be discarded trivially
/// 2. Implement `Destruct` - for types that need cleanup when destroyed. Typically, any type that
/// contains
///    a `Felt252Dict` must implement `Destruct`, as the `Felt252Dict` needs to be "squashed" when
///    going
/// out of scope to ensure a program is sound.
///
/// Generally, `Destruct` does not need to be implemented manually. It can be derived from the
/// `Drop` and `Destruct` implementations of the type's fields.
///
/// # Examples
///
/// Here's a simple type that wraps a `Felt252Dict` and needs to be destructed:
///
/// ```
/// use core::dict::Felt252Dict;
///
/// // A struct containing a Felt252Dict must implement Destruct
/// #[derive(Destruct, Default)]
/// struct ResourceManager {
///     resources: Felt252Dict<u32>,
///     count: u32,
/// }
///
/// #[generate_trait]
/// impl ResourceManagerImpl of ResourceManagerTrait{
///    fn add_resource(ref self: ResourceManager, resource_id: felt252, amount: u32){
///        assert!(self.resources.get(resource_id) == 0, "Resource already exists");
///        self.resources.insert(resource_id, amount);
///        self.count += amount;
///    }
/// }
///
/// let mut manager = Default::default();
///
/// // Add some resources
/// manager.add_resource(1, 100);
///
/// // When manager goes out of scope here, Destruct is automatically called,
/// // which ensures the dictionary is properly squashed
/// ```
pub trait Destruct<T> {
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

/// A trait for giving a type a useful default value.
///
/// Cairo implements `Default` for various primitives types.
///
/// # Derivable
///
/// This trait can be used with `#[derive]` if all of the type's fields implement
/// `Default`. When `derive`d, it will use the default value for each field's type.
///
/// ## `enum`s
///
/// When using `#[derive(Default)]` on an `enum`, you need to choose which unit variant will be
/// default. You do this by placing the `#[default]` attribute on the variant.
///
/// ```
/// #[derive(Default)]
/// enum Kind {
///     #[default]
///     A,
///     B,
///     C,
/// }
/// ```
///
/// You can even use the `#[default]` attribute even on non-unit variants, provided that the
/// associated type implements `Default`.
///
/// # How can I implement `Default`?
///
/// Provide an implementation for the `default()` method that returns the value of
/// your type that should be the default:
///
/// ```
/// #[derive(Copy, Drop)]
/// enum Kind {
///     A,
///     B,
///     C,
/// }
///
/// impl DefaultKind of Default<Kind> {
///     fn default() -> Kind { Kind::A }
/// }
/// ```
///
/// # Examples
///
/// #[derive(Drop, Default, PartialEq)]
/// struct SomeOptions {
///     foo: i32,
///     bar: u32,
/// }
///
/// assert!(Default::default() == SomeOptions { foo: 0, bar: 0 });
pub trait Default<T> {
    /// Returns the "default value" for a type.
    ///
    /// Default values are often some kind of initial value, identity value, or anything else that
    /// may make sense as a default.
    ///
    /// # Examples
    ///
    /// ```
    /// let i: i8 = Default::default();
    /// let (x, y): (Option<ByteArray>, u64) = Default::default();
    /// let (a, b, (c, d)): (i32, u32, (bool, bool)) = Default::default();
    /// ```
    #[must_use]
    fn default() -> T;
}

impl SnapshotDefault<T, +Default<T>, +Drop<T>> of Default<@T> {
    #[inline]
    fn default() -> @T {
        @Default::default()
    }
}

/// A trait that must be implemented for any type that will be stored as a value in a `Felt252Dict`.
///
/// When working with dictionaries in Cairo, we need a way to represent "empty" or "uninitialized"
/// slots. This trait provides a zero-like default value that is returned when accessing a key
/// that hasn't been explicitly set.
///
/// # Why is this needed?
///
/// The `Felt252Dict` implementation needs to handle cases where a key hasn't been assigned a value
/// yet.
/// Instead of using `Option` or similar constructs, it uses a zero-like value specific to each
/// type.
///
/// This trait is __only__ implemented for primitive scalar types and `Nullable<T>`. It cannot be
/// implemented manually.
/// Instead, if you want to use a custom type as a value in a dictionary, you can wrap your type in
/// a [`Nullable`], which implements `Felt252DictValue` for any wrapped type.
///
/// # Examples
///
/// ```
/// use core::dict::Felt252Dict;
///
/// #[derive(Copy, Drop, Default)]
/// struct Counter {
///     value: u32,
/// }
///
///  // u8 already implements Felt252DictValue
///  let mut dict: Felt252Dict<u8> = Default::default();
///  assert!(dict.get(123) == 0);
///
///  // Counter is wrapped in a Nullable, as it doesn't implement Felt252DictValue
///  let mut counters: Felt252Dict<Nullable<Counter>> = Default::default();
///
///  // If the key is not set, `deref` would panic. `deref_or` returns the default value.
///  let maybe_counter: Nullable<Counter> = counters.get(123);
///  assert!(maybe_counter.deref_or(Default::default()).value == 0);
/// ```
pub trait Felt252DictValue<T> {
    /// Returns the default value for this type when used in a `Felt252Dict`.
    /// This value should be logically equivalent to zero or an "empty" state.
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
