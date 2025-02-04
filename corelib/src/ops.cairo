//! Overloadable operators.
//!
//! Implementing these traits allows you to overload certain operators.
//!
//! > Note: Other overloadable operators are also defined in the [`core::traits`] module.
//!
//! Only operators backed by traits can be overloaded. For
//! example, the addition assignment operator (`+=`) can be overloaded through the [`AddAssign`]
//! trait, but since the assignment operator (`=`) has no backing trait, there
//! is no way of overloading its semantics. Additionally, this module does not
//! provide any mechanism to create new operators.
//!
//! Implementations of operator traits should be unsurprising in their
//! respective contexts, keeping in mind their usual meanings and
//! operator precedence. For example, when implementing [`MulAssign`], the operation
//! should have some resemblance to multiplication assignment.
//!
//! # Examples
//!
//! This example creates a `Point` struct that implements [`AddAssign`] and [`SubAssign`],
//! and then demonstrates adding and subtracting two `Point`s to themselves.
//!
//! ```
//! use core::ops::{AddAssign, SubAssign};
//!
//! #[derive(Debug, Drop, Copy, PartialEq)]
//! struct Point {
//!     x: i32,
//!     y: i32,
//! }
//!
//! impl AddAssignImpl of AddAssign<Point, Point> {
//!     fn add_assign(ref self: Point, rhs: Point) {
//!         self = Point { x: self.x + rhs.x, y: self.y + rhs.y }
//!     }
//! }
//!
//! impl SubAssignImpl of SubAssign<Point, Point> {
//!     fn sub_assign(ref self: Point, rhs: Point) {
//!         self = Point { x: self.x - rhs.x, y: self.y - rhs.y }
//!     }
//! }
//!
//! fn main() {
//!     let mut point = Point {x : 3, y: 4};
//!     point += point;
//!     assert!(point == Point {x: 6, y: 8});
//!     point -= point;
//!     assert!(point == Point {x: 0, y: 0});
//! }
//! ```
//!
//! See the documentation for each trait for an example implementation.

mod arith;
pub use arith::{AddAssign, DivAssign, MulAssign, RemAssign, SubAssign};

mod deref;
pub use deref::Deref;
#[feature("deref_mut")]
pub use deref::DerefMut;

mod function;
pub use function::{Fn, FnOnce};

pub mod index;
pub use index::{Index, IndexView};

mod range;
pub use range::{
    Range, RangeInclusive, RangeInclusiveIterator, RangeInclusiveTrait, RangeIterator, RangeTrait,
};
// `RangeOp` and `RangeInclusiveOp` are used internally by the compiler.
#[allow(unused_imports)]
use range::{RangeInclusiveOp, RangeOp};
