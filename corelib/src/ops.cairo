pub mod index;
pub use index::{Index, IndexView};

mod arith;
pub use arith::{AddAssign, DivAssign, MulAssign, RemAssign, SubAssign};
mod deref;
#[feature("deref_mut")]
pub use deref::DerefMut;
pub use deref::{Deref, SnapshotDeref};
mod range;
// `RangeInclusiveOp` is used internally by the compiler.
#[allow(unused_imports)]
use range::RangeInclusiveOp;
// `RangeOp` is used internally by the compiler.
#[allow(unused_imports)]
use range::RangeOp;
pub use range::{Range, RangeInclusive, RangeInclusiveIterator, RangeIterator, RangeTrait};

mod function;
pub use function::{Fn, FnOnce};
