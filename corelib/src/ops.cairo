pub mod index;
pub use index::{Index, IndexView};

mod arith;
pub use arith::{AddAssign, DivAssign, MulAssign, RemAssign, SubAssign};
mod deref;
#[feature("deref_mut")]
pub use deref::DerefMut;
pub use deref::{Deref, SnapshotDeref};
mod range;
pub use range::{Range, RangeInclusive, RangeInclusiveIterator, RangeIterator, RangeTrait};
// `RangeOp` and `RangeInclusiveOp` are used internally by the compiler.
#[allow(unused_imports)]
use range::{RangeInclusiveOp, RangeOp};

mod function;
pub use function::{Fn, FnOnce};
