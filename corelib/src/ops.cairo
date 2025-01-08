pub mod index;
pub use index::{Index, IndexView};

mod arith;
pub use arith::{AddAssign, DivAssign, MulAssign, RemAssign, SubAssign};
mod deref;
pub use deref::Deref;
#[feature("deref_mut")]
pub use deref::DerefMut;
mod range;
// `RangeOp` is used internally by the compiler.
#[allow(unused_imports)]
use range::RangeOp;
pub use range::{Range, RangeIterator, RangeTrait};

mod function;
pub use function::{Fn, FnOnce};
