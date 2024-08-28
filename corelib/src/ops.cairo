pub mod index;
pub use index::{Index, IndexView};

mod arith;
pub use arith::{AddAssign, SubAssign, MulAssign, DivAssign, RemAssign};
mod deref;
pub use deref::{Deref, SnapshotDeref};
#[feature("deref_mut")]
pub use deref::DerefMut;
mod range;
pub use range::Range;
// `RangeOp` is used internally by the compiler.
#[allow(unused_imports)]
use range::RangeOp;

mod function;
pub use function::FnOnce;
