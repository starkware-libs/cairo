mod traits;

pub use traits::iterator::Iterator;
#[unstable(feature: "collections-into-iter", note: "type bounds in traits not yet supported.")]
pub use traits::iterator::IntoIterator;
