mod adapters;
mod traits;
pub use adapters::Chain;
pub(crate) use adapters::chained_iterator;
pub use traits::{IntoIterator, Iterator};
