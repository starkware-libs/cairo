//! Debug utilities for types that need a salsa database for debug formatting.

pub mod debug;
#[cfg(feature = "heapsize")]
pub mod heap_size;

pub use debug::{DebugWithDb, helper};
#[cfg(feature = "heapsize")]
pub use heap_size::{HeapSize, heap_size};
