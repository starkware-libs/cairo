//! Traits and utilities for iterating over sequences of values.
//!
//! # Overview
//!
//! This module defines the basic abstraction for iteration in Cairo, allowing for generic
//! and efficient traversal over collections. The `Iterator` trait is fundamental for
//! implementing iterators, which are used to perform operations like mapping, filtering,
//! and reducing over collections. The `IntoIterator` trait is also very useful as it allows to
//! convert any collection into an iterator.
//!
//! # Traits
//!
//! - [`Iterator`]: Defines the method to advance an iterator and retrieve the next value.
//! - [`IntoIterator`]: Allows to convert a collection into an iteror.
//!
//! [`Iterator`]: crate::iter::Iterator
//! [`IntoIterator`]: crate::iter::IntoIterator

mod traits;
pub use traits::{IntoIterator, Iterator};
