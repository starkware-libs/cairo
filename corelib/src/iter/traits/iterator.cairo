//! Traits and utilities for iterating over sequences of values.
//!
//! # Overview
//!
//! This module defines the basic abstraction for iteration in Cairo, allowing for generic
//! and efficient traversal over collections. The `Iterator` trait is fundamental for
//! implementing iterators, which are used to perform operations like mapping, filtering,
//! and reducing over collections.
//!
//! # Traits
//!
//! - [`Iterator`]: Defines the method to advance an iterator and retrieve the next value.
//!
//! # Examples
//!
//! Basic usage of an iterator:
//!
//! ```
//! use core::iter::Iterator;
//!
//! #[derive(Drop, Copy)]
//! pub struct Counter {
//!     pub count: u32,
//! }
//!
//! impl CounterIterator of Iterator<Counter> {
//!     type Item = u32;
//!
//!     fn next(ref self: Counter) -> Option<Self::Item> {
//!         if self.count < 5 {
//!             self.count += 1;
//!             Option::Some(self.count)
//!         } else {
//!             Option::None
//!         }
//!     }
//! }
//!
//! fn main() {
//!     let mut counter = Counter { count: 0 };
//!     assert!(counter.next() == Option::Some(1));
//!     assert!(counter.next() == Option::Some(2));
//!     assert!(counter.next() == Option::Some(3));
//!     assert!(counter.next() == Option::Some(4));
//!     assert!(counter.next() == Option::Some(5));
//!     assert!(counter.next() == Option::None);
//! }
//! ```

/// An iterator over a collection of values.
pub trait Iterator<T> {
    /// The type of the elements being iterated over.
    type Item;

    /// Advance the iterator and return the next value.
    fn next(ref self: T) -> Option<Self::Item>;
}
