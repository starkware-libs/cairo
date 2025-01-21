//! Composable external iteration.
//!
//! If you've found yourself with a collection of some kind, and needed to
//! perform an operation on the elements of said collection, you'll quickly run
//! into 'iterators'. Iterators are heavily used in idiomatic code, so
//! it's worth becoming familiar with them.
//!
//! Before explaining more, let's talk about how this module is structured:
//!
//! # Organization
//!
//! This module is largely organized by type:
//!
//! * [Traits] are the core portion: these traits define what kind of iterators
//!   exist and what you can do with them. The methods of these traits are worth
//!   putting some extra study time into.
//! * [Functions] provide some helpful ways to create some basic iterators.
//! * [Structs] are often the return types of the various methods on this
//!   module's traits. You'll usually want to look at the method that creates
//!   the `struct`, rather than the `struct` itself. For more detail about why,
//!   see '[Implementing Iterator](#implementing-iterator)'.
//!
//! [Traits]: #traits
//! [Functions]: #functions
//! [Structs]: #structs
//!
//! That's it! Let's dig into iterators.
//!
//! # Iterator
//!
//! The heart and soul of this module is the [`Iterator`] trait. The core of
//! [`Iterator`] looks like this:
//!
//! ```
//! trait Iterator {
//!     type Item;
//!     fn next(ref self) -> Option<Self::Item>;
//! }
//! ```
//!
//! An iterator has a method, [`next`], which when called, returns an
//! <code>[Option]\<Item></code>. Calling [`next`] will return [`Some(Item)`] as long as
//! there are elements, and once they've all been exhausted, will return `None` to
//! indicate that iteration is finished.
//!
//! [`Iterator`]'s full definition includes a number of other methods as well,
//! but they are default methods, built on top of [`next`], and so you get
//! them for free.
//!
//! Iterators are also composable, and it's common to chain them together to do
//! more complex forms of processing. See the [Adapters](#adapters) section
//! below for more details.
//!
//! [`Some(Item)`]: Some
//! [`next`]: Iterator::next
//!
//! # Forms of iteration
//!
//! There is currently only one common method which can create iterators from a collection:
//!
//! * `into_iter()`, which iterates over `T`.
//!
//! # Implementing Iterator
//!
//! Creating an iterator of your own involves two steps: creating a `struct` to
//! hold the iterator's state, and then implementing [`Iterator`] for that `struct`.
//! This is why there are so many `struct`s in this module: there is one for
//! each iterator and iterator adapter.
//!
//! Let's make an iterator named `Counter` which counts from `1` to `5`:
//!
//! ```
//! // First, the struct:
//!
//! /// An iterator which counts from one to five
//! #[derive(Drop)]
//! struct Counter {
//!     count: usize,
//! }
//!
//! // we want our count to start at one, so let's add a new() method to help.
//! // This isn't strictly necessary, but is convenient. Note that we start
//! // `count` at zero, we'll see why in `next()`'s implementation below.
//! #[generate_trait]
//! impl CounterImpl of CounterTrait {
//!     fn new() -> Counter {
//!         Counter { count: 0 }
//!     }
//! }
//!
//! // Then, we implement `Iterator` for our `Counter`:
//!
//! impl CounterIter of core::iter::Iterator<Counter> {
//!     // we will be counting with usize
//!     type Item = usize;
//!
//!     // next() is the only required method
//!     fn next(ref self: Counter) -> Option<Self::Item> {
//!         // Increment our count. This is why we started at zero.
//!         self.count += 1;
//!
//!         // Check to see if we've finished counting or not.
//!         if self.count < 6 {
//!             Some(self.count)
//!         } else {
//!             None
//!         }
//!     }
//! }
//!
//! // And now we can use it!
//!
//! let mut counter = CounterTrait::new();
//!
//! assert!(counter.next() == Some(1));
//! assert!(counter.next() == Some(2));
//! assert!(counter.next() == Some(3));
//! assert!(counter.next() == Some(4));
//! assert!(counter.next() == Some(5));
//! assert!(counter.next() == None);
//! ```
//!
//! Calling [`next`] this way gets repetitive. Cairo has a construct which can
//! call [`next`] on your iterator, until it reaches `None`. Let's go over that
//! next.
//!
//! # `for` loops and `IntoIterator`
//!
//! Cairo's `for` loop syntax is actually sugar for iterators. Here's a basic
//! example of `for`:
//!
//! ```
//! let values = array![1, 2, 3, 4, 5];
//!
//! for x in values {
//!     println!("{x}");
//! }
//! ```
//!
//! This will print the numbers one through five, each on their own line. But
//! you'll notice something here: we never called anything on our array to
//! produce an iterator. What gives?
//!
//! There's a trait in the core library for converting something into an
//! iterator: [`IntoIterator`]. This trait has one method, [`into_iter`],
//! which converts the thing implementing [`IntoIterator`] into an iterator.
//! Let's take a look at that `for` loop again, and what the compiler converts
//! it into:
//!
//! [`into_iter`]: core::iter::IntoIterator::into_iter
//!
//! ```
//! let values = array![1, 2, 3, 4, 5];
//!
//! for x in values {
//!     println!("{x}");
//! }
//! ```
//!
//! Cairo de-sugars this into:
//!
//! ```
//! let values = array![1, 2, 3, 4, 5];
//! {
//!     let mut iter = IntoIterator::into_iter(values);
//!     let result = loop {
//!             let mut next = 0;
//!             match iter.next() {
//!                 Some(val) => next = val,
//!                 None => {
//!                     break;
//!                 },
//!             };
//!             let x = next;
//!             let () = { println!("{x}"); };
//!         };
//!     result
//! }
//! ```
//!
//! First, we call `into_iter()` on the value. Then, we match on the iterator
//! that returns, calling [`next`] over and over until we see a `None`. At
//! that point, we `break` out of the loop, and we're done iterating.
//!
//! There's one more subtle bit here: the core library contains an
//! interesting implementation of [`IntoIterator`]:
//!
//! ```ignore (only-for-syntax-highlight)
//! impl IteratorIntoIterator<T, +Iterator<T>> of IntoIterator<T>
//! ```
//!
//! In other words, all [`Iterator`]s implement [`IntoIterator`], by just
//! returning themselves. This means two things:
//!
//! 1. If you're writing an [`Iterator`], you can use it with a `for` loop.
//! 2. If you're creating a collection, implementing [`IntoIterator`] for it
//!    will allow your collection to be used with the `for` loop.
//!
//! # Adapters
//!
//! Functions which take an [`Iterator`] and return another [`Iterator`] are
//! often called 'iterator adapters', as they're a form of the 'adapter
//! pattern'.
//!
//! Common iterators adapters include [`map`], [`enumerate`] and [`zip`].
//!
//! [`map`]: Iterator::map
//! [`enumerate`]: Iterator::enumerate
//! [`zip`]: Iterator::zip
//!
//! # Laziness
//!
//! Iterators (and iterator [adapters](#adapters)) are *lazy*. This means that
//! just creating an iterator doesn't _do_ a whole lot. Nothing really happens
//! until you call [`next`]. This is sometimes a source of confusion when
//! creating an iterator solely for its side effects. For example, the [`map`]
//! method calls a closure on each element it iterates over:
//!
//! ```
//! let v = array![1, 2, 3, 4, 5];
//! let _ = v.into_iter().map(|x| println!("{x}"));
//! ```
//!
//! This will not print any values, as we only created an iterator, rather than
//! using it. The compiler will warn us about this kind of behavior:
//!
//! ```text
//! Unhandled `#[must_use]` type
//! ```
//!
//! [`map`]: Iterator::map
mod adapters;
mod traits;
pub use adapters::PeekableTrait;
pub use traits::{Extend, FromIterator, IntoIterator, Iterator};
