//! Indexing traits for indexing operations on collections.
//!
//! This module provides traits for implementing the indexing operator `[]`, offering two distinct
//! approaches to access elements in collections:
//!
//! * [`IndexView`] - For snapshot-based access
//! * [`Index`] - For reference-based access
//!
//! # When to use which trait
//!
//! - Use [`IndexView`] when the collection can be accessed in a read-only context and is not
//! mutated by a read access. This is the most common case in Cairo.
//! - Use [`Index`] when the input type needs to be passed as `ref`. This is mainly useful for types
//!   depending on a [`Felt252Dict`], where dictionary accesses are modifying the data structure
//!   itself.
//!
//! Only one of these traits should be implemented for any given type, not both.
//!
//! [`Felt252Dict`]: core::dict::Felt252Dict

#[feature("deprecated-index-traits")]
use crate::traits::{Index as DeprecatedIndex, IndexView as DeprecatedIndexView};

/// A trait for indexing operations (`container[index]`) where the input type is not modified.
///
/// `container[index]` is syntactic sugar for `container.index(index)`.
///
/// # Examples
///
/// The following example implements `IndexView` on a `NucleotideCount` container, which can be
/// indexed without modifying the input, enabling individual counts to be retrieved with index
/// syntax.
///
/// ```
/// use core::ops::IndexView;
///
/// #[derive(Copy, Drop)]
/// enum Nucleotide {
///      A,
///      C,
///      G,
///      T,
///  }
///
/// #[derive(Copy, Drop)]
/// struct NucleotideCount {
///      a: usize,
///      c: usize,
///      g: usize,
///      t: usize,
///  }
///
/// impl NucleotideIndex of IndexView<NucleotideCount, Nucleotide> {
///      type Target = usize;
///
///      fn index(self: @NucleotideCount, index: Nucleotide) -> Self::Target {
///          match index {
///              Nucleotide::A => *self.a,
///              Nucleotide::C => *self.c,
///              Nucleotide::G => *self.g,
///              Nucleotide::T => *self.t,
///          }
///      }
///  }
///
/// let nucleotide_count = NucleotideCount {a: 14, c: 9, g: 10, t: 12};
/// assert!(nucleotide_count[Nucleotide::A] == 14);
/// assert!(nucleotide_count[Nucleotide::C] == 9);
/// assert!(nucleotide_count[Nucleotide::G] == 10);
/// assert!(nucleotide_count[Nucleotide::T] == 12);
/// ```
pub trait IndexView<C, I> {
    /// The returned type after indexing.
    type Target;

    /// Performs the indexing (`container[index]`) operation.
    ///
    /// # Panics
    ///
    /// May panic if the index is out of bounds.
    fn index(self: @C, index: I) -> Self::Target;
}

impl DeprecatedIndexViewImpl<
    C, I, V, impl Deprecated: DeprecatedIndexView<C, I, V>,
> of crate::ops::IndexView<C, I> {
    type Target = V;
    fn index(self: @C, index: I) -> Self::Target {
        Deprecated::index(self, index)
    }
}

/// A trait for indexing operations (`container[index]`) where the input type is mutated.
///
/// This trait should be implemented when you want to implement indexing operations on a type that's
/// mutated by a read access. This is useful for any type depending on a [`Felt252Dict`], where
/// dictionary accesses are modifying the data structure itself.
///
/// `container[index]` is syntactic sugar for `container.index(index)`.
///
/// # Examples
///
/// The following example implements `Index` on a `Stack` type. This `Stack` is implemented based on
/// a [`Felt252Dict`], where dictionary accesses are modifying the dictionary itself. As such, we
/// must implement the `Index` trait instead of the `IndexView` trait.
///
/// [`Felt252Dict`]: core::dict::Felt252Dict
///
/// ```
/// use core::ops::Index;
///
/// #[derive(Destruct, Default)]
/// struct Stack {
///     items: Felt252Dict<u128>,
///     len: usize
/// }
///
/// #[generate_trait]
/// impl StackImpl of StackTrait {
///     fn push(ref self: Stack, item: u128) {
///         self.items.insert(self.len.into(), item);
///         self.len += 1;
///     }
/// }
///
/// impl StackIndex of Index<Stack, usize> {
///      type Target = u128;
///
///      fn index(ref self: Stack, index: usize) -> Self::Target {
///          if index >= self.len {
///              panic!("Index out of bounds");
///          }
///          self.items.get(index.into())
///      }
///  }
///
/// let mut stack: Stack = Default::default();
/// stack.push(1);
/// assert!(stack[0] == 1);
/// ```
pub trait Index<C, I> {
    /// The returned type after indexing.
    type Target;

    /// Performs the indexing (`container[index]`) operation.
    ///
    /// # Panics
    ///
    /// May panic if the index is out of bounds.
    fn index(ref self: C, index: I) -> Self::Target;
}

#[feature("deprecated-index-traits")]
impl DeprecatedIndexImpl<
    C, I, V, impl Deprecated: DeprecatedIndex<C, I, V>,
> of crate::ops::Index<C, I> {
    type Target = V;
    fn index(ref self: C, index: I) -> Self::Target {
        Deprecated::index(ref self, index)
    }
}
