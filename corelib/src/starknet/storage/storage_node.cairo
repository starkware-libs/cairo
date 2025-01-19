//! Storage nodes provide a way to structure contract storage data, reflecting their structure in
//! the storage address computation of their members. They are special structs that can contain any
//! storable type and are marked with the `#[starknet::storage_node]` attribute.
//!
//! # Purpose and Benefits
//!
//! Storage nodes provide a flexible way to structure storage data by allowing non-sequential
//! storage layouts. They allow the creation of storage-only types, that can contain both
//! storage-specific types (like `Map` and `Vec`) and regular types - so long as these types are
//! storable.
//!
//! Storage nodes are particularly valuable when defining structs containing phantom types like
//! `Map` and `Vec`. When a struct is marked with `#[starknet::storage_node]`, it automatically
//! becomes a phantom type.
//!
//! While you can declare any struct as a storage node (even those without phantom types), doing so
//! will make that struct a phantom type. For structs that don't contain phantom types, it's often
//! more appropriate to make them storable using `#[derive(Store)]`. This alternative approach
//! still enables access to individual struct members through `SubPointers` without imposing the
//! phantom type behavior.
//!
//! The storage layout differs significantly between these two approaches:
//! * `#[derive(Store)]`: Members are stored continuously in the same variable space, with a limit
//!   of 256 field elements.
//! * Storage node: Each member is stored at a different location. For a storage node member `m`
//!   within a storage variable `variable_name`, the path to that member is computed as
//!   `h(sn_keccak(variable_name), sn_keccak(m))`, where `h` is the Pedersen hash.
//!
//! # Examples
//!
//! Here's how to define a storage node:
//!
//! ```
//! #[starknet::storage_node]
//! struct MyStruct {
//!    a: felt252,
//!    b: Map<felt252, felt52>,
//! }
//! ```
//!
//! For the struct above, the following storage node struct and impl will be generated:
//!
//! ```
//! struct MyStructStorageNode {
//!     a: PendingStoragePath<felt252>,
//!     b: PendingStoragePath<Map<felt252, felt52>>,
//! }
//!
//! impl MyStructStorageNodeImpl of StorageNode<MyStruct> {
//!    fn storage_node(self: StoragePath<MyStruct>) -> MyStructStorageNode {
//!         MyStructStorageNode {
//!            a: PendingStoragePathTrait::new(@self, selector!("a")),
//!            b: PendingStoragePathTrait::new(@self, selector!("b")),
//!         }
//!    }
//! }
//! ```
//!
//! For a type `T` that implements `StorageNode` (e.g. `MyStruct` in the example above),
//! `Deref<StoragePath<T>>` is implemented as simply calling `storage_node`, and thus exposing the
//! members of the storage node (`a` and `b` in the example above).
//! For example, given the following storage:
//!
//! ```
//! #[storage]
//! struct Storage {
//!     my_struct: MyStruct,
//!     a: felt52,
//! }
//!
//! We can access the members of the storage node as follows:
//!
//! ```
//! fn use_storage(self: @ContractState) {
//!    let a_value = self.a.read();
//!    let inner_a_value = self.my_struct.a.read();
//!    let b_value = self.my_struct.b.entry(42).read();
//! }
//! ```
//!
//! # Flattening Storage Nodes
//!
//! Storage Nodes members can be annotated with `#[flat]` to flatten the storage hierarchy and not
//! use the member name in the computation of the storage address for its fields.
//!
//! ```
//! #[storage]
//! struct Storage {
//!    #[flat]
//!    my_struct: MyStruct,
//!    a: felt52,
//! }
//! ```
//!
//! When flattened, the storage node's field name (e.g., `my_struct`) doesn't affect storage address
//! computation. In the example above, both `self.a` and `self.my_struct.a` will point to the same
//! address. Use `#[flat]` with caution as this behavior is rarely intended.
//!
//! # Performance Considerations
//!
//! Storage node members are implemented as `PendingStoragePath` instances, enabling lazy evaluation
//! of storage paths. This means storage addresses are only computed for members that are actually
//! accessed.

use super::{Mutable, StoragePath};

/// A trait that given a storage path of a struct, generates the storage node of this struct.
pub trait StorageNode<T> {
    type NodeType;
    fn storage_node(self: StoragePath<T>) -> Self::NodeType;
}

/// A mutable version of `StorageNode`, works the same way, but on `Mutable<T>`.
pub trait StorageNodeMut<T> {
    type NodeType;
    fn storage_node_mut(self: StoragePath<Mutable<T>>) -> Self::NodeType;
}
