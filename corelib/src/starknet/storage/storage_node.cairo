use super::{StoragePath, Mutable};


/// A trait that given a storage path of a struct, generates the storage node of this struct.
///
/// The storage node is a struct that is used to structure the storage of a struct, while taking
/// into account this structure when computing the address of the struct members in the storage.
/// The trigger for creating a storage node is the `#[starknet::storage_node]` attribute.
///
/// Storage nodes are used in order to structure the storage of a struct, while not enforcing the
/// struct to be sequential in the storage. This is useful for structs that contains phantom types
/// such as `Map` and `Vec`. As a result, structs attributed with `#[starknet::storage_node]` are
/// also considered to be phantom types, although not explicitly annotated as such.
/// Structs which do not contain phantom types, can still be declared a storage node, and it will
/// make them a phantom type.
/// However, it may be preferable to simply make this struct storable (i.e. `#[derive(Store)]')
/// instead. This will still allow accessing individual members of the struct (see `SubPointers`),
/// but will not make the struct a phantom type.
///
/// For example, given the following struct:
/// ```
/// #[starknet::storage_node]
/// struct MyStruct {
///    a: felt252,
///    b: Map<felt252, felt52>,
/// }
/// ```
///
/// The following storage node struct and impl will be generated:
/// ```
/// struct MyStructStorageNode {
///     a: PendingStoragePath<felt252>,
///     b: PendingStoragePath<Map<felt252, felt52>>,
/// }
///
/// impl MyStructStorageNodeImpl of StorageNode<MyStruct> {
///    fn storage_node(self: StoragePath<MyStruct>) -> MyStructStorageNode {
///         MyStructStorageNode {
///            a: PendingStoragePathTrait::new(@self, selector!("a")),
///            b: PendingStoragePathTrait::new(@self, selector!("b")),
///         }
///    }
/// }
/// ```
/// For a type `T` that implement `StorageNode` (e.g. `MyStruct` in the example above),
/// `Deref<StoragePath<T>>` is implemented as simply calling `storage_node`, and thus exposing the
/// members of the storage node (`a` and `b` in the example above).
/// For example, given the following storage:
/// ```
/// #[storage]
/// struct Storage {
///     my_struct: MyStruct,
///     a: felt52,
/// }
///
/// We can access the members of the storage node as follows:
/// ```
/// fn use_storage(self: @ContractState) {
///    let a_value = self.a.read();
///    let inner_a_value = self.my_struct.a.read();
///    let b_value = self.my_struct.b.entry(42).read();
/// }
/// ```
///
/// If a member is annotated with `#[flat]`, the storage node will be flattened, and the
/// member name (i.e. `my_struct`) will not affect the address of the storage object.
/// In the storage example above, it will look like:
/// ```
/// #[storage]
/// struct Storage {
///    #[flat]
///    my_struct: MyStruct,
///    a: felt52,
/// }
///
/// In this case, the storage node will be flattened, and both `self.a` and `self.my_struct.a` will
/// point to the same address. This behavior is rarely intended, and thus `#[flat]` should be used
/// with caution.
///
/// Notice that the members of the storage node are `PendingStoragePath` instances, which are used
/// to lazily get the updated storage path of the struct members, in this way only members that are
/// accessed are actually evaluated.
///
pub trait StorageNode<T> {
    type NodeType;
    fn storage_node(self: StoragePath<T>) -> Self::NodeType;
}

/// This makes the storage node members directly accessible from a path to the parent struct.
pub impl StorageNodeDeref<T, +StorageNode<T>> of core::ops::Deref<StoragePath<T>> {
    type Target = StorageNode::<T>::NodeType;
    fn deref(self: StoragePath<T>) -> Self::Target {
        self.storage_node()
    }
}

/// A mutable version of `StorageNode`, works the same way, but on `Mutable<T>`.
pub trait StorageNodeMut<T> {
    type NodeType;
    fn storage_node_mut(self: StoragePath<Mutable<T>>) -> Self::NodeType;
}

/// This makes the storage node members directly accessible from a path to the parent struct.
pub impl StorageNodeMutDeref<T, +StorageNodeMut<T>> of core::ops::Deref<StoragePath<Mutable<T>>> {
    type Target = StorageNodeMut::<T>::NodeType;
    fn deref(self: StoragePath<Mutable<T>>) -> Self::Target {
        self.storage_node_mut()
    }
}
