use super::{StoragePath, Mutable};

/// A trait that binds a storage path of a struct, and the struct storage node (a storage node is a
/// struct that all its fields are storage paths, one for each member of the original struct).
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
