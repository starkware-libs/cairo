#[derive(Copy, Drop, storage_access::StorageAccess)]
struct Node {
    left: u128,
    right: u128
}

#[starknet::interface]
trait ITree<TStorage> {
    fn sorted_list(ref self: TStorage, root: u128);
}

#[starknet::contract]
mod ExampleFailure {
    use super::Node;
    use super::ITree;
    use array::{ArrayTrait, Array};

    #[storage]
    struct Storage {
        nodes: LegacyMap::<u128, Node>, 
    }

    #[generate_trait]
    impl Impl of MyTrait {
        fn append_in_order_nodes(self: @Storage, ref list: Array<(u128, Node)>, at: u128) {
            let node = self.nodes.read(at);
            if (node.left != 0) {
                self.append_in_order_nodes(ref list, node.left);
            }
            list.append((at, node));
            if (node.right != 0) {
                self.append_in_order_nodes(ref list, node.right);
            }
        }
    }

    #[external(v0)]
    impl Tree of ITree<Storage> {
        fn sorted_list(ref self: Storage, root: u128) {
            let mut in_order: Array<(u128, Node)> = ArrayTrait::new();
            self.append_in_order_nodes(ref in_order, root);
        }
    }
}
