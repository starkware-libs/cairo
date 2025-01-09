#[derive(Copy, Drop, starknet::Store)]
struct Node {
    left: u128,
    right: u128,
}

#[starknet::interface]
trait ITree<TContractState> {
    fn sorted_list(ref self: TContractState, root: u128);
}

#[starknet::contract]
mod example_failure {
    use starknet::storage::Map;
    use super::{ITree, Node};

    #[storage]
    struct Storage {
        nodes: Map<u128, Node>,
    }

    #[generate_trait]
    impl Impl of MyTrait {
        fn append_in_order_nodes(self: @ContractState, ref list: Array<(u128, Node)>, at: u128) {
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

    #[abi(embed_v0)]
    impl Tree of ITree<ContractState> {
        fn sorted_list(ref self: ContractState, root: u128) {
            let mut in_order: Array<(u128, Node)> = array![];
            self.append_in_order_nodes(ref in_order, root);
        }
    }
}
