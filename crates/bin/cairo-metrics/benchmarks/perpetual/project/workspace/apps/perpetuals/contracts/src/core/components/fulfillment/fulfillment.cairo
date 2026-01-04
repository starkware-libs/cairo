/// # Nonce Component
///
/// The Nonce component provides a simple mechanism for handling incremental nonce. It is commonly
/// used to prevent replay attacks when contracts accept signatures as input.
#[starknet::component]
pub mod Fulfillement {
    use core::panics::panic_with_byte_array;
    use perpetuals::core::components::fulfillment::interface::IFulfillment;
    use starknet::storage::{
        Map, StoragePathEntry, StoragePointerReadAccess, StoragePointerWriteAccess,
    };
    use starkware_utils::math::abs::Abs;
    use starkware_utils::signature::stark::HashType;
    use crate::core::errors::fulfillment_exceeded_err;

    #[storage]
    pub struct Storage {
        pub fulfillment: Map<HashType, u64>,
    }

    #[embeddable_as(FulfillmentImpl)]
    impl FulfillmentComponent<
        TContractState, +HasComponent<TContractState>, +Drop<TContractState>,
    > of IFulfillment<ComponentState<TContractState>> {
        fn update_fulfillment(
            ref self: ComponentState<TContractState>,
            position_id: crate::core::types::position::PositionId,
            hash: felt252,
            order_base_amount: i64,
            actual_base_amount: i64,
        ) {
            let mut fulfillment_entry = self.fulfillment.entry(hash);
            let total_amount = fulfillment_entry.read() + actual_base_amount.abs();

            if (total_amount > order_base_amount.abs()) {
                let err = fulfillment_exceeded_err(:position_id);
                panic_with_byte_array(err: @err);
            }
            fulfillment_entry.write(total_amount);
        }
        fn clean_fulfillment(ref self: ComponentState<TContractState>, hashes: Span<felt252>) {
            for hash in hashes {
                self.fulfillment.entry(*hash).write(Default::default());
            }
        }
    }
}
