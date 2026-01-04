#[starknet::contract]
pub mod Fulfillment {
    use core::panics::panic_with_byte_array;
    use fulfillment::errors::{CALLER_NOT_OWNER, fulfillment_exceeded_err};
    use fulfillment::interface::{IFulfillment, UpdateFulfillment};
    use starknet::storage::{
        Map, StoragePathEntry, StoragePointerReadAccess, StoragePointerWriteAccess,
    };
    use starknet::{ContractAddress, get_caller_address};
    use starkware_utils::signature::stark::HashType;
    use starkware_utils::time::time::Timestamp;

    #[storage]
    pub struct Storage {
        pub owner: ContractAddress,
        pub expiration_time: Timestamp,
        pub fulfillment: Map<HashType, u64>,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {}
    /// todo: add roles and access control
    /// and other sfuff for a contract

    #[constructor]
    pub fn constructor(
        ref self: ContractState, owner: ContractAddress, expiration_time: Timestamp,
    ) {
        self.expiration_time.write(expiration_time);
        self.owner.write(owner);
    }

    #[abi(embed_v0)]
    pub impl Impl of IFulfillment<ContractState> {
        fn update_fulfillment(ref self: ContractState, updates: Span<UpdateFulfillment>) {
            assert(get_caller_address() == self.owner.read(), CALLER_NOT_OWNER);
            for update in updates {
                let entry = self.fulfillment.entry(*update.key);
                let new_value = entry.read() + *update.diff;

                if (new_value > *update.limit) {
                    let err = fulfillment_exceeded_err(id: *update.id);
                    panic_with_byte_array(err: @err);
                }
                entry.write(new_value);
            }
        }

        fn get_owner(self: @ContractState) -> ContractAddress {
            self.owner.read()
        }

        fn get_expiration_time(self: @ContractState) -> Timestamp {
            self.expiration_time.read()
        }

        fn get_fulfillment(self: @ContractState, key: HashType) -> u64 {
            self.fulfillment.entry(key).read()
        }
    }
}
