use starknet::ContractAddress;
use starknet::storage::Map;

#[starknet::interface]
trait IERC20<TContractState> {
    fn get_name(self: @TContractState) -> felt252;
    fn get_symbol(self: @TContractState) -> felt252;
    fn get_decimals(self: @TContractState) -> u8;
    fn get_total_supply(self: @TContractState) -> u256;
    fn balance_of(self: @TContractState, account: ContractAddress) -> u256;
    fn allowance(self: @TContractState, owner: ContractAddress, spender: ContractAddress) -> u256;
    fn increase_allowance(ref self: TContractState, spender: ContractAddress, added_value: u256);
    fn decrease_allowance(
        ref self: TContractState, spender: ContractAddress, subtracted_value: u256,
    );
    fn write_info(ref self: TContractState, user_info: UserInfo);
    fn get_info(self: @TContractState) -> UserInfo;
}

#[derive(starknet::Store, Serde, Drop, Copy)]
struct UserInfo {
    name: felt252,
    symbol: felt252,
    decimals: u8,
    total_supply: u256,
}

#[starknet::storage_node]
struct TransactionInfo {
    balances: Map<ContractAddress, u256>,
    allowances: Map<ContractAddress, Map<ContractAddress, u256>>,
}

#[starknet::contract]
mod storage_accesses {
    use core::num::traits::Zero;
    use starknet::storage::{
        StorageMapReadAccess, StorageMapWriteAccess, StoragePathEntry, StoragePointerReadAccess,
        StoragePointerWriteAccess,
    };
    use starknet::{ContractAddress, get_caller_address};
    use super::{TransactionInfo, UserInfo};

    #[storage]
    struct Storage {
        user_info: UserInfo,
        transaction_info: TransactionInfo,
    }

    #[constructor]
    fn constructor(
        ref self: ContractState,
        name_: felt252,
        symbol_: felt252,
        decimals_: u8,
        initial_supply: u256,
        recipient: ContractAddress,
    ) {
        self.user_info.name.write(name_);
        self.user_info.symbol.write(symbol_);
        self.user_info.decimals.write(decimals_);
        assert(!recipient.is_zero(), 'ERC20: mint to the 0 address');
        self.user_info.total_supply.write(initial_supply);
        self.transaction_info.balances.write(recipient, initial_supply);
    }

    #[abi(embed_v0)]
    impl IERC20Impl of super::IERC20<ContractState> {
        fn get_name(self: @ContractState) -> felt252 {
            self.user_info.name.read()
        }

        fn get_symbol(self: @ContractState) -> felt252 {
            self.user_info.symbol.read()
        }

        fn get_decimals(self: @ContractState) -> u8 {
            self.user_info.decimals.read()
        }

        fn get_total_supply(self: @ContractState) -> u256 {
            self.user_info.total_supply.read()
        }

        fn balance_of(self: @ContractState, account: ContractAddress) -> u256 {
            self.transaction_info.balances.read(account)
        }

        fn allowance(
            self: @ContractState, owner: ContractAddress, spender: ContractAddress,
        ) -> u256 {
            self.transaction_info.allowances.entry(owner).read(spender)
        }

        fn increase_allowance(
            ref self: ContractState, spender: ContractAddress, added_value: u256,
        ) {
            let caller = get_caller_address();
            self
                .set_transaction_info(
                    caller,
                    spender,
                    self.transaction_info.allowances.entry(caller).read(spender) + added_value,
                );
        }

        fn decrease_allowance(
            ref self: ContractState, spender: ContractAddress, subtracted_value: u256,
        ) {
            let caller = get_caller_address();
            self
                .set_transaction_info(
                    caller,
                    spender,
                    self.transaction_info.allowances.entry(caller).read(spender) - subtracted_value,
                );
        }

        fn write_info(ref self: ContractState, user_info: UserInfo) {
            self.user_info.write(user_info);
        }

        fn get_info(self: @ContractState) -> UserInfo {
            self.user_info.read()
        }
    }

    #[generate_trait]
    impl StorageImpl of StorageTrait {
        fn transfer_helper(
            ref self: ContractState,
            sender: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) {
            self
                .transaction_info
                .balances
                .write(sender, self.transaction_info.balances.read(sender) - amount);
            self
                .transaction_info
                .balances
                .write(recipient, self.transaction_info.balances.read(recipient) + amount);
        }

        fn set_transaction_info(
            ref self: ContractState, owner: ContractAddress, spender: ContractAddress, amount: u256,
        ) {
            self.transaction_info.allowances.entry(owner).entry(spender).write(amount);
        }
    }
}
