use starknet::ContractAddress;
use starknet::storage::{StoragePathEntry, Map};

#[starknet::interface]
pub trait ERC20Trait<TCS> {
    fn get_name(self: @TCS) -> felt252;
    fn get_symbol(self: @TCS) -> felt252;
    fn get_decimals(self: @TCS) -> u8;
    fn get_total_supply(self: @TCS) -> u256;
    fn balance_of(self: @TCS, account: ContractAddress) -> u256;
    fn allowance(self: @TCS, owner: ContractAddress, spender: ContractAddress) -> u256;
    fn increase_allowance(ref self: TCS, spender: ContractAddress, added_value: u256);
    fn decrease_allowance(ref self: TCS, spender: ContractAddress, subtracted_value: u256);
    fn write_info(ref self: TCS, user_info: UserInfo);
    fn get_info(self: @TCS) -> UserInfo;
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
    balances: Map::<ContractAddress, u256>,
    allowances: Map::<ContractAddress, Map<ContractAddress, u256>>,
}

#[starknet::component]
pub mod storage_accesses {
    use core::num::traits::Zero;
    use starknet::get_caller_address;
    use starknet::ContractAddress;
    use super::{UserInfo, TransactionInfo};
    use starknet::storage::StoragePathEntry;

    #[storage]
    struct Storage {
        user_info: UserInfo,
        transaction_info: TransactionInfo,
    }

    #[embeddable_as(IERC20)]
    pub impl ERC20Impl<
        TContractState, +HasComponent<TContractState>
    > of super::ERC20Trait<ComponentState<TContractState>> {
        fn get_name(self: @ComponentState<TContractState>) -> felt252 {
            self.user_info.name.read()
        }

        fn get_symbol(self: @ComponentState<TContractState>) -> felt252 {
            self.user_info.symbol.read()
        }

        fn get_decimals(self: @ComponentState<TContractState>) -> u8 {
            self.user_info.decimals.read()
        }

        fn get_total_supply(self: @ComponentState<TContractState>) -> u256 {
            self.user_info.total_supply.read()
        }

        fn balance_of(self: @ComponentState<TContractState>, account: ContractAddress) -> u256 {
            self.transaction_info.balances.read(account)
        }

        fn allowance(
            self: @ComponentState<TContractState>, owner: ContractAddress, spender: ContractAddress
        ) -> u256 {
            self.transaction_info.allowances.entry(owner).read(spender)
        }

        fn increase_allowance(
            ref self: ComponentState<TContractState>, spender: ContractAddress, added_value: u256
        ) {
            let caller = get_caller_address();
            self
                .set_transaction_info(
                    caller,
                    spender,
                    self.transaction_info.allowances.entry(caller).read(spender) + added_value
                );
        }

        fn decrease_allowance(
            ref self: ComponentState<TContractState>,
            spender: ContractAddress,
            subtracted_value: u256
        ) {
            let caller = get_caller_address();
            self
                .set_transaction_info(
                    caller,
                    spender,
                    self.transaction_info.allowances.entry(caller).read(spender) - subtracted_value
                );
        }

        fn write_info(ref self: ComponentState<TContractState>, user_info: UserInfo) {
            self.user_info.write(user_info);
        }

        fn get_info(self: @ComponentState<TContractState>) -> UserInfo {
            self.user_info.read()
        }
    }

    #[generate_trait]
    pub impl ERC20HelperImpl<
        TContractState, impl X: HasComponent<TContractState>
    > of ERC20HelperTrait<TContractState, X> {
        fn transfer_helper(
            ref self: ComponentState<TContractState>,
            sender: ContractAddress,
            recipient: ContractAddress,
            amount: u256
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
            ref self: ComponentState<TContractState>,
            owner: ContractAddress,
            spender: ContractAddress,
            amount: u256
        ) {
            assert(!spender.is_zero(), 'ERC20: approve from 0');
            self.transaction_info.allowances.entry(owner).entry(spender).write(amount);
        }

        fn init(
            ref self: ComponentState<TContractState>,
            name: felt252,
            symbol: felt252,
            decimals: u8,
            initial_supply: u256,
            recipient: ContractAddress
        ) {
            self.user_info.name.write(name);
            self.user_info.symbol.write(symbol);
            self.user_info.decimals.write(decimals);
            assert(!recipient.is_zero(), 'ERC20: mint to the 0 address');
            self.user_info.total_supply.write(initial_supply);
            self.transaction_info.balances.write(recipient, initial_supply);
        }
    }
}
