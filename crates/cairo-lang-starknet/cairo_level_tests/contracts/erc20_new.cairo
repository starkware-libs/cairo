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
    fn transfer(ref self: TContractState, recipient: ContractAddress, amount: u256);
    fn transfer_from(
        ref self: TContractState, sender: ContractAddress, recipient: ContractAddress, amount: u256
    );
    fn approve(ref self: TContractState, spender: ContractAddress, amount: u256);
    fn increase_allowance(ref self: TContractState, spender: ContractAddress, added_value: u256);
    fn decrease_allowance(
        ref self: TContractState, spender: ContractAddress, subtracted_value: u256
    );
    fn write_info(ref self: TContractState, user_info: UserInfo);
    fn get_info(self: @TContractState) -> UserInfo;
}

#[derive(starknet::Store, Serde, Drop, Copy)]
#[starknet::sub_pointers]
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

#[starknet::contract]
mod erc_20_new {
    use core::num::traits::Zero;
    use starknet::get_caller_address;
    use starknet::contract_address_const;
    use starknet::ContractAddress;
    use super::{UserInfo, TransactionInfo};
    use starknet::storage::StoragePathEntry;

    #[storage]
    struct Storage {
        user_info: UserInfo,
        transaction_info: TransactionInfo,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        Transfer: Transfer,
        Approval: Approval,
    }
    #[derive(Drop, starknet::Event)]
    struct Transfer {
        from: ContractAddress,
        to: ContractAddress,
        value: u256,
    }
    #[derive(Drop, starknet::Event)]
    struct Approval {
        owner: ContractAddress,
        spender: ContractAddress,
        value: u256,
    }

    #[constructor]
    fn constructor(
        ref self: ContractState,
        name_: felt252,
        symbol_: felt252,
        decimals_: u8,
        initial_supply: u256,
        recipient: ContractAddress
    ) {
        self.user_info.name.write(name_);
        self.user_info.symbol.write(symbol_);
        self.user_info.decimals.write(decimals_);
        assert(!recipient.is_zero(), 'ERC20: mint to the 0 address');
        self.user_info.total_supply.write(initial_supply);
        self.transaction_info.balances.write(recipient, initial_supply);
        self
            .emit(
                Event::Transfer(
                    Transfer {
                        from: contract_address_const::<0>(), to: recipient, value: initial_supply
                    }
                )
            );
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
            self: @ContractState, owner: ContractAddress, spender: ContractAddress
        ) -> u256 {
            self.transaction_info.allowances.entry(owner).read(spender)
        }

        fn transfer(ref self: ContractState, recipient: ContractAddress, amount: u256) {
            let sender = get_caller_address();
            self.transfer_helper(sender, recipient, amount);
        }

        fn transfer_from(
            ref self: ContractState,
            sender: ContractAddress,
            recipient: ContractAddress,
            amount: u256
        ) {
            let caller = get_caller_address();
            self.spend_allowance(sender, caller, amount);
            self.transfer_helper(sender, recipient, amount);
        }

        fn approve(ref self: ContractState, spender: ContractAddress, amount: u256) {
            let caller = get_caller_address();
            self.approve_helper(caller, spender, amount);
        }

        fn increase_allowance(
            ref self: ContractState, spender: ContractAddress, added_value: u256
        ) {
            let caller = get_caller_address();
            self
                .approve_helper(
                    caller, 
                    spender, 
                    self.transaction_info.allowances.entry(caller).read(spender) + added_value
                );
        }

        fn decrease_allowance(
            ref self: ContractState, spender: ContractAddress, subtracted_value: u256
        ) {
            let caller = get_caller_address();
            self
                .approve_helper(
                    caller, 
                    spender, 
                    self.transaction_info.allowances.entry(caller).read(spender) - subtracted_value
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
            amount: u256
        ) {
            assert(!sender.is_zero(), 'ERC20: transfer from 0');
            assert(!recipient.is_zero(), 'ERC20: transfer to 0');
            self
                .transaction_info
                .balances
                .write(sender, self.transaction_info.balances.read(sender) - amount);
            self
                .transaction_info
                .balances
                .write(recipient, self.transaction_info.balances.read(recipient) + amount);
            self.emit(Transfer { from: sender, to: recipient, value: amount });
        }

        fn spend_allowance(
            ref self: ContractState, owner: ContractAddress, spender: ContractAddress, amount: u256
        ) {
            let current_allowance = self.transaction_info.allowances.entry(owner).read(spender);
            let ONES_MASK = 0xffffffffffffffffffffffffffffffff_u128;
            let is_unlimited_allowance = current_allowance.low == ONES_MASK
                && current_allowance.high == ONES_MASK;
            if !is_unlimited_allowance {
                self.approve_helper(owner, spender, current_allowance - amount);
            }
        }

        fn approve_helper(
            ref self: ContractState, owner: ContractAddress, spender: ContractAddress, amount: u256
        ) {
            assert(!spender.is_zero(), 'ERC20: approve from 0');
            self.transaction_info.allowances.entry(owner).entry(spender).write(amount);
            self.emit(Approval { owner, spender, value: amount });
        }
    }
}
