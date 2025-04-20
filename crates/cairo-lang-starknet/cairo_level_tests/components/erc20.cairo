use starknet::ContractAddress;

#[starknet::interface]
pub trait ERC20Trait<TCS> {
    fn get_name(self: @TCS) -> felt252;
    fn get_symbol(self: @TCS) -> felt252;
    fn get_decimals(self: @TCS) -> u8;
    fn get_total_supply(self: @TCS) -> u256;
    fn balance_of(self: @TCS, account: ContractAddress) -> u256;
    fn allowance(self: @TCS, owner: ContractAddress, spender: ContractAddress) -> u256;
    fn transfer(ref self: TCS, recipient: ContractAddress, amount: u256);
    fn transfer_from(
        ref self: TCS, sender: ContractAddress, recipient: ContractAddress, amount: u256,
    );
    fn approve(ref self: TCS, spender: ContractAddress, amount: u256);
    fn increase_allowance(ref self: TCS, spender: ContractAddress, added_value: u256);
    fn decrease_allowance(ref self: TCS, spender: ContractAddress, subtracted_value: u256);
}

#[starknet::component]
pub mod erc20 {
    use core::num::traits::Zero;
    use starknet::storage::{
        Map, StorageMapReadAccess, StorageMapWriteAccess, StoragePointerReadAccess,
        StoragePointerWriteAccess,
    };
    use starknet::{ContractAddress, get_caller_address};
    #[storage]
    pub struct Storage {
        pub name: felt252,
        pub symbol: felt252,
        pub decimals: u8,
        pub total_supply: u256,
        pub balances: Map<ContractAddress, u256>,
        pub allowances: Map<(ContractAddress, ContractAddress), u256>,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    pub enum Event {
        Transfer: TransferEvent,
        Approval: ApprovalEvent,
    }
    #[derive(Drop, starknet::Event)]
    pub struct TransferEvent {
        pub from: ContractAddress,
        pub to: ContractAddress,
        pub value: u256,
    }
    #[derive(Drop, starknet::Event)]
    pub struct ApprovalEvent {
        pub owner: ContractAddress,
        pub spender: ContractAddress,
        pub value: u256,
    }

    #[embeddable_as(IERC20)]
    pub impl ERC20Impl<
        TContractState, +HasComponent<TContractState>,
    > of super::ERC20Trait<ComponentState<TContractState>> {
        fn get_name(self: @ComponentState<TContractState>) -> felt252 {
            self.name.read()
        }

        fn get_symbol(self: @ComponentState<TContractState>) -> felt252 {
            self.symbol.read()
        }

        fn get_decimals(self: @ComponentState<TContractState>) -> u8 {
            self.decimals.read()
        }

        fn get_total_supply(self: @ComponentState<TContractState>) -> u256 {
            self.total_supply.read()
        }

        fn balance_of(self: @ComponentState<TContractState>, account: ContractAddress) -> u256 {
            self.balances.read(account)
        }

        fn allowance(
            self: @ComponentState<TContractState>, owner: ContractAddress, spender: ContractAddress,
        ) -> u256 {
            self.allowances.read((owner, spender))
        }

        fn transfer(
            ref self: ComponentState<TContractState>, recipient: ContractAddress, amount: u256,
        ) {
            let sender = get_caller_address();
            self.transfer_helper(sender, recipient, amount);
        }

        fn transfer_from(
            ref self: ComponentState<TContractState>,
            sender: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) {
            let caller = get_caller_address();
            self.spend_allowance(sender, caller, amount);
            self.transfer_helper(sender, recipient, amount);
        }

        fn approve(
            ref self: ComponentState<TContractState>, spender: ContractAddress, amount: u256,
        ) {
            let caller = get_caller_address();
            self.approve_helper(caller, spender, amount);
        }

        fn increase_allowance(
            ref self: ComponentState<TContractState>, spender: ContractAddress, added_value: u256,
        ) {
            let caller = get_caller_address();
            self
                .approve_helper(
                    caller, spender, self.allowances.read((caller, spender)) + added_value,
                );
        }

        fn decrease_allowance(
            ref self: ComponentState<TContractState>,
            spender: ContractAddress,
            subtracted_value: u256,
        ) {
            let caller = get_caller_address();
            self
                .approve_helper(
                    caller, spender, self.allowances.read((caller, spender)) - subtracted_value,
                );
        }
    }

    #[generate_trait]
    pub impl ERC20HelperImpl<
        TContractState, impl X: HasComponent<TContractState>,
    > of ERC20HelperTrait<TContractState, X> {
        fn transfer_helper(
            ref self: ComponentState<TContractState>,
            sender: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) {
            assert(!sender.is_zero(), 'ERC20: transfer from 0');
            assert(!recipient.is_zero(), 'ERC20: transfer to 0');
            self.balances.write(sender, self.balances.read(sender) - amount);
            self.balances.write(recipient, self.balances.read(recipient) + amount);
            self.emit(TransferEvent { from: sender, to: recipient, value: amount });
        }

        fn spend_allowance(
            ref self: ComponentState<TContractState>,
            owner: ContractAddress,
            spender: ContractAddress,
            amount: u256,
        ) {
            let current_allowance = self.allowances.read((owner, spender));
            let ONES_MASK = 0xffffffffffffffffffffffffffffffff_u128;
            let is_unlimited_allowance = current_allowance.low == ONES_MASK
                && current_allowance.high == ONES_MASK;
            if !is_unlimited_allowance {
                self.approve_helper(owner, spender, current_allowance - amount);
            }
        }

        fn approve_helper(
            ref self: ComponentState<TContractState>,
            owner: ContractAddress,
            spender: ContractAddress,
            amount: u256,
        ) {
            assert(!spender.is_zero(), 'ERC20: approve from 0');
            self.allowances.write((owner, spender), amount);
            self.emit(ApprovalEvent { owner, spender, value: amount });
        }
        fn init(
            ref self: ComponentState<TContractState>,
            name: felt252,
            symbol: felt252,
            decimals: u8,
            initial_supply: u256,
            recipient: ContractAddress,
        ) {
            self.name.write(name);
            self.symbol.write(symbol);
            self.decimals.write(decimals);
            assert(!recipient.is_zero(), 'ERC20: mint to the 0 address');
            self.total_supply.write(initial_supply);
            self.balances.write(recipient, initial_supply);
            self
                .emit(
                    Event::Transfer(
                        TransferEvent { from: Zero::zero(), to: recipient, value: initial_supply },
                    ),
                );
        }
    }
}
