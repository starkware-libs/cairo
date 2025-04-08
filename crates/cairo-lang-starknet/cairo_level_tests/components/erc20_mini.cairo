use starknet::storage::{
    StorageMapReadAccess, StorageMapWriteAccess, StoragePointerReadAccess,
    StoragePointerWriteAccess,
};
use starknet::{ContractAddress, get_caller_address};


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

/// A trait that, given a path to a storage, provides the storage node and mutable storage node.
pub trait HasStorage<
    TContractState,
    /// The storage type.
    Storage,
    /// The storage node.
    impl StorageImpl: starknet::storage::StorageTrait<Storage>,
    /// The mutable storage node.
    impl StorageImplMut: starknet::storage::StorageTraitMut<Storage>,
> {
    fn storage(self: @TContractState) -> StorageImpl::BaseType;
    fn storage_mut(ref self: TContractState) -> StorageImplMut::BaseType;
}
use core::num::traits::Zero;
use starknet::storage::Map;
#[starknet::storage_node]
pub struct ERC20Storage {
    pub name: felt252,
    pub symbol: felt252,
    pub decimals: u8,
    pub total_supply: u256,
    pub balances: Map<ContractAddress, u256>,
    pub allowances: Map<(ContractAddress, ContractAddress), u256>,
}

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

#[starknet::embeddable]
pub impl ERC20Impl<
    TContractState,
    ContractStateEvent,
    +HasStorage<TContractState, ERC20Storage>,
    +Drop<TContractState>,
    +starknet::event::EventEmitter<TContractState, ContractStateEvent>,
    +Into<Event, ContractStateEvent>,
> of ERC20Trait<TContractState> {
    fn get_name(self: @TContractState) -> felt252 {
        self.storage().name.read()
    }

    fn get_symbol(self: @TContractState) -> felt252 {
        self.storage().symbol.read()
    }

    fn get_decimals(self: @TContractState) -> u8 {
        self.storage().decimals.read()
    }

    fn get_total_supply(self: @TContractState) -> u256 {
        self.storage().total_supply.read()
    }

    fn balance_of(self: @TContractState, account: ContractAddress) -> u256 {
        self.storage().balances.read(account)
    }

    fn allowance(self: @TContractState, owner: ContractAddress, spender: ContractAddress) -> u256 {
        self.storage().allowances.read((owner, spender))
    }

    fn transfer(ref self: TContractState, recipient: ContractAddress, amount: u256) {
        let sender = get_caller_address();
        self.transfer_helper(sender, recipient, amount);
    }

    fn transfer_from(
        ref self: TContractState, sender: ContractAddress, recipient: ContractAddress, amount: u256,
    ) {
        let caller = get_caller_address();
        self.spend_allowance(sender, caller, amount);
        self.transfer_helper(sender, recipient, amount);
    }

    fn approve(ref self: TContractState, spender: ContractAddress, amount: u256) {
        let caller = get_caller_address();
        self.approve_helper(caller, spender, amount);
    }

    fn increase_allowance(ref self: TContractState, spender: ContractAddress, added_value: u256) {
        let caller = get_caller_address();
        self
            .approve_helper(
                caller, spender, self.storage().allowances.read((caller, spender)) + added_value,
            );
    }

    fn decrease_allowance(
        ref self: TContractState, spender: ContractAddress, subtracted_value: u256,
    ) {
        let caller = get_caller_address();
        self
            .approve_helper(
                caller,
                spender,
                self.storage().allowances.read((caller, spender)) - subtracted_value,
            );
    }
}

#[generate_trait]
pub impl ERC20HelperImpl<
    TContractState,
    ContractStateEvent,
    +HasStorage<TContractState, ERC20Storage>,
    +Drop<TContractState>,
    +starknet::event::EventEmitter<TContractState, ContractStateEvent>,
    +Into<Event, ContractStateEvent>,
> of ERC20HelperTrait<TContractState> {
    fn transfer_helper(
        ref self: TContractState, sender: ContractAddress, recipient: ContractAddress, amount: u256,
    ) {
        assert(!sender.is_zero(), 'ERC20: transfer from 0');
        assert(!recipient.is_zero(), 'ERC20: transfer to 0');
        self.storage_mut().balances.write(sender, self.storage().balances.read(sender) - amount);
        self
            .storage_mut()
            .balances
            .write(recipient, self.storage().balances.read(recipient) + amount);
        self.emit(Event::Transfer(TransferEvent { from: sender, to: recipient, value: amount }));
    }

    fn spend_allowance(
        ref self: TContractState, owner: ContractAddress, spender: ContractAddress, amount: u256,
    ) {
        let current_allowance = self.storage().allowances.read((owner, spender));
        let ONES_MASK = 0xffffffffffffffffffffffffffffffff_u128;
        let is_unlimited_allowance = current_allowance.low == ONES_MASK
            && current_allowance.high == ONES_MASK;
        if !is_unlimited_allowance {
            self.approve_helper(owner, spender, current_allowance - amount);
        }
    }

    fn approve_helper(
        ref self: TContractState, owner: ContractAddress, spender: ContractAddress, amount: u256,
    ) {
        assert(!spender.is_zero(), 'ERC20: approve from 0');
        self.storage_mut().allowances.write((owner, spender), amount);
        self.emit(Event::Approval(ApprovalEvent { owner, spender, value: amount }));
    }
    fn erc20_init(
        ref self: TContractState,
        name: felt252,
        symbol: felt252,
        decimals: u8,
        initial_supply: u256,
        recipient: ContractAddress,
    ) {
        self.storage_mut().name.write(name);
        self.storage_mut().symbol.write(symbol);
        self.storage_mut().decimals.write(decimals);
        assert(!recipient.is_zero(), 'ERC20: mint to the 0 address');
        self.storage_mut().total_supply.write(initial_supply);
        self.storage_mut().balances.write(recipient, initial_supply);
        self
            .emit(
                Event::Transfer(
                    TransferEvent { from: Zero::zero(), to: recipient, value: initial_supply },
                ),
            );
    }
}
