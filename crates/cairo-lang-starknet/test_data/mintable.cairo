// The ERC20 and Ownable components are duplicates of the ones in `with_erc20` and `with_ownable`
// files. The duplication is done since the expand test which also use this file is not aware to
// the rest of the crate. The contract using these components is at the end of the file.
// TODO(Gil): Add the capability to read multiple files in the test framework and remove this
// duplication.

use starknet::ContractAddress;

#[starknet::interface]
trait TransferTrait<TContractState> {
    fn owner(self: @TContractState) -> ContractAddress;
    fn transfer_ownership(ref self: TContractState, new_owner: ContractAddress);
}

#[starknet::component]
mod ownable {
    use starknet::ContractAddress;
    #[storage]
    struct Storage {
        owner: ContractAddress,
    }

    #[embeddable_as(Transfer)]
    impl TransferImpl<
        TContractState, impl X: HasComponent<TContractState>,
    > of super::TransferTrait<ComponentState<TContractState>> {
        fn owner(self: @ComponentState<TContractState>) -> ContractAddress {
            self.owner.read()
        }

        fn transfer_ownership(
            ref self: ComponentState<TContractState>, new_owner: ContractAddress,
        ) {
            self.validate_ownership();
            self.owner.write(new_owner);
        }
    }

    #[generate_trait]
    impl OwnableHelperImpl<
        TContractState, impl X: HasComponent<TContractState>,
    > of OwnableHelperTrait<TContractState, X> {
        fn init_ownable(ref self: ComponentState<TContractState>, owner: ContractAddress) {
            self.owner.write(owner);
        }
        fn validate_ownership(self: @ComponentState<TContractState>) {
            assert(self.owner.read() == starknet::get_caller_address(), 'Wrong owner.');
        }
    }
}


#[starknet::interface]
trait ERC20Trait<TCS> {
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
mod erc20 {
    use core::num::traits::Zero;
    use starknet::{ContractAddress, contract_address_const, get_caller_address};
    #[storage]
    struct Storage {
        name: felt252,
        symbol: felt252,
        decimals: u8,
        total_supply: u256,
        balances: LegacyMap<ContractAddress, u256>,
        allowances: LegacyMap<(ContractAddress, ContractAddress), u256>,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        Transfer: TransferEvent,
        Approval: ApprovalEvent,
    }
    #[derive(Drop, starknet::Event)]
    struct TransferEvent {
        from: ContractAddress,
        to: ContractAddress,
        value: u256,
    }
    #[derive(Drop, starknet::Event)]
    struct ApprovalEvent {
        owner: ContractAddress,
        spender: ContractAddress,
        value: u256,
    }

    #[embeddable_as(IERC20)]
    impl ERC20Impl<
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
    impl ERC20HelperImpl<
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
            let current_allowance: u256 = self.allowances.read((owner, spender));
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
                        TransferEvent {
                            from: contract_address_const::<0>(),
                            to: recipient,
                            value: initial_supply,
                        },
                    ),
                );
        }
    }
}

// End of copied components.

#[starknet::interface]
trait MintTrait<TContractState> {
    fn mint(ref self: TContractState, account: ContractAddress, amount: u256);
}

#[starknet::component]
mod mintable {
    use starknet::{ContractAddress, contract_address_const};
    use super::ownable::OwnableHelperImpl;
    #[storage]
    struct Storage {}

    #[embeddable_as(Mint)]
    impl MintImpl<
        TContractState,
        +HasComponent<TContractState>,
        impl Ownable: super::ownable::HasComponent<TContractState>,
        impl ERC20: super::erc20::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of super::MintTrait<ComponentState<TContractState>> {
        fn mint(ref self: ComponentState<TContractState>, account: ContractAddress, amount: u256) {
            assert(!account.is_zero(), 'ERC20: mint to the 0 address');
            get_dep_component!(@self, Ownable).validate_ownership();
            let mut erc20_component = get_dep_component_mut!(ref self, ERC20);
            let total_supply = erc20_component.total_supply.read();
            erc20_component.total_supply.write(total_supply + amount);
            erc20_component
                .balances
                .write(account, erc20_component.balances.read(account) + amount);
            erc20_component
                .emit(
                    super::erc20::TransferEvent {
                        from: contract_address_const::<0>(), to: account, value: amount,
                    },
                );
        }
    }
}

#[starknet::interface]
trait GetSupply<TContractState> {
    fn get_total_supply_plus_1(self: @TContractState) -> u256;
}

#[starknet::contract]
mod mintable_erc20_ownable {
    use starknet::ContractAddress;
    #[storage]
    struct Storage {
        #[substorage(v0)]
        erc20_storage: super::erc20::Storage,
        #[substorage(v0)]
        ownable_storage: super::ownable::Storage,
        #[substorage(v0)]
        mintable_storage: super::mintable::Storage,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        ERC20: super::erc20::Event,
        Ownable: super::ownable::Event,
        Mintable: super::mintable::Event,
    }

    component!(path: super::erc20, storage: erc20_storage, event: ERC20);
    component!(path: super::ownable, storage: ownable_storage, event: Ownable);
    component!(path: super::mintable, storage: mintable_storage, event: Mintable);

    #[abi(embed_v0)]
    impl ERC20Impl = super::erc20::IERC20<ContractState>;

    impl ERC20Helper = super::erc20::ERC20HelperImpl<ContractState>;

    #[abi(embed_v0)]
    impl OwnershipTransfer = super::ownable::Transfer<ContractState>;

    impl OwnershipHelper = super::ownable::OwnableHelperImpl<ContractState>;

    #[abi(embed_v0)]
    impl MintImpl = super::mintable::Mint<ContractState>;

    #[abi(per_item)]
    #[generate_trait]
    impl ImplCtor of TraitCtor {
        #[constructor]
        fn constructor(
            ref self: ContractState,
            name: felt252,
            symbol: felt252,
            decimals: u8,
            initial_supply: u256,
            recipient: ContractAddress,
            owner: ContractAddress,
        ) {
            self.erc20_storage.init(name, symbol, decimals, initial_supply, recipient);
            self.ownable_storage.init_ownable(owner);
        }
    }

    #[abi(embed_v0)]
    impl ImplGetSupply of super::GetSupply<ContractState> {
        fn get_total_supply_plus_1(self: @ContractState) -> u256 {
            self.erc20_storage.get_total_supply() + 1
        }
    }
}
