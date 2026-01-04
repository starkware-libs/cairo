use starknet::ContractAddress;

#[starknet::contract]
#[with_components(ERC20)]
pub mod DualCaseERC20Mock {
    use openzeppelin_token::erc20::{DefaultConfig, ERC20HooksEmptyImpl};
    use starknet::ContractAddress;

    #[abi(embed_v0)]
    impl ERC20Impl = ERC20Component::ERC20Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC20MetadataImpl = ERC20Component::ERC20MetadataImpl<ContractState>;
    #[abi(embed_v0)]
    impl ERC20CamelOnlyImpl = ERC20Component::ERC20CamelOnlyImpl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(
        ref self: ContractState,
        name: ByteArray,
        symbol: ByteArray,
        initial_supply: u256,
        recipient: ContractAddress,
    ) {
        self.erc20.initializer(name, symbol);
        self.erc20.mint(recipient, initial_supply);
    }
}

#[starknet::contract]
#[with_components(ERC20)]
pub mod SnakeERC20Mock {
    use openzeppelin_token::erc20::{DefaultConfig, ERC20HooksEmptyImpl};
    use starknet::ContractAddress;

    #[abi(embed_v0)]
    impl ERC20Impl = ERC20Component::ERC20Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC20MetadataImpl = ERC20Component::ERC20MetadataImpl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(
        ref self: ContractState,
        name: ByteArray,
        symbol: ByteArray,
        initial_supply: u256,
        recipient: ContractAddress,
    ) {
        self.erc20.initializer(name, symbol);
        self.erc20.mint(recipient, initial_supply);
    }
}

/// Similar to `SnakeERC20Mock`, but emits events for `before_update` and `after_update` hooks.
/// This is used to test that the hooks are called with the correct arguments.
#[starknet::contract]
#[with_components(ERC20)]
pub mod SnakeERC20MockWithHooks {
    use openzeppelin_token::erc20::DefaultConfig;
    use starknet::ContractAddress;

    #[abi(embed_v0)]
    impl ERC20Impl = ERC20Component::ERC20Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC20MetadataImpl = ERC20Component::ERC20MetadataImpl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[event]
    #[derive(Drop, starknet::Event)]
    pub enum Event {
        BeforeUpdate: BeforeUpdate,
        AfterUpdate: AfterUpdate,
    }

    /// Event used to test that `before_update` hook is called.
    #[derive(Drop, PartialEq, starknet::Event)]
    pub struct BeforeUpdate {
        pub from: ContractAddress,
        pub recipient: ContractAddress,
        pub amount: u256,
    }

    /// Event used to test that `after_update` hook is called.
    #[derive(Drop, PartialEq, starknet::Event)]
    pub struct AfterUpdate {
        pub from: ContractAddress,
        pub recipient: ContractAddress,
        pub amount: u256,
    }

    #[constructor]
    fn constructor(
        ref self: ContractState,
        name: ByteArray,
        symbol: ByteArray,
        initial_supply: u256,
        recipient: ContractAddress,
    ) {
        self.erc20.initializer(name, symbol);
        self.erc20.mint(recipient, initial_supply);
    }

    impl ERC20HooksImpl of ERC20Component::ERC20HooksTrait<ContractState> {
        fn before_update(
            ref self: ERC20Component::ComponentState<ContractState>,
            from: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) {
            let mut contract_state = self.get_contract_mut();
            contract_state.emit(BeforeUpdate { from, recipient, amount });
        }

        fn after_update(
            ref self: ERC20Component::ComponentState<ContractState>,
            from: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) {
            let mut contract_state = self.get_contract_mut();
            contract_state.emit(AfterUpdate { from, recipient, amount });
        }
    }
}

#[starknet::contract]
#[with_components(ERC20, Nonces)]
pub mod DualCaseERC20PermitMock {
    use openzeppelin_token::erc20::{DefaultConfig, ERC20HooksEmptyImpl};
    use openzeppelin_utils::cryptography::snip12::SNIP12Metadata;
    use starknet::ContractAddress;

    // ERC20Mixin
    #[abi(embed_v0)]
    impl ERC20MixinImpl = ERC20Component::ERC20MixinImpl<ContractState>;

    // IERC20Permit
    #[abi(embed_v0)]
    impl ERC20PermitImpl = ERC20Component::ERC20PermitImpl<ContractState>;

    // ISNIP12Metadata
    #[abi(embed_v0)]
    impl SNIP12MetadataExternal =
        ERC20Component::SNIP12MetadataExternalImpl<ContractState>;

    #[storage]
    pub struct Storage {}

    /// Required for hash computation.
    pub impl SNIP12MetadataImpl of SNIP12Metadata {
        fn name() -> felt252 {
            'DAPP_NAME'
        }
        fn version() -> felt252 {
            'DAPP_VERSION'
        }
    }

    /// Sets the token `name` and `symbol`.
    /// Mints `fixed_supply` tokens to `recipient`.
    #[constructor]
    fn constructor(
        ref self: ContractState,
        name: ByteArray,
        symbol: ByteArray,
        initial_supply: u256,
        recipient: ContractAddress,
    ) {
        self.erc20.initializer(name, symbol);
        self.erc20.mint(recipient, initial_supply);
    }
}

#[derive(Drop, Serde, PartialEq, Debug, starknet::Store)]
pub enum Type {
    #[default]
    No,
    Before,
    After,
}

#[starknet::interface]
pub trait IERC20ReentrantHelpers<TState> {
    fn schedule_reenter(
        ref self: TState,
        when: Type,
        target: ContractAddress,
        selector: felt252,
        calldata: Span<felt252>,
    );
    fn function_call(ref self: TState);
    fn unsafe_mint(ref self: TState, recipient: ContractAddress, amount: u256);
    fn unsafe_burn(ref self: TState, account: ContractAddress, amount: u256);
}

#[starknet::interface]
pub trait IERC20Reentrant<TState> {
    fn schedule_reenter(
        ref self: TState,
        when: Type,
        target: ContractAddress,
        selector: felt252,
        calldata: Span<felt252>,
    );
    fn function_call(ref self: TState);
    fn unsafe_mint(ref self: TState, recipient: ContractAddress, amount: u256);
    fn unsafe_burn(ref self: TState, account: ContractAddress, amount: u256);

    // IERC20
    fn total_supply(self: @TState) -> u256;
    fn balance_of(self: @TState, account: ContractAddress) -> u256;
    fn allowance(self: @TState, owner: ContractAddress, spender: ContractAddress) -> u256;
    fn transfer(ref self: TState, recipient: ContractAddress, amount: u256) -> bool;
    fn transfer_from(
        ref self: TState, sender: ContractAddress, recipient: ContractAddress, amount: u256,
    ) -> bool;
    fn approve(ref self: TState, spender: ContractAddress, amount: u256) -> bool;
}

#[starknet::contract]
#[with_components(ERC20)]
pub mod ERC20ReentrantMock {
    use openzeppelin_token::erc20::DefaultConfig;
    use starknet::storage::{
        MutableVecTrait, StoragePointerReadAccess, StoragePointerWriteAccess, Vec,
    };
    use starknet::syscalls::call_contract_syscall;
    use starknet::{ContractAddress, SyscallResultTrait};
    use super::Type;

    #[abi(embed_v0)]
    impl ERC20Impl = ERC20Component::ERC20Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC20MetadataImpl = ERC20Component::ERC20MetadataImpl<ContractState>;
    #[abi(embed_v0)]
    impl ERC20CamelOnlyImpl = ERC20Component::ERC20CamelOnlyImpl<ContractState>;

    #[storage]
    pub struct Storage {
        reenter_type: Type,
        reenter_target: ContractAddress,
        reenter_selector: felt252,
        reenter_calldata: Vec<felt252>,
    }

    //
    // Hooks
    //

    impl ERC20ReentrantImpl of ERC20Component::ERC20HooksTrait<ContractState> {
        fn before_update(
            ref self: ERC20Component::ComponentState<ContractState>,
            from: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) {
            let mut contract_state = self.get_contract_mut();

            if contract_state.reenter_type.read() == Type::Before {
                contract_state.reenter_type.write(Type::No);
                contract_state.function_call();
            }
        }

        fn after_update(
            ref self: ERC20Component::ComponentState<ContractState>,
            from: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) {
            let mut contract_state = self.get_contract_mut();

            if contract_state.reenter_type.read() == Type::After {
                contract_state.reenter_type.write(Type::No);
                contract_state.function_call();
            }
        }
    }

    #[abi(embed_v0)]
    pub impl ERC20ReentrantHelpers of super::IERC20ReentrantHelpers<ContractState> {
        fn schedule_reenter(
            ref self: ContractState,
            when: Type,
            target: ContractAddress,
            selector: felt252,
            calldata: Span<felt252>,
        ) {
            self.reenter_type.write(when);
            self.reenter_target.write(target);
            self.reenter_selector.write(selector);
            for elem in calldata {
                self.reenter_calldata.push(*elem);
            }
        }

        fn function_call(ref self: ContractState) {
            let target = self.reenter_target.read();
            let selector = self.reenter_selector.read();
            let mut calldata = array![];
            for i in 0..self.reenter_calldata.len() {
                calldata.append(self.reenter_calldata.at(i).read());
            }
            call_contract_syscall(target, selector, calldata.span()).unwrap_syscall();
        }

        fn unsafe_mint(ref self: ContractState, recipient: ContractAddress, amount: u256) {
            self.erc20.mint(recipient, amount);
        }

        fn unsafe_burn(ref self: ContractState, account: ContractAddress, amount: u256) {
            self.erc20.burn(account, amount);
        }
    }

    #[constructor]
    fn constructor(ref self: ContractState, name: ByteArray, symbol: ByteArray) {
        self.erc20.initializer(name, symbol);
        self.reenter_type.write(Type::No);
    }
}
