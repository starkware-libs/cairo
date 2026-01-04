// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (token/src/erc20/erc20.cairo)

/// # ERC20 Component
///
/// The ERC20 component provides an implementation of the IERC20 interface as well as
/// non-standard implementations that can be used to create an ERC20 contract. This
/// component is agnostic regarding how tokens are created, which means that developers
/// must create their own token distribution mechanism.
/// See [the documentation]
/// (https://docs.openzeppelin.com/contracts-cairo/2.0.0/guides/erc20-supply)
/// for examples.
#[starknet::component]
pub mod ERC20Component {
    use core::num::traits::{Bounded, Zero};
    use openzeppelin_account::interface::{ISRC6Dispatcher, ISRC6DispatcherTrait};
    use openzeppelin_utils::cryptography::interface::{INonces, ISNIP12Metadata};
    use openzeppelin_utils::cryptography::snip12::{
        OffchainMessageHash, SNIP12Metadata, StarknetDomain, StructHash,
    };
    use openzeppelin_utils::nonces::NoncesComponent;
    use openzeppelin_utils::nonces::NoncesComponent::InternalTrait as NoncesInternalTrait;
    use starknet::ContractAddress;
    use starknet::storage::{
        Map, StorageMapReadAccess, StorageMapWriteAccess, StoragePointerReadAccess,
        StoragePointerWriteAccess,
    };
    use crate::erc20::interface;
    use crate::erc20::snip12_utils::permit::Permit;

    // This default decimals is only used when the DefaultConfig
    // is in scope in the implementing contract.
    pub const DEFAULT_DECIMALS: u8 = 18;

    #[storage]
    pub struct Storage {
        pub ERC20_name: ByteArray,
        pub ERC20_symbol: ByteArray,
        pub ERC20_total_supply: u256,
        pub ERC20_balances: Map<ContractAddress, u256>,
        pub ERC20_allowances: Map<(ContractAddress, ContractAddress), u256>,
    }

    #[event]
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub enum Event {
        Transfer: Transfer,
        Approval: Approval,
    }

    /// Emitted when tokens are moved from address `from` to address `to`.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct Transfer {
        #[key]
        pub from: ContractAddress,
        #[key]
        pub to: ContractAddress,
        pub value: u256,
    }

    /// Emitted when the allowance of a `spender` for an `owner` is set by a call
    /// to `approve`. `value` is the new allowance.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct Approval {
        #[key]
        pub owner: ContractAddress,
        #[key]
        pub spender: ContractAddress,
        pub value: u256,
    }

    pub mod Errors {
        pub const APPROVE_FROM_ZERO: felt252 = 'ERC20: approve from 0';
        pub const APPROVE_TO_ZERO: felt252 = 'ERC20: approve to 0';
        pub const TRANSFER_FROM_ZERO: felt252 = 'ERC20: transfer from 0';
        pub const TRANSFER_TO_ZERO: felt252 = 'ERC20: transfer to 0';
        pub const BURN_FROM_ZERO: felt252 = 'ERC20: burn from 0';
        pub const MINT_TO_ZERO: felt252 = 'ERC20: mint to 0';
        pub const INSUFFICIENT_BALANCE: felt252 = 'ERC20: insufficient balance';
        pub const INSUFFICIENT_ALLOWANCE: felt252 = 'ERC20: insufficient allowance';
        pub const EXPIRED_PERMIT_SIGNATURE: felt252 = 'ERC20: expired permit signature';
        pub const INVALID_PERMIT_SIGNATURE: felt252 = 'ERC20: invalid permit signature';
    }

    /// Constants expected to be defined at the contract level used to configure the component
    /// behaviour.
    ///
    /// - `DECIMALS`: Returns the number of decimals the token uses.
    pub trait ImmutableConfig {
        const DECIMALS: u8;
    }

    //
    // Hooks
    //

    pub trait ERC20HooksTrait<TContractState> {
        fn before_update(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) {}

        fn after_update(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) {}
    }

    //
    // External
    //

    #[embeddable_as(ERC20Impl)]
    impl ERC20<
        TContractState, +HasComponent<TContractState>, +ERC20HooksTrait<TContractState>,
    > of interface::IERC20<ComponentState<TContractState>> {
        /// Returns the value of tokens in existence.
        fn total_supply(self: @ComponentState<TContractState>) -> u256 {
            self.ERC20_total_supply.read()
        }

        /// Returns the amount of tokens owned by `account`.
        fn balance_of(self: @ComponentState<TContractState>, account: ContractAddress) -> u256 {
            self.ERC20_balances.read(account)
        }

        /// Returns the remaining number of tokens that `spender` is
        /// allowed to spend on behalf of `owner` through `transfer_from`.
        /// This is zero by default.
        /// This value changes when `approve` or `transfer_from` are called.
        fn allowance(
            self: @ComponentState<TContractState>, owner: ContractAddress, spender: ContractAddress,
        ) -> u256 {
            self.ERC20_allowances.read((owner, spender))
        }

        /// Moves `amount` tokens from the caller's token balance to `to`.
        ///
        /// Requirements:
        ///
        /// - `recipient` is not the zero address.
        /// - The caller has a balance of at least `amount`.
        ///
        /// Emits a `Transfer` event.
        fn transfer(
            ref self: ComponentState<TContractState>, recipient: ContractAddress, amount: u256,
        ) -> bool {
            let sender = starknet::get_caller_address();
            self._transfer(sender, recipient, amount);
            true
        }

        /// Moves `amount` tokens from `from` to `to` using the allowance mechanism.
        /// `amount` is then deducted from the caller's allowance.
        ///
        /// Requirements:
        ///
        /// - `sender` is not the zero address.
        /// - `sender` must have a balance of at least `amount`.
        /// - `recipient` is not the zero address.
        /// - The caller has an allowance of `sender`'s tokens of at least `amount`.
        ///
        /// Emits a `Transfer` event.
        fn transfer_from(
            ref self: ComponentState<TContractState>,
            sender: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) -> bool {
            let caller = starknet::get_caller_address();
            self._spend_allowance(sender, caller, amount);
            self._transfer(sender, recipient, amount);
            true
        }

        /// Sets `amount` as the allowance of `spender` over the caller’s tokens.
        ///
        /// Requirements:
        ///
        /// - `spender` is not the zero address.
        ///
        /// Emits an `Approval` event.
        fn approve(
            ref self: ComponentState<TContractState>, spender: ContractAddress, amount: u256,
        ) -> bool {
            let caller = starknet::get_caller_address();
            self._approve(caller, spender, amount);
            true
        }
    }

    #[embeddable_as(ERC20MetadataImpl)]
    impl ERC20Metadata<
        TContractState,
        +HasComponent<TContractState>,
        impl Immutable: ImmutableConfig,
        +ERC20HooksTrait<TContractState>,
    > of interface::IERC20Metadata<ComponentState<TContractState>> {
        /// Returns the name of the token.
        fn name(self: @ComponentState<TContractState>) -> ByteArray {
            self.ERC20_name.read()
        }

        /// Returns the ticker symbol of the token, usually a shorter version of the name.
        fn symbol(self: @ComponentState<TContractState>) -> ByteArray {
            self.ERC20_symbol.read()
        }

        /// Returns the number of decimals used to get its user representation.
        fn decimals(self: @ComponentState<TContractState>) -> u8 {
            Immutable::DECIMALS
        }
    }

    /// Adds camelCase support for `IERC20`.
    #[embeddable_as(ERC20CamelOnlyImpl)]
    impl ERC20CamelOnly<
        TContractState, +HasComponent<TContractState>, +ERC20HooksTrait<TContractState>,
    > of interface::IERC20CamelOnly<ComponentState<TContractState>> {
        fn totalSupply(self: @ComponentState<TContractState>) -> u256 {
            ERC20::total_supply(self)
        }

        fn balanceOf(self: @ComponentState<TContractState>, account: ContractAddress) -> u256 {
            ERC20::balance_of(self, account)
        }

        fn transferFrom(
            ref self: ComponentState<TContractState>,
            sender: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) -> bool {
            ERC20::transfer_from(ref self, sender, recipient, amount)
        }
    }

    #[embeddable_as(ERC20MixinImpl)]
    impl ERC20Mixin<
        TContractState,
        +HasComponent<TContractState>,
        +ImmutableConfig,
        +ERC20HooksTrait<TContractState>,
    > of interface::IERC20Mixin<ComponentState<TContractState>> {
        // IERC20
        fn total_supply(self: @ComponentState<TContractState>) -> u256 {
            ERC20::total_supply(self)
        }

        fn balance_of(self: @ComponentState<TContractState>, account: ContractAddress) -> u256 {
            ERC20::balance_of(self, account)
        }

        fn allowance(
            self: @ComponentState<TContractState>, owner: ContractAddress, spender: ContractAddress,
        ) -> u256 {
            ERC20::allowance(self, owner, spender)
        }

        fn transfer(
            ref self: ComponentState<TContractState>, recipient: ContractAddress, amount: u256,
        ) -> bool {
            ERC20::transfer(ref self, recipient, amount)
        }

        fn transfer_from(
            ref self: ComponentState<TContractState>,
            sender: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) -> bool {
            ERC20::transfer_from(ref self, sender, recipient, amount)
        }

        fn approve(
            ref self: ComponentState<TContractState>, spender: ContractAddress, amount: u256,
        ) -> bool {
            ERC20::approve(ref self, spender, amount)
        }

        // IERC20Metadata
        fn name(self: @ComponentState<TContractState>) -> ByteArray {
            ERC20Metadata::name(self)
        }

        fn symbol(self: @ComponentState<TContractState>) -> ByteArray {
            ERC20Metadata::symbol(self)
        }

        fn decimals(self: @ComponentState<TContractState>) -> u8 {
            ERC20Metadata::decimals(self)
        }

        // IERC20CamelOnly
        fn totalSupply(self: @ComponentState<TContractState>) -> u256 {
            ERC20CamelOnly::totalSupply(self)
        }

        fn balanceOf(self: @ComponentState<TContractState>, account: ContractAddress) -> u256 {
            ERC20CamelOnly::balanceOf(self, account)
        }

        fn transferFrom(
            ref self: ComponentState<TContractState>,
            sender: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) -> bool {
            ERC20CamelOnly::transferFrom(ref self, sender, recipient, amount)
        }
    }

    /// The ERC20Permit impl implements the EIP-2612 standard, facilitating token approvals via
    /// off-chain signatures. This approach allows token holders to delegate their approval to spend
    /// tokens without executing an on-chain transaction, reducing gas costs and enhancing
    /// usability.
    /// See https://eips.ethereum.org/EIPS/eip-2612.
    ///
    /// The message signed and the signature must follow the SNIP-12 standard for hashing and
    /// signing typed structured data.
    /// See https://github.com/starknet-io/SNIPs/blob/main/SNIPS/snip-12.md.
    ///
    /// To safeguard against replay attacks and ensure the uniqueness of each approval via `permit`,
    /// the data signed includes:
    ///   - The address of the owner.
    ///   - The parameters specified in the `approve` function (spender and amount).
    ///   - The address of the token contract itself.
    ///   - A nonce, which must be unique for each operation, incrementing after each use to prevent
    ///   reuse of the signature.
    ///   - The chain ID, which protects against cross-chain replay attacks.
    #[embeddable_as(ERC20PermitImpl)]
    impl ERC20Permit<
        TContractState,
        +HasComponent<TContractState>,
        +ERC20HooksTrait<TContractState>,
        impl Nonces: NoncesComponent::HasComponent<TContractState>,
        impl Metadata: SNIP12Metadata,
        +Drop<TContractState>,
    > of interface::IERC20Permit<ComponentState<TContractState>> {
        /// Sets `amount` as the allowance of `spender` over `owner`'s tokens after validating the
        /// signature.
        ///
        /// Requirements:
        ///
        /// - `owner` is a deployed account contract.
        /// - `spender` is not the zero address.
        /// - `deadline` is not a timestamp in the past.
        /// - `signature` is a valid signature that can be validated with a call to `owner` account.
        /// - `signature` must use the current nonce of the `owner`.
        ///
        /// Emits an `Approval` event.
        /// Every successful call increases `owner`'s nonce by one.
        fn permit(
            ref self: ComponentState<TContractState>,
            owner: ContractAddress,
            spender: ContractAddress,
            amount: u256,
            deadline: u64,
            signature: Span<felt252>,
        ) {
            // 1. Ensure the deadline is not missed
            assert(starknet::get_block_timestamp() <= deadline, Errors::EXPIRED_PERMIT_SIGNATURE);

            // 2. Get the current nonce and increment it
            let mut nonces_component = get_dep_component_mut!(ref self, Nonces);
            let nonce = nonces_component.use_nonce(owner);

            // 3. Make a call to the account to validate permit signature
            let permit = Permit {
                token: starknet::get_contract_address(), spender, amount, nonce, deadline,
            };
            let permit_hash = permit.get_message_hash(owner);
            let is_valid_sig_felt = ISRC6Dispatcher { contract_address: owner }
                .is_valid_signature(permit_hash, signature.into());

            // 4. Check the response is either 'VALID' or True (for backwards compatibility)
            let is_valid_sig = is_valid_sig_felt == starknet::VALIDATED || is_valid_sig_felt == 1;
            assert(is_valid_sig, Errors::INVALID_PERMIT_SIGNATURE);

            // 5. Approve
            self._approve(owner, spender, amount);
        }

        /// Returns the current nonce of `owner`. A nonce value must be
        /// included whenever a signature for `permit` call is generated.
        fn nonces(self: @ComponentState<TContractState>, owner: ContractAddress) -> felt252 {
            let nonces_component = get_dep_component!(self, Nonces);
            nonces_component.nonces(owner)
        }

        /// Returns the domain separator used in generating a message hash for `permit` signature.
        /// The domain hashing logic follows SNIP-12 standard.
        fn DOMAIN_SEPARATOR(self: @ComponentState<TContractState>) -> felt252 {
            let domain = StarknetDomain {
                name: Metadata::name(),
                version: Metadata::version(),
                chain_id: starknet::get_tx_info().unbox().chain_id,
                revision: 1,
            };
            domain.hash_struct()
        }
    }

    #[embeddable_as(SNIP12MetadataExternalImpl)]
    impl SNIP12MetadataExternal<
        TContractState, +HasComponent<TContractState>, impl Metadata: SNIP12Metadata,
    > of ISNIP12Metadata<ComponentState<TContractState>> {
        /// Returns the domain name and version used to generate the message hash for for permit
        /// signature.
        ///
        /// The returned tuple contains:
        ///
        /// - `t.0`: The name used in the SNIP12Metadata implementation.
        /// - `t.1`: The version used in the SNIP12Metadata implementation.
        fn snip12_metadata(self: @ComponentState<TContractState>) -> (felt252, felt252) {
            (Metadata::name(), Metadata::version())
        }
    }

    //
    // Internal
    //

    #[generate_trait]
    pub impl InternalImpl<
        TContractState, +HasComponent<TContractState>, impl Hooks: ERC20HooksTrait<TContractState>,
    > of InternalTrait<TContractState> {
        /// Initializes the contract by setting the token name and symbol.
        /// To prevent reinitialization, this should only be used inside of a contract's
        /// constructor.
        fn initializer(
            ref self: ComponentState<TContractState>, name: ByteArray, symbol: ByteArray,
        ) {
            self.ERC20_name.write(name);
            self.ERC20_symbol.write(symbol);
        }

        /// Creates a `value` amount of tokens and assigns them to `account`.
        ///
        /// Requirements:
        ///
        /// - `recipient` is not the zero address.
        ///
        /// Emits a `Transfer` event with `from` set to the zero address.
        fn mint(
            ref self: ComponentState<TContractState>, recipient: ContractAddress, amount: u256,
        ) {
            assert(!recipient.is_zero(), Errors::MINT_TO_ZERO);
            self.update(Zero::zero(), recipient, amount);
        }

        /// Destroys `amount` of tokens from `account`.
        ///
        /// Requirements:
        ///
        /// - `account` is not the zero address.
        /// - `account` must have at least a balance of `amount`.
        ///
        /// Emits a `Transfer` event with `to` set to the zero address.
        fn burn(ref self: ComponentState<TContractState>, account: ContractAddress, amount: u256) {
            assert(!account.is_zero(), Errors::BURN_FROM_ZERO);
            self.update(account, Zero::zero(), amount);
        }

        /// Transfers an `amount` of tokens from `from` to `to`, or alternatively mints (or burns)
        /// if `from` (or `to`) is the zero address.
        ///
        /// NOTE: This function can be extended using the `ERC20HooksTrait`, to add
        /// functionality before and/or after the transfer, mint, or burn.
        ///
        /// Emits a `Transfer` event.
        fn update(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            to: ContractAddress,
            amount: u256,
        ) {
            Hooks::before_update(ref self, from, to, amount);

            let zero_address = Zero::zero();
            if (from == zero_address) {
                let total_supply = self.ERC20_total_supply.read();
                self.ERC20_total_supply.write(total_supply + amount);
            } else {
                let from_balance = self.ERC20_balances.read(from);
                assert(from_balance >= amount, Errors::INSUFFICIENT_BALANCE);
                self.ERC20_balances.write(from, from_balance - amount);
            }

            if (to == zero_address) {
                let total_supply = self.ERC20_total_supply.read();
                self.ERC20_total_supply.write(total_supply - amount);
            } else {
                let to_balance = self.ERC20_balances.read(to);
                self.ERC20_balances.write(to, to_balance + amount);
            }

            self.emit(Transfer { from, to, value: amount });

            Hooks::after_update(ref self, from, to, amount);
        }

        /// Internal method that moves an `amount` of tokens from `from` to `to`.
        ///
        /// Requirements:
        ///
        /// - `sender` is not the zero address.
        /// - `sender` must have at least a balance of `amount`.
        /// - `recipient` is not the zero address.
        ///
        /// Emits a `Transfer` event.
        fn _transfer(
            ref self: ComponentState<TContractState>,
            sender: ContractAddress,
            recipient: ContractAddress,
            amount: u256,
        ) {
            assert(!sender.is_zero(), Errors::TRANSFER_FROM_ZERO);
            assert(!recipient.is_zero(), Errors::TRANSFER_TO_ZERO);
            self.update(sender, recipient, amount);
        }

        /// Internal method that sets `amount` as the allowance of `spender` over the
        /// `owner`s tokens.
        ///
        /// Requirements:
        ///
        /// - `owner` is not the zero address.
        /// - `spender` is not the zero address.
        ///
        /// Emits an `Approval` event.
        fn _approve(
            ref self: ComponentState<TContractState>,
            owner: ContractAddress,
            spender: ContractAddress,
            amount: u256,
        ) {
            assert(!owner.is_zero(), Errors::APPROVE_FROM_ZERO);
            assert(!spender.is_zero(), Errors::APPROVE_TO_ZERO);
            self.ERC20_allowances.write((owner, spender), amount);
            self.emit(Approval { owner, spender, value: amount });
        }

        /// Updates `owner`s allowance for `spender` based on spent `amount`.
        /// Does not update the allowance value in case of infinite allowance.
        ///
        /// Requirements:
        ///
        /// - `spender` must have at least an allowance of `amount` from `owner`.
        ///
        /// Possibly emits an `Approval` event.
        fn _spend_allowance(
            ref self: ComponentState<TContractState>,
            owner: ContractAddress,
            spender: ContractAddress,
            amount: u256,
        ) {
            let current_allowance = self.ERC20_allowances.read((owner, spender));
            if current_allowance != Bounded::MAX {
                assert(current_allowance >= amount, Errors::INSUFFICIENT_ALLOWANCE);
                self._approve(owner, spender, current_allowance - amount);
            }
        }
    }
}

/// Implementation of the default ERC20Component ImmutableConfig.
///
/// See
/// https://github.com/starknet-io/SNIPs/blob/main/SNIPS/snip-107.md#defaultconfig-implementation
///
/// The default decimals is set to `DEFAULT_DECIMALS`.
pub impl DefaultConfig of ERC20Component::ImmutableConfig {
    const DECIMALS: u8 = ERC20Component::DEFAULT_DECIMALS;
}

/// An empty implementation of the ERC20 hooks to be used in basic ERC20 preset contracts.
pub impl ERC20HooksEmptyImpl<TContractState> of ERC20Component::ERC20HooksTrait<TContractState> {}
