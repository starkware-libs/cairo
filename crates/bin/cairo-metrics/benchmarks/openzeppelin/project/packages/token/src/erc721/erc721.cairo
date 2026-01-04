// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (token/src/erc721/erc721.cairo)

/// # ERC721 Component
///
/// The ERC721 component provides implementations for both the IERC721 interface
/// and the IERC721Metadata interface.
#[starknet::component]
pub mod ERC721Component {
    use core::num::traits::Zero;
    use openzeppelin_introspection::interface::{ISRC5Dispatcher, ISRC5DispatcherTrait};
    use openzeppelin_introspection::src5::SRC5Component;
    use openzeppelin_introspection::src5::SRC5Component::{
        InternalTrait as SRC5InternalTrait, SRC5Impl,
    };
    use starknet::storage::{
        Map, StorageMapReadAccess, StorageMapWriteAccess, StoragePointerReadAccess,
        StoragePointerWriteAccess,
    };
    use starknet::{ContractAddress, get_caller_address};
    use crate::erc721::interface;
    use crate::erc721::interface::{IERC721ReceiverDispatcher, IERC721ReceiverDispatcherTrait};

    #[storage]
    pub struct Storage {
        pub ERC721_name: ByteArray,
        pub ERC721_symbol: ByteArray,
        pub ERC721_owners: Map<u256, ContractAddress>,
        pub ERC721_balances: Map<ContractAddress, u256>,
        pub ERC721_token_approvals: Map<u256, ContractAddress>,
        pub ERC721_operator_approvals: Map<(ContractAddress, ContractAddress), bool>,
        pub ERC721_base_uri: ByteArray,
    }

    #[event]
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub enum Event {
        Transfer: Transfer,
        Approval: Approval,
        ApprovalForAll: ApprovalForAll,
    }

    /// Emitted when `token_id` token is transferred from `from` to `to`.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct Transfer {
        #[key]
        pub from: ContractAddress,
        #[key]
        pub to: ContractAddress,
        #[key]
        pub token_id: u256,
    }

    /// Emitted when `owner` enables `approved` to manage the `token_id` token.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct Approval {
        #[key]
        pub owner: ContractAddress,
        #[key]
        pub approved: ContractAddress,
        #[key]
        pub token_id: u256,
    }

    /// Emitted when `owner` enables or disables (`approved`) `operator` to manage
    /// all of its assets.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct ApprovalForAll {
        #[key]
        pub owner: ContractAddress,
        #[key]
        pub operator: ContractAddress,
        pub approved: bool,
    }

    pub mod Errors {
        pub const INVALID_TOKEN_ID: felt252 = 'ERC721: invalid token ID';
        pub const INVALID_ACCOUNT: felt252 = 'ERC721: invalid account';
        pub const INVALID_OPERATOR: felt252 = 'ERC721: invalid operator';
        pub const UNAUTHORIZED: felt252 = 'ERC721: unauthorized caller';
        pub const INVALID_RECEIVER: felt252 = 'ERC721: invalid receiver';
        pub const ALREADY_MINTED: felt252 = 'ERC721: token already minted';
        pub const INVALID_SENDER: felt252 = 'ERC721: invalid sender';
        pub const SAFE_MINT_FAILED: felt252 = 'ERC721: safe mint failed';
        pub const SAFE_TRANSFER_FAILED: felt252 = 'ERC721: safe transfer failed';
    }

    //
    // Hooks
    //

    pub trait ERC721HooksTrait<TContractState> {
        fn before_update(
            ref self: ComponentState<TContractState>,
            to: ContractAddress,
            token_id: u256,
            auth: ContractAddress,
        ) {}

        fn after_update(
            ref self: ComponentState<TContractState>,
            to: ContractAddress,
            token_id: u256,
            auth: ContractAddress,
        ) {}
    }

    //
    // External
    //

    #[embeddable_as(ERC721Impl)]
    impl ERC721<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +ERC721HooksTrait<TContractState>,
        +Drop<TContractState>,
    > of interface::IERC721<ComponentState<TContractState>> {
        /// Returns the number of NFTs owned by `account`.
        fn balance_of(self: @ComponentState<TContractState>, account: ContractAddress) -> u256 {
            assert(!account.is_zero(), Errors::INVALID_ACCOUNT);
            self.ERC721_balances.read(account)
        }

        /// Returns the owner address of `token_id`.
        ///
        /// Requirements:
        ///
        /// - `token_id` exists.
        fn owner_of(self: @ComponentState<TContractState>, token_id: u256) -> ContractAddress {
            self._require_owned(token_id)
        }

        /// Transfers ownership of `token_id` from `from` if `to` is either an account or
        /// `IERC721Receiver`.
        ///
        /// `data` is additional data, it has no specified format and it is sent in call to `to`.
        ///
        /// WARNING: This method makes an external call to the recipient contract, which can lead to
        /// reentrancy vulnerabilities.
        ///
        /// Requirements:
        ///
        /// - Caller is either approved or the `token_id` owner.
        /// - `to` is not the zero address.
        /// - `from` is not the zero address.
        /// - `token_id` exists.
        /// - `to` is either an account contract or supports the `IERC721Receiver` interface.
        ///
        /// Emits a `Transfer` event.
        fn safe_transfer_from(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            to: ContractAddress,
            token_id: u256,
            data: Span<felt252>,
        ) {
            Self::transfer_from(ref self, from, to, token_id);
            assert(
                _check_on_erc721_received(from, to, token_id, data), Errors::SAFE_TRANSFER_FAILED,
            );
        }

        /// Transfers ownership of `token_id` from `from` to `to`.
        ///
        /// WARNING: This method may lead to the loss of tokens if `to` is not aware of the ERC721
        /// protocol.
        ///
        /// Requirements:
        ///
        /// - Caller is either approved or the `token_id` owner.
        /// - `to` is not the zero address.
        /// - `from` is not the zero address.
        /// - `token_id` exists.
        ///
        /// Emits a `Transfer` event.
        fn transfer_from(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            to: ContractAddress,
            token_id: u256,
        ) {
            assert(!to.is_zero(), Errors::INVALID_RECEIVER);

            // Setting an "auth" arguments enables the `_is_authorized` check which verifies that
            // the token exists (from != 0). Therefore, it is not needed to verify that the return
            // value is not 0 here.
            let previous_owner = self.update(to, token_id, get_caller_address());

            assert(from == previous_owner, Errors::INVALID_SENDER);
        }

        /// Change or reaffirm the approved address for an NFT.
        ///
        /// Requirements:
        ///
        /// - The caller is either an approved operator or the `token_id` owner.
        /// - `token_id` exists.
        ///
        /// Emits an `Approval` event.
        fn approve(ref self: ComponentState<TContractState>, to: ContractAddress, token_id: u256) {
            self._approve(to, token_id, get_caller_address());
        }

        /// Enable or disable approval for `operator` to manage all of the
        /// caller's assets.
        ///
        /// Requirements:
        ///
        /// - `operator` is not the zero address.
        ///
        /// Emits an `Approval` event.
        fn set_approval_for_all(
            ref self: ComponentState<TContractState>, operator: ContractAddress, approved: bool,
        ) {
            self._set_approval_for_all(get_caller_address(), operator, approved)
        }

        /// Returns the address approved for `token_id`.
        ///
        /// Requirements:
        ///
        /// - `token_id` exists.
        fn get_approved(self: @ComponentState<TContractState>, token_id: u256) -> ContractAddress {
            self._require_owned(token_id);
            self.ERC721_token_approvals.read(token_id)
        }

        /// Query if `operator` is an authorized operator for `owner`.
        fn is_approved_for_all(
            self: @ComponentState<TContractState>,
            owner: ContractAddress,
            operator: ContractAddress,
        ) -> bool {
            self.ERC721_operator_approvals.read((owner, operator))
        }
    }

    #[embeddable_as(ERC721MetadataImpl)]
    impl ERC721Metadata<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +ERC721HooksTrait<TContractState>,
        +Drop<TContractState>,
    > of interface::IERC721Metadata<ComponentState<TContractState>> {
        /// Returns the NFT name.
        fn name(self: @ComponentState<TContractState>) -> ByteArray {
            self.ERC721_name.read()
        }

        /// Returns the NFT symbol.
        fn symbol(self: @ComponentState<TContractState>) -> ByteArray {
            self.ERC721_symbol.read()
        }

        /// Returns the Uniform Resource Identifier (URI) for the `token_id` token.
        /// If the URI is not set, the return value will be an empty ByteArray.
        ///
        /// Requirements:
        ///
        /// - `token_id` exists.
        fn token_uri(self: @ComponentState<TContractState>, token_id: u256) -> ByteArray {
            self._require_owned(token_id);
            let base_uri = self._base_uri();
            if base_uri.len() == 0 {
                return "";
            } else {
                return format!("{}{}", base_uri, token_id);
            }
        }
    }

    /// Adds camelCase support for `IERC721`.
    #[embeddable_as(ERC721CamelOnlyImpl)]
    impl ERC721CamelOnly<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +ERC721HooksTrait<TContractState>,
        +Drop<TContractState>,
    > of interface::IERC721CamelOnly<ComponentState<TContractState>> {
        fn balanceOf(self: @ComponentState<TContractState>, account: ContractAddress) -> u256 {
            ERC721::balance_of(self, account)
        }

        fn ownerOf(self: @ComponentState<TContractState>, tokenId: u256) -> ContractAddress {
            ERC721::owner_of(self, tokenId)
        }

        fn safeTransferFrom(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            to: ContractAddress,
            tokenId: u256,
            data: Span<felt252>,
        ) {
            ERC721::safe_transfer_from(ref self, from, to, tokenId, data)
        }

        fn transferFrom(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            to: ContractAddress,
            tokenId: u256,
        ) {
            ERC721::transfer_from(ref self, from, to, tokenId)
        }

        fn setApprovalForAll(
            ref self: ComponentState<TContractState>, operator: ContractAddress, approved: bool,
        ) {
            ERC721::set_approval_for_all(ref self, operator, approved)
        }

        fn getApproved(self: @ComponentState<TContractState>, tokenId: u256) -> ContractAddress {
            ERC721::get_approved(self, tokenId)
        }

        fn isApprovedForAll(
            self: @ComponentState<TContractState>,
            owner: ContractAddress,
            operator: ContractAddress,
        ) -> bool {
            ERC721::is_approved_for_all(self, owner, operator)
        }
    }

    /// Adds camelCase support for `IERC721Metadata`.
    #[embeddable_as(ERC721MetadataCamelOnlyImpl)]
    impl ERC721MetadataCamelOnly<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +ERC721HooksTrait<TContractState>,
        +Drop<TContractState>,
    > of interface::IERC721MetadataCamelOnly<ComponentState<TContractState>> {
        fn tokenURI(self: @ComponentState<TContractState>, tokenId: u256) -> ByteArray {
            ERC721Metadata::token_uri(self, tokenId)
        }
    }

    #[embeddable_as(ERC721MixinImpl)]
    impl ERC721Mixin<
        TContractState,
        +HasComponent<TContractState>,
        impl SRC5: SRC5Component::HasComponent<TContractState>,
        +ERC721HooksTrait<TContractState>,
        +Drop<TContractState>,
    > of interface::ERC721ABI<ComponentState<TContractState>> {
        // IERC721
        fn balance_of(self: @ComponentState<TContractState>, account: ContractAddress) -> u256 {
            ERC721::balance_of(self, account)
        }

        fn owner_of(self: @ComponentState<TContractState>, token_id: u256) -> ContractAddress {
            ERC721::owner_of(self, token_id)
        }

        fn safe_transfer_from(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            to: ContractAddress,
            token_id: u256,
            data: Span<felt252>,
        ) {
            ERC721::safe_transfer_from(ref self, from, to, token_id, data);
        }

        fn transfer_from(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            to: ContractAddress,
            token_id: u256,
        ) {
            ERC721::transfer_from(ref self, from, to, token_id);
        }

        fn approve(ref self: ComponentState<TContractState>, to: ContractAddress, token_id: u256) {
            ERC721::approve(ref self, to, token_id);
        }

        fn set_approval_for_all(
            ref self: ComponentState<TContractState>, operator: ContractAddress, approved: bool,
        ) {
            ERC721::set_approval_for_all(ref self, operator, approved);
        }

        fn get_approved(self: @ComponentState<TContractState>, token_id: u256) -> ContractAddress {
            ERC721::get_approved(self, token_id)
        }

        fn is_approved_for_all(
            self: @ComponentState<TContractState>,
            owner: ContractAddress,
            operator: ContractAddress,
        ) -> bool {
            ERC721::is_approved_for_all(self, owner, operator)
        }

        // IERC721Metadata
        fn name(self: @ComponentState<TContractState>) -> ByteArray {
            ERC721Metadata::name(self)
        }

        fn symbol(self: @ComponentState<TContractState>) -> ByteArray {
            ERC721Metadata::symbol(self)
        }

        fn token_uri(self: @ComponentState<TContractState>, token_id: u256) -> ByteArray {
            ERC721Metadata::token_uri(self, token_id)
        }

        // IERC721CamelOnly
        fn balanceOf(self: @ComponentState<TContractState>, account: ContractAddress) -> u256 {
            ERC721CamelOnly::balanceOf(self, account)
        }

        fn ownerOf(self: @ComponentState<TContractState>, tokenId: u256) -> ContractAddress {
            ERC721CamelOnly::ownerOf(self, tokenId)
        }

        fn safeTransferFrom(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            to: ContractAddress,
            tokenId: u256,
            data: Span<felt252>,
        ) {
            ERC721CamelOnly::safeTransferFrom(ref self, from, to, tokenId, data);
        }

        fn transferFrom(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            to: ContractAddress,
            tokenId: u256,
        ) {
            ERC721CamelOnly::transferFrom(ref self, from, to, tokenId);
        }

        fn setApprovalForAll(
            ref self: ComponentState<TContractState>, operator: ContractAddress, approved: bool,
        ) {
            ERC721CamelOnly::setApprovalForAll(ref self, operator, approved);
        }

        fn getApproved(self: @ComponentState<TContractState>, tokenId: u256) -> ContractAddress {
            ERC721CamelOnly::getApproved(self, tokenId)
        }

        fn isApprovedForAll(
            self: @ComponentState<TContractState>,
            owner: ContractAddress,
            operator: ContractAddress,
        ) -> bool {
            ERC721CamelOnly::isApprovedForAll(self, owner, operator)
        }

        // IERC721MetadataCamelOnly
        fn tokenURI(self: @ComponentState<TContractState>, tokenId: u256) -> ByteArray {
            ERC721MetadataCamelOnly::tokenURI(self, tokenId)
        }

        // ISRC5
        fn supports_interface(
            self: @ComponentState<TContractState>, interface_id: felt252,
        ) -> bool {
            let src5 = get_dep_component!(self, SRC5);
            src5.supports_interface(interface_id)
        }
    }

    //
    // Internal
    //

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        impl SRC5: SRC5Component::HasComponent<TContractState>,
        impl Hooks: ERC721HooksTrait<TContractState>,
        +Drop<TContractState>,
    > of InternalTrait<TContractState> {
        /// Initializes the contract by setting the token name, symbol, and base URI.
        /// This should only be used inside the contract's constructor.
        ///
        /// WARNING: Most ERC721 contracts expose the IERC721Metadata interface which
        /// is what this initializer is meant to support.
        /// If the contract DOES NOT expose the IERC721Metadata interface,
        /// meaning the token does not have a name, symbol, or URI,
        /// the contract must instead instead use `initializer_no_metadata` in the constructor.
        /// Failure to abide by these instructions can lead to unexpected issues especially with
        /// UIs.
        fn initializer(
            ref self: ComponentState<TContractState>,
            name: ByteArray,
            symbol: ByteArray,
            base_uri: ByteArray,
        ) {
            self.ERC721_name.write(name);
            self.ERC721_symbol.write(symbol);
            self._set_base_uri(base_uri);

            let mut src5_component = get_dep_component_mut!(ref self, SRC5);
            src5_component.register_interface(interface::IERC721_ID);
            src5_component.register_interface(interface::IERC721_METADATA_ID);
        }

        /// Initializes the contract with no metadata by registering only the IERC721 interface.
        ///
        /// WARNING: This initializer should ONLY be used during construction in the very
        /// specific instance when the contract does NOT expose the IERC721Metadata interface.
        /// Initializing a contract with this initializer means that tokens will not
        /// have a name, symbol, or URI.
        fn initializer_no_metadata(ref self: ComponentState<TContractState>) {
            let mut src5_component = get_dep_component_mut!(ref self, SRC5);
            src5_component.register_interface(interface::IERC721_ID);
        }

        /// Returns whether `token_id` exists.
        fn exists(self: @ComponentState<TContractState>, token_id: u256) -> bool {
            !self._owner_of(token_id).is_zero()
        }

        /// Transfers `token_id` from `from` to `to`.
        ///
        /// Internal function without access restriction.
        ///
        /// WARNING: This method may lead to the loss of tokens if `to` is not aware of the ERC721
        /// protocol.
        ///
        /// Requirements:
        ///
        /// - `to` is not the zero address.
        /// - `from` is the token owner.
        /// - `token_id` exists.
        ///
        /// Emits a `Transfer` event.
        fn transfer(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            to: ContractAddress,
            token_id: u256,
        ) {
            assert(!to.is_zero(), Errors::INVALID_RECEIVER);

            let previous_owner = self.update(to, token_id, Zero::zero());

            assert(!previous_owner.is_zero(), Errors::INVALID_TOKEN_ID);
            assert(from == previous_owner, Errors::INVALID_SENDER);
        }

        /// Mints `token_id` and transfers it to `to`.
        /// Internal function without access restriction.
        ///
        /// WARNING: This method may lead to the loss of tokens if `to` is not aware of the ERC721
        /// protocol.
        ///
        /// Requirements:
        ///
        /// - `to` is not the zero address.
        /// - `token_id` does not exist.
        ///
        /// Emits a `Transfer` event.
        fn mint(ref self: ComponentState<TContractState>, to: ContractAddress, token_id: u256) {
            assert(!to.is_zero(), Errors::INVALID_RECEIVER);

            let previous_owner = self.update(to, token_id, Zero::zero());

            assert(previous_owner.is_zero(), Errors::ALREADY_MINTED);
        }

        /// Transfers ownership of `token_id` from `from` if `to` is either an account or
        /// `IERC721Receiver`.
        ///
        /// `data` is additional data, it has no specified format and it is sent in call to `to`.
        ///
        /// WARNING: This method makes an external call to the recipient contract, which can lead to
        /// reentrancy vulnerabilities.
        ///
        /// Requirements:
        ///
        /// - `to` cannot be the zero address.
        /// - `from` must be the token owner.
        /// - `token_id` exists.
        /// - `to` is either an account contract or supports the `IERC721Receiver` interface.
        ///
        /// Emits a `Transfer` event.
        fn safe_transfer(
            ref self: ComponentState<TContractState>,
            from: ContractAddress,
            to: ContractAddress,
            token_id: u256,
            data: Span<felt252>,
        ) {
            self.transfer(from, to, token_id);
            assert(
                _check_on_erc721_received(from, to, token_id, data), Errors::SAFE_TRANSFER_FAILED,
            );
        }

        /// Mints `token_id` if `to` is either an account or `IERC721Receiver`.
        ///
        /// `data` is additional data, it has no specified format and it is sent in call to `to`.
        ///
        /// WARNING: This method makes an external call to the recipient contract, which can lead to
        /// reentrancy vulnerabilities.
        ///
        /// Requirements:
        ///
        /// - `token_id` does not exist.
        /// - `to` is either an account contract or supports the `IERC721Receiver` interface.
        ///
        /// Emits a `Transfer` event.
        fn safe_mint(
            ref self: ComponentState<TContractState>,
            to: ContractAddress,
            token_id: u256,
            data: Span<felt252>,
        ) {
            self.mint(to, token_id);
            assert(
                _check_on_erc721_received(Zero::zero(), to, token_id, data),
                Errors::SAFE_MINT_FAILED,
            );
        }

        /// Destroys `token_id`. The approval is cleared when the token is burned.
        ///
        /// This internal function does not check if the caller is authorized
        /// to operate on the token.
        ///
        /// Requirements:
        ///
        /// - `token_id` exists.
        ///
        /// Emits a `Transfer` event.
        fn burn(ref self: ComponentState<TContractState>, token_id: u256) {
            let previous_owner = self.update(Zero::zero(), token_id, Zero::zero());
            assert(!previous_owner.is_zero(), Errors::INVALID_TOKEN_ID);
        }

        /// Transfers `token_id` from its current owner to `to`, or alternatively mints (or burns)
        /// if the current owner (or `to`) is the zero address. Returns the owner of the `token_id`
        /// before the update.
        ///
        /// The `auth` argument is optional. If the value passed is non-zero, then this function
        /// will check that `auth` is either the owner of the token, or approved to operate on the
        /// token (by the owner).
        ///
        /// Emits a `Transfer` event.
        ///
        /// NOTE: This function can be extended using the `ERC721HooksTrait`, to add
        /// functionality before and/or after the transfer, mint, or burn.
        ///
        fn update(
            ref self: ComponentState<TContractState>,
            to: ContractAddress,
            token_id: u256,
            auth: ContractAddress,
        ) -> ContractAddress {
            Hooks::before_update(ref self, to, token_id, auth);

            let from = self._owner_of(token_id);

            // Perform (optional) operator check
            if !auth.is_zero() {
                self._check_authorized(from, auth, token_id);
            }
            if !from.is_zero() {
                let zero_address = Zero::zero();
                self._approve_with_optional_event(zero_address, token_id, zero_address, false);

                self.ERC721_balances.write(from, self.ERC721_balances.read(from) - 1);
            }
            if !to.is_zero() {
                self.ERC721_balances.write(to, self.ERC721_balances.read(to) + 1);
            }

            self.ERC721_owners.write(token_id, to);
            self.emit(Transfer { from, to, token_id });

            Hooks::after_update(ref self, to, token_id, auth);

            from
        }

        /// Returns the owner address of `token_id`.
        fn _owner_of(self: @ComponentState<TContractState>, token_id: u256) -> ContractAddress {
            self.ERC721_owners.read(token_id)
        }

        /// Returns the owner address of `token_id`.
        ///
        /// Requirements:
        ///
        /// - `token_id` exists.
        fn _require_owned(
            self: @ComponentState<TContractState>, token_id: u256,
        ) -> ContractAddress {
            let owner = self._owner_of(token_id);
            assert(!owner.is_zero(), Errors::INVALID_TOKEN_ID);
            owner
        }

        /// Approve `to` to operate on `token_id`
        ///
        /// The `auth` argument is optional. If the value passed is non-zero, then this function
        /// will check that `auth` is either the owner of the token, or approved to operate on all
        /// tokens held by this owner.
        ///
        /// Emits an `Approval` event.
        fn _approve(
            ref self: ComponentState<TContractState>,
            to: ContractAddress,
            token_id: u256,
            auth: ContractAddress,
        ) {
            self._approve_with_optional_event(to, token_id, auth, true);
        }

        /// Variant of `_approve` with an optional flag to enable or disable the `Approval` event.
        /// The event is not emitted in the context of transfers.
        ///
        /// WARNING: If `auth` is zero and `emit_event` is false, this function will not check that
        /// the token exists.
        ///
        /// Requirements:
        ///
        /// - If `auth` is non-zero, it must be either the owner of the token or approved to
        /// operate on all of its tokens.
        ///
        /// May emit an `Approval` event.
        fn _approve_with_optional_event(
            ref self: ComponentState<TContractState>,
            to: ContractAddress,
            token_id: u256,
            auth: ContractAddress,
            emit_event: bool,
        ) {
            if emit_event || !auth.is_zero() {
                let owner = self._require_owned(token_id);

                if !auth.is_zero() && owner != auth {
                    let is_approved_for_all = ERC721::is_approved_for_all(@self, owner, auth);
                    assert(is_approved_for_all, Errors::UNAUTHORIZED);
                }

                if emit_event {
                    self.emit(Approval { owner, approved: to, token_id });
                }
            }

            self.ERC721_token_approvals.write(token_id, to);
        }

        /// Enables or disables approval for `operator` to manage
        /// all of the `owner` assets.
        ///
        /// Requirements:
        ///
        /// - `operator` is not the zero address.
        ///
        /// Emits an `Approval` event.
        fn _set_approval_for_all(
            ref self: ComponentState<TContractState>,
            owner: ContractAddress,
            operator: ContractAddress,
            approved: bool,
        ) {
            assert(!operator.is_zero(), Errors::INVALID_OPERATOR);
            self.ERC721_operator_approvals.write((owner, operator), approved);
            self.emit(ApprovalForAll { owner, operator, approved });
        }

        /// Sets the base URI.
        fn _set_base_uri(ref self: ComponentState<TContractState>, base_uri: ByteArray) {
            self.ERC721_base_uri.write(base_uri);
        }

        /// Base URI for computing `token_uri`.
        ///
        /// If set, the resulting URI for each token will be the concatenation of the base URI and
        /// the token ID.
        /// Returns an empty `ByteArray` if not set.
        fn _base_uri(self: @ComponentState<TContractState>) -> ByteArray {
            self.ERC721_base_uri.read()
        }

        /// Returns whether `spender` is allowed to manage `owner`'s tokens, or `token_id` in
        /// particular (ignoring whether it is owned by `owner`).
        ///
        /// WARNING: This function assumes that `owner` is the actual owner of `token_id` and does
        /// not verify this assumption.
        fn _is_authorized(
            self: @ComponentState<TContractState>,
            owner: ContractAddress,
            spender: ContractAddress,
            token_id: u256,
        ) -> bool {
            let is_approved_for_all = ERC721::is_approved_for_all(self, owner, spender);

            !spender.is_zero()
                && (owner == spender
                    || is_approved_for_all
                    || spender == ERC721::get_approved(self, token_id))
        }

        /// Checks if `spender` can operate on `token_id`, assuming the provided `owner` is the
        /// actual owner.
        ///
        /// Requirements:
        ///
        /// - `owner` cannot be the zero address.
        /// - `spender` cannot be the zero address.
        /// - `spender` must be the owner of `token_id` or be approved to operate on it.
        ///
        /// WARNING: This function assumes that `owner` is the actual owner of `token_id` and does
        /// not verify this assumption.
        fn _check_authorized(
            self: @ComponentState<TContractState>,
            owner: ContractAddress,
            spender: ContractAddress,
            token_id: u256,
        ) {
            // Non-existent token
            assert(!owner.is_zero(), Errors::INVALID_TOKEN_ID);
            assert(self._is_authorized(owner, spender, token_id), Errors::UNAUTHORIZED);
        }
    }

    /// Checks if `to` either is an account contract or has registered support
    /// for the `IERC721Receiver` interface through SRC5.
    fn _check_on_erc721_received(
        from: ContractAddress, to: ContractAddress, token_id: u256, data: Span<felt252>,
    ) -> bool {
        let src5_dispatcher = ISRC5Dispatcher { contract_address: to };

        if src5_dispatcher.supports_interface(interface::IERC721_RECEIVER_ID) {
            IERC721ReceiverDispatcher { contract_address: to }
                .on_erc721_received(
                    get_caller_address(), from, token_id, data,
                ) == interface::IERC721_RECEIVER_ID
        } else {
            src5_dispatcher.supports_interface(openzeppelin_account::interface::ISRC6_ID)
        }
    }
}

/// An empty implementation of the ERC721 hooks to be used in basic ERC721 preset contracts.
pub impl ERC721HooksEmptyImpl<
    TContractState,
> of ERC721Component::ERC721HooksTrait<TContractState> {}
