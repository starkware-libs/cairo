// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0
// (token/src/erc20/extensions/erc4626/erc4626.cairo)

/// # ERC4626 Component
///
/// The ERC4626 component is an extension of ERC20 and provides an implementation of the IERC4626
/// interface which allows the minting and burning of "shares" in exchange for an underlying
/// "asset". The component leverages traits to configure fees, limits, and decimals.
///
/// CAUTION: In empty (or nearly empty) ERC-4626 vaults, deposits are at high risk of being stolen
/// through frontrunning with a "donation" to the vault that inflates the price of a share. This is
/// variously known as a donation or inflation attack and is essentially a problem of slippage.
/// Vault deployers can protect against this attack by making an initial deposit of a non-trivial
/// amount of the asset, such that price manipulation becomes infeasible. Withdrawals may similarly
/// be affected by slippage. Users can protect against this attack as well as unexpected slippage in
/// general by verifying the amount received is as expected, using a wrapper that performs these
/// checks.
///
/// This implementation offers configurable virtual assets and shares to help developers mitigate
/// that risk. `ImmutableConfig::DECIMALS_OFFSET` corresponds to an offset in the decimal
/// representation between the underlying asset's decimals and vault decimals. This offset also
/// determines the rate of virtual shares to virtual assets in the vault, which itself determines
/// the initial exchange rate. While not fully preventing the attack, analysis shows that the
/// default offset (0) makes it non-profitable even if an attacker is able to capture value from
/// multiple user deposits, as a result of the value being captured by the virtual shares (out of
/// the attacker's donation) matching the attacker's expected gains. With a larger offset, the
/// attack becomes orders of magnitude more expensive than it is profitable.
///
/// The drawback of this approach is that the virtual shares do capture (a very small) part of the
/// value being accrued to the vault. Also, if the vault experiences losses and users try to exit
/// the vault, the virtual shares and assets will cause the first exiting user to experience reduced
/// losses to the detriment to the last users who will experience bigger losses.
#[starknet::component]
pub mod ERC4626Component {
    use core::num::traits::{Bounded, Pow, Zero};
    use openzeppelin_utils::math;
    use openzeppelin_utils::math::Rounding;
    use starknet::ContractAddress;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use crate::erc20::ERC20Component;
    use crate::erc20::ERC20Component::InternalImpl as ERC20InternalImpl;
    use crate::erc20::extensions::erc4626::interface::IERC4626;
    use crate::erc20::interface::{IERC20, IERC20Dispatcher, IERC20DispatcherTrait, IERC20Metadata};

    // The default values are only used when the DefaultConfig
    // is in scope in the implementing contract.
    pub const DEFAULT_UNDERLYING_DECIMALS: u8 = 18;
    pub const DEFAULT_DECIMALS_OFFSET: u8 = 0;

    #[storage]
    pub struct Storage {
        pub ERC4626_asset: ContractAddress,
    }

    #[event]
    #[derive(Drop, PartialEq, starknet::Event)]
    pub enum Event {
        Deposit: Deposit,
        Withdraw: Withdraw,
    }

    /// Emitted when `sender` exchanges `assets` for `shares` and transfers those
    /// `shares` to `owner`.
    #[derive(Drop, PartialEq, starknet::Event)]
    pub struct Deposit {
        #[key]
        pub sender: ContractAddress,
        #[key]
        pub owner: ContractAddress,
        pub assets: u256,
        pub shares: u256,
    }

    /// Emitted when `sender` exchanges `shares`, owned by `owner`, for `assets` and transfers
    /// those `assets` to `receiver`.
    #[derive(Drop, PartialEq, starknet::Event)]
    pub struct Withdraw {
        #[key]
        pub sender: ContractAddress,
        #[key]
        pub receiver: ContractAddress,
        #[key]
        pub owner: ContractAddress,
        pub assets: u256,
        pub shares: u256,
    }

    pub mod Errors {
        pub const EXCEEDED_MAX_DEPOSIT: felt252 = 'ERC4626: exceeds max deposit';
        pub const EXCEEDED_MAX_MINT: felt252 = 'ERC4626: exceeds max mint';
        pub const EXCEEDED_MAX_WITHDRAW: felt252 = 'ERC4626: exceeds max withdraw';
        pub const EXCEEDED_MAX_REDEEM: felt252 = 'ERC4626: exceeds max redeem';
        pub const TOKEN_TRANSFER_FAILED: felt252 = 'ERC4626: token transfer failed';
        pub const INVALID_ASSET_ADDRESS: felt252 = 'ERC4626: asset address set to 0';
        pub const DECIMALS_OVERFLOW: felt252 = 'ERC4626: decimals overflow';
    }

    /// Constants expected to be defined at the contract level which configure virtual
    /// assets and shares.
    ///
    /// `UNDERLYING_DECIMALS` should match the underlying asset's decimals. The default
    /// value is `18`.
    ///
    /// `DECIMALS_OFFSET` corresponds to the representational offset between `UNDERLYING_DECIMALS`
    /// and the vault decimals. The greater the offset, the more expensive it is for attackers to
    /// execute an inflation attack.
    ///
    /// Requirements:
    ///
    /// - `UNDERLYING_DECIMALS`  + `DECIMALS_OFFSET` cannot exceed 255 (max u8).
    pub trait ImmutableConfig {
        const UNDERLYING_DECIMALS: u8;
        const DECIMALS_OFFSET: u8;

        fn validate() {
            assert(
                Bounded::MAX - Self::UNDERLYING_DECIMALS >= Self::DECIMALS_OFFSET,
                Errors::DECIMALS_OVERFLOW,
            )
        }
    }

    /// Adjustments for fees expected to be defined at the contract level.
    /// Defaults to no entry or exit fees.
    ///
    /// NOTE: The FeeConfigTrait hooks directly into the preview methods of the ERC4626 component.
    /// The preview methods must return as close to the exact amount of shares or assets as possible
    /// if the actual (previewed) operation occurred in the same transaction (according to EIP-4626
    /// spec).
    /// All operations use their corresponding preview method as the value of assets or shares being
    /// moved.
    /// Therefore, adjusting an operation's assets in FeeConfigTrait consequently adjusts the assets
    /// (or assets to be converted into shares) in both the preview operation and the actual
    /// operation.
    ///
    /// NOTE: To transfer fees, this trait needs to be coordinated with
    /// `ERC4626Component::ERC4626Hooks`.
    /// See the ERC4626FeesMock example:
    /// https://github.com/OpenZeppelin/cairo-contracts/tree/main/packages/test_common/src/mocks/erc4626.cairo
    pub trait FeeConfigTrait<TContractState, +HasComponent<TContractState>> {
        /// Adjusts deposits within `preview_deposit` to account for entry fees.
        /// Entry fees should be transferred in the `after_deposit` hook.
        fn adjust_deposit(self: @ComponentState<TContractState>, assets: u256) -> u256 {
            assets
        }

        /// Adjusts mints within `preview_mint` to account for entry fees.
        /// Entry fees should be transferred in the `after_deposit` hook.
        fn adjust_mint(self: @ComponentState<TContractState>, assets: u256) -> u256 {
            assets
        }

        /// Adjusts withdraws within `preview_withdraw` to account for exit fees.
        /// Exit fees should be transferred in the `before_withdraw` hook.
        fn adjust_withdraw(self: @ComponentState<TContractState>, assets: u256) -> u256 {
            assets
        }

        /// Adjusts redeems within `preview_redeem` to account for exit fees.
        /// Exit fees should be transferred in the `before_withdraw` hook.
        fn adjust_redeem(self: @ComponentState<TContractState>, assets: u256) -> u256 {
            assets
        }
    }

    /// Sets limits to the target exchange type and is expected to be defined at the contract
    /// level.
    ///
    /// It's important to note that these limits correspond directly to the `max_<OPERATION>`
    /// i.e. `deposit_limit` -> `max_deposit`.
    ///
    /// The EIP-4626 spec states that the `max_<OPERATION>` methods must take into account all
    /// global and user-specific limits.
    /// If an operation is disabled (even temporarily), the corresponding limit MUST be `0`
    /// and MUST NOT panic.
    pub trait LimitConfigTrait<TContractState, +HasComponent<TContractState>> {
        /// The max deposit allowed.
        /// Defaults (`Option::None`) to 2 ** 256 - 1.
        fn deposit_limit(
            self: @ComponentState<TContractState>, receiver: ContractAddress,
        ) -> Option<u256> {
            Option::None
        }

        /// The max mint allowed.
        /// Defaults (`Option::None`) to 2 ** 256 - 1.
        fn mint_limit(
            self: @ComponentState<TContractState>, receiver: ContractAddress,
        ) -> Option<u256> {
            Option::None
        }

        /// The max withdraw allowed.
        /// Defaults (`Option::None`) to the full asset balance of `owner` converted from shares.
        fn withdraw_limit(
            self: @ComponentState<TContractState>, owner: ContractAddress,
        ) -> Option<u256> {
            Option::None
        }

        /// The max redeem allowed.
        /// Defaults (`Option::None`) to the full asset balance of `owner`.
        fn redeem_limit(
            self: @ComponentState<TContractState>, owner: ContractAddress,
        ) -> Option<u256> {
            Option::None
        }
    }

    /// Allows contracts to hook logic into deposit and withdraw transactions.
    /// This is where contracts can transfer fees.
    ///
    /// NOTE: ERC4626 preview methods must be inclusive of any entry or exit fees.
    /// The `AdjustFeesTrait` will adjust these values accordingly; therefore,
    /// fees must be set in the `AdjustFeesTrait` if the using contract enforces
    /// entry or exit fees.
    ///
    /// CAUTION: Special care must be taken when calling external contracts in these hooks. In
    /// that case, consider implementing reentrancy protections. For example, in the
    /// `withdraw` flow, the `withdraw_limit` is checked *before* the `before_withdraw` hook
    /// is invoked. If this hook performs a reentrant call that invokes `withdraw` again, the
    /// subsequent check on `withdraw_limit` will be done before the first withdrawal’s core logic
    /// (e.g., burning shares and transferring assets) is executed. This could
    /// lead to bypassing withdrawal constraints or draining funds.
    ///
    /// See the example:
    /// https://github.com/OpenZeppelin/cairo-contracts/tree/main/packages/test_common/src/mocks/erc4626.cairo
    pub trait ERC4626HooksTrait<TContractState, +HasComponent<TContractState>> {
        /// Hooks into `InternalImpl::_withdraw`.
        /// Executes logic before burning shares and transferring assets.
        fn before_withdraw(ref self: ComponentState<TContractState>, assets: u256, shares: u256) {}
        /// Hooks into `InternalImpl::_deposit`.
        /// Executes logic after transferring assets and minting shares.
        fn after_deposit(ref self: ComponentState<TContractState>, assets: u256, shares: u256) {}
    }

    //
    // External
    //

    #[embeddable_as(ERC4626Impl)]
    impl ERC4626<
        TContractState,
        +HasComponent<TContractState>,
        impl Fee: FeeConfigTrait<TContractState>,
        impl Limit: LimitConfigTrait<TContractState>,
        impl Hooks: ERC4626HooksTrait<TContractState>,
        impl Immutable: ImmutableConfig,
        impl ERC20: ERC20Component::HasComponent<TContractState>,
        +ERC20Component::ERC20HooksTrait<TContractState>,
        +Drop<TContractState>,
    > of IERC4626<ComponentState<TContractState>> {
        /// Returns the address of the underlying token used for the Vault for accounting,
        /// depositing, and withdrawing.
        fn asset(self: @ComponentState<TContractState>) -> ContractAddress {
            self.ERC4626_asset.read()
        }

        /// Returns the total amount of the underlying asset that is “managed” by Vault.
        fn total_assets(self: @ComponentState<TContractState>) -> u256 {
            let this = starknet::get_contract_address();
            let asset_dispatcher = IERC20Dispatcher { contract_address: self.ERC4626_asset.read() };
            asset_dispatcher.balance_of(this)
        }

        /// Returns the amount of shares that the Vault would exchange for the amount of assets
        /// provided irrespective of slippage or fees.
        ///
        /// NOTE: As per the EIP-4626 spec, this may panic _only_ if there's an overflow
        /// from an unreasonably large input.
        fn convert_to_shares(self: @ComponentState<TContractState>, assets: u256) -> u256 {
            self._convert_to_shares(assets, Rounding::Floor)
        }

        /// Returns the amount of assets that the Vault would exchange for the amount of shares
        /// provided irrespective of slippage or fees.
        ///
        /// NOTE: As per the EIP-4626 spec, this may panic _only_ if there's an overflow
        /// from an unreasonably large input.
        fn convert_to_assets(self: @ComponentState<TContractState>, shares: u256) -> u256 {
            self._convert_to_assets(shares, Rounding::Floor)
        }

        /// Returns the maximum amount of the underlying asset that can be deposited into the Vault
        /// for the receiver, through a deposit call.
        ///
        /// The default max deposit value is 2 ** 256 - 1.
        /// This can be changed in the implementing contract by defining custom logic in
        /// `LimitConfigTrait::deposit_limit`.
        fn max_deposit(self: @ComponentState<TContractState>, receiver: ContractAddress) -> u256 {
            match Limit::deposit_limit(self, receiver) {
                Option::Some(limit) => limit,
                Option::None => Bounded::MAX,
            }
        }

        /// Allows an on-chain or off-chain user to simulate the effects of their deposit at the
        /// current block, given current on-chain conditions.
        ///
        /// The default deposit preview value is the full amount of shares.
        /// This can be changed to account for fees, for example, in the implementing contract by
        /// defining custom logic in `FeeConfigTrait::adjust_deposit`.
        ///
        /// NOTE: `preview_deposit` must be inclusive of entry fees to be compliant with the
        /// EIP-4626 spec.
        fn preview_deposit(self: @ComponentState<TContractState>, assets: u256) -> u256 {
            let adjusted_assets = Fee::adjust_deposit(self, assets);
            self._convert_to_shares(adjusted_assets, Rounding::Floor)
        }

        /// Mints Vault shares to `receiver` by depositing exactly `assets` of underlying tokens.
        /// Returns the amount of newly-minted shares.
        ///
        /// Requirements:
        ///
        /// - `assets` is less than or equal to the max deposit amount for `receiver`.
        ///
        /// Emits a `Deposit` event.
        fn deposit(
            ref self: ComponentState<TContractState>, assets: u256, receiver: ContractAddress,
        ) -> u256 {
            let max_assets = self.max_deposit(receiver);
            assert(assets <= max_assets, Errors::EXCEEDED_MAX_DEPOSIT);

            let shares = self.preview_deposit(assets);
            let caller = starknet::get_caller_address();
            self._deposit(caller, receiver, assets, shares);

            shares
        }

        /// Returns the maximum amount of the Vault shares that can be minted for `receiver` through
        /// a `mint` call.
        ///
        /// The default max mint value is 2 ** 256 - 1.
        /// This can be changed in the implementing contract by defining custom logic in
        /// `LimitConfigTrait::mint_limit`.
        fn max_mint(self: @ComponentState<TContractState>, receiver: ContractAddress) -> u256 {
            match Limit::mint_limit(self, receiver) {
                Option::Some(limit) => limit,
                Option::None => Bounded::MAX,
            }
        }

        /// Allows an on-chain or off-chain user to simulate the effects of their mint at the
        /// current block, given current on-chain conditions.
        ///
        /// The default mint preview value is the full amount of assets.
        /// This can be changed to account for fees, for example, in the implementing contract by
        /// defining custom logic in `FeeConfigTrait::adjust_mint`.
        ///
        /// NOTE: `preview_mint` must be inclusive of entry fees to be compliant with the EIP-4626
        /// spec.
        fn preview_mint(self: @ComponentState<TContractState>, shares: u256) -> u256 {
            let full_assets = self._convert_to_assets(shares, Rounding::Ceil);
            Fee::adjust_mint(self, full_assets)
        }

        /// Mints exactly Vault `shares` to `receiver` by depositing amount of underlying tokens.
        /// Returns the amount deposited assets.
        ///
        /// Requirements:
        ///
        /// - `shares` is less than or equal to the max shares amount for `receiver`.
        ///
        /// Emits a `Deposit` event.
        fn mint(
            ref self: ComponentState<TContractState>, shares: u256, receiver: ContractAddress,
        ) -> u256 {
            let max_shares = self.max_mint(receiver);
            assert(shares <= max_shares, Errors::EXCEEDED_MAX_MINT);

            let assets = self.preview_mint(shares);
            let caller = starknet::get_caller_address();
            self._deposit(caller, receiver, assets, shares);

            assets
        }

        /// Returns the maximum amount of the underlying asset that can be withdrawn from the owner
        /// balance in the Vault, through a `withdraw` call.
        ///
        /// The default max withdraw value is the full balance of assets for `owner` (converted from
        /// shares).
        /// This can be changed in the implementing contract by defining custom logic in
        /// `LimitConfigTrait::withdraw_limit`.
        /// Do note that with customized limits, the maximum withdraw amount will either be
        /// the custom limit itself or ``owner``'s total asset balance, whichever value is less.
        fn max_withdraw(self: @ComponentState<TContractState>, owner: ContractAddress) -> u256 {
            let erc20_component = get_dep_component!(self, ERC20);
            let owner_shares = erc20_component.balance_of(owner);
            let total_owner_assets = self._convert_to_assets(owner_shares, Rounding::Floor);

            match Limit::withdraw_limit(self, owner) {
                Option::Some(limit) => {
                    if total_owner_assets < limit {
                        total_owner_assets
                    } else {
                        limit
                    }
                },
                Option::None => { total_owner_assets },
            }
        }

        /// Allows an on-chain or off-chain user to simulate the effects of their withdrawal at the
        /// current block, given current on-chain conditions.
        ///
        /// The default withdraw preview value is the full amount of shares.
        /// This can be changed to account for fees, for example, in the implementing contract by
        /// defining custom logic in `FeeConfigTrait::adjust_withdraw`.
        ///
        /// NOTE: `preview_withdraw` must be inclusive of exit fees to be compliant with the
        /// EIP-4626 spec.
        fn preview_withdraw(self: @ComponentState<TContractState>, assets: u256) -> u256 {
            let adjusted_assets = Fee::adjust_withdraw(self, assets);
            self._convert_to_shares(adjusted_assets, Rounding::Ceil)
        }

        /// Burns shares from `owner` and sends exactly `assets` of underlying tokens to `receiver`.
        ///
        /// Requirements:
        ///
        /// - `assets` is less than or equal to the max withdraw amount of `owner`.
        ///
        /// Emits a `Withdraw` event.
        fn withdraw(
            ref self: ComponentState<TContractState>,
            assets: u256,
            receiver: ContractAddress,
            owner: ContractAddress,
        ) -> u256 {
            let max_assets = self.max_withdraw(owner);
            assert(assets <= max_assets, Errors::EXCEEDED_MAX_WITHDRAW);

            let shares = self.preview_withdraw(assets);
            let caller = starknet::get_caller_address();
            self._withdraw(caller, receiver, owner, assets, shares);

            shares
        }

        /// Returns the maximum amount of Vault shares that can be redeemed from the owner balance
        /// in the Vault, through a `redeem` call.
        ///
        /// The default max redeem value is the full balance of assets for `owner`.
        /// This can be changed in the implementing contract by defining custom logic in
        /// `LimitConfigTrait::redeem_limit`.
        /// Do note that with customized limits, the maximum redeem amount will either be
        /// the custom limit itself or ``owner``'s total asset balance, whichever value is less.
        fn max_redeem(self: @ComponentState<TContractState>, owner: ContractAddress) -> u256 {
            let erc20_component = get_dep_component!(self, ERC20);
            let owner_assets = erc20_component.balance_of(owner);

            match Limit::redeem_limit(self, owner) {
                Option::Some(limit) => { if owner_assets < limit {
                    owner_assets
                } else {
                    limit
                } },
                Option::None => { owner_assets },
            }
        }

        /// Allows an on-chain or off-chain user to simulate the effects of their redemption at the
        /// current block, given current on-chain conditions.
        ///
        /// The default redeem preview value is the full amount of assets.
        /// This can be changed to account for fees, for example, in the implementing contract by
        /// defining custom logic in `FeeConfigTrait::adjust_redeem`.
        ///
        /// NOTE: `preview_redeem` must be inclusive of exit fees to be compliant with the EIP-4626
        /// spec.
        fn preview_redeem(self: @ComponentState<TContractState>, shares: u256) -> u256 {
            let full_assets = self._convert_to_assets(shares, Rounding::Floor);
            Fee::adjust_redeem(self, full_assets)
        }

        /// Burns exactly `shares` from `owner` and sends assets of underlying tokens to `receiver`.
        ///
        /// Requirements:
        ///
        /// - `shares` is less than or equal to the max redeem amount of `owner`.
        ///
        /// Emits a `Withdraw` event.
        fn redeem(
            ref self: ComponentState<TContractState>,
            shares: u256,
            receiver: ContractAddress,
            owner: ContractAddress,
        ) -> u256 {
            let max_shares = self.max_redeem(owner);
            assert(shares <= max_shares, Errors::EXCEEDED_MAX_REDEEM);

            let assets = self.preview_redeem(shares);
            let caller = starknet::get_caller_address();
            self._withdraw(caller, receiver, owner, assets, shares);

            assets
        }
    }

    #[embeddable_as(ERC4626MetadataImpl)]
    impl ERC4626Metadata<
        TContractState,
        +HasComponent<TContractState>,
        impl Immutable: ImmutableConfig,
        impl ERC20: ERC20Component::HasComponent<TContractState>,
    > of IERC20Metadata<ComponentState<TContractState>> {
        /// Returns the name of the token.
        fn name(self: @ComponentState<TContractState>) -> ByteArray {
            let erc20_component = get_dep_component!(self, ERC20);
            erc20_component.ERC20_name.read()
        }

        /// Returns the ticker symbol of the token, usually a shorter version of the name.
        fn symbol(self: @ComponentState<TContractState>) -> ByteArray {
            let erc20_component = get_dep_component!(self, ERC20);
            erc20_component.ERC20_symbol.read()
        }

        /// Returns the cumulative number of decimals which includes both the underlying and offset
        /// decimals.
        /// Both of which must be defined in the `ImmutableConfig` inside the implementing contract.
        fn decimals(self: @ComponentState<TContractState>) -> u8 {
            Immutable::UNDERLYING_DECIMALS + Immutable::DECIMALS_OFFSET
        }
    }

    //
    // Internal
    //

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        impl Hooks: ERC4626HooksTrait<TContractState>,
        impl Immutable: ImmutableConfig,
        impl ERC20: ERC20Component::HasComponent<TContractState>,
        +FeeConfigTrait<TContractState>,
        +LimitConfigTrait<TContractState>,
        +ERC20Component::ERC20HooksTrait<TContractState>,
        +Drop<TContractState>,
    > of InternalTrait<TContractState> {
        /// Validates the `ImmutableConfig` constants and sets the `asset_address` to the vault.
        /// This should be set in the contract's constructor.
        ///
        /// Requirements:
        ///
        /// - `asset_address` cannot be the zero address.
        fn initializer(ref self: ComponentState<TContractState>, asset_address: ContractAddress) {
            Immutable::validate();
            assert(asset_address.is_non_zero(), Errors::INVALID_ASSET_ADDRESS);
            self.ERC4626_asset.write(asset_address);
        }

        /// Internal logic for `deposit` and `mint`.
        /// Transfers `assets` from `caller` to the Vault contract then mints `shares` to
        /// `receiver`.
        /// Fees can be transferred in the `ERC4626Hooks::after_deposit` hook which is executed
        /// after assets are transferred and shares are minted.
        ///
        /// Requirements:
        ///
        /// - `ERC20::transfer_from` must return true.
        ///
        /// Emits two `ERC20::Transfer` events (`ERC20::mint` and `ERC20::transfer_from`).
        /// Emits a `Deposit` event.
        fn _deposit(
            ref self: ComponentState<TContractState>,
            caller: ContractAddress,
            receiver: ContractAddress,
            assets: u256,
            shares: u256,
        ) {
            // Transfer assets first
            let this = starknet::get_contract_address();
            let asset_dispatcher = IERC20Dispatcher { contract_address: self.ERC4626_asset.read() };
            assert(
                asset_dispatcher.transfer_from(caller, this, assets), Errors::TOKEN_TRANSFER_FAILED,
            );

            // Mint shares after transferring assets
            let mut erc20_component = get_dep_component_mut!(ref self, ERC20);
            erc20_component.mint(receiver, shares);
            self.emit(Deposit { sender: caller, owner: receiver, assets, shares });

            // After deposit hook
            Hooks::after_deposit(ref self, assets, shares);
        }

        /// Internal logic for `withdraw` and `redeem`.
        /// Burns `shares` from `owner` and then transfers `assets` to `receiver`.
        /// Fees can be transferred in the `ERC4626Hooks::before_withdraw` hook which is executed
        /// before shares are burned and assets are transferred.
        ///
        /// Requirements:
        ///
        /// - `ERC20::transfer` must return true.
        ///
        /// Emits two `ERC20::Transfer` events (`ERC20::burn` and `ERC20::transfer`).
        ///
        /// Emits a `Withdraw` event.
        fn _withdraw(
            ref self: ComponentState<TContractState>,
            caller: ContractAddress,
            receiver: ContractAddress,
            owner: ContractAddress,
            assets: u256,
            shares: u256,
        ) {
            // Before withdraw hook
            Hooks::before_withdraw(ref self, assets, shares);

            // Burn shares first
            let mut erc20_component = get_dep_component_mut!(ref self, ERC20);
            if caller != owner {
                erc20_component._spend_allowance(owner, caller, shares);
            }
            erc20_component.burn(owner, shares);

            // Transfer assets after burn
            let asset_dispatcher = IERC20Dispatcher { contract_address: self.ERC4626_asset.read() };
            assert(asset_dispatcher.transfer(receiver, assets), Errors::TOKEN_TRANSFER_FAILED);

            self.emit(Withdraw { sender: caller, receiver, owner, assets, shares });
        }

        /// Internal conversion function (from assets to shares) with support for `rounding`
        /// direction.
        fn _convert_to_shares(
            self: @ComponentState<TContractState>, assets: u256, rounding: Rounding,
        ) -> u256 {
            let erc20_component = get_dep_component!(self, ERC20);
            let total_supply = erc20_component.total_supply();

            math::u256_mul_div(
                assets,
                total_supply + 10_u256.pow(Immutable::DECIMALS_OFFSET.into()),
                self.total_assets() + 1,
                rounding,
            )
        }

        /// Internal conversion function (from shares to assets) with support for `rounding`
        /// direction.
        fn _convert_to_assets(
            self: @ComponentState<TContractState>, shares: u256, rounding: Rounding,
        ) -> u256 {
            let erc20_component = get_dep_component!(self, ERC20);
            let total_supply = erc20_component.total_supply();

            math::u256_mul_div(
                shares,
                self.total_assets() + 1,
                total_supply + 10_u256.pow(Immutable::DECIMALS_OFFSET.into()),
                rounding,
            )
        }
    }
}

//
// Default (empty) traits
//

pub impl ERC4626HooksEmptyImpl<
    TContractState, +ERC4626Component::HasComponent<TContractState>,
> of ERC4626Component::ERC4626HooksTrait<TContractState> {}

pub impl ERC4626DefaultNoFees<
    TContractState, +ERC4626Component::HasComponent<TContractState>,
> of ERC4626Component::FeeConfigTrait<TContractState> {}

pub impl ERC4626DefaultLimits<
    TContractState, +ERC4626Component::HasComponent<TContractState>,
> of ERC4626Component::LimitConfigTrait<TContractState> {}

/// Implementation of the default `ERC4626Component::ImmutableConfig`.
///
/// See
/// https://github.com/starknet-io/SNIPs/blob/963848f0752bde75c7087c2446d83b7da8118b25/SNIPS/snip-107.md#defaultconfig-implementation
///
/// The default `UNDERLYING_DECIMALS` is set to `18`.
/// The default `DECIMALS_OFFSET` is set to `0`.
pub impl DefaultConfig of ERC4626Component::ImmutableConfig {
    const UNDERLYING_DECIMALS: u8 = ERC4626Component::DEFAULT_UNDERLYING_DECIMALS;
    const DECIMALS_OFFSET: u8 = ERC4626Component::DEFAULT_DECIMALS_OFFSET;
}

#[cfg(test)]
mod Test {
    use openzeppelin_test_common::mocks::erc4626::ERC4626Mock;
    use super::ERC4626Component::InternalImpl;
    use super::{ERC4626Component, ERC4626DefaultLimits, ERC4626DefaultNoFees};

    type ComponentState = ERC4626Component::ComponentState<ERC4626Mock::ContractState>;

    fn COMPONENT_STATE() -> ComponentState {
        ERC4626Component::component_state_for_testing()
    }

    // Invalid decimals
    impl InvalidImmutableConfig of ERC4626Component::ImmutableConfig {
        const UNDERLYING_DECIMALS: u8 = 255;
        const DECIMALS_OFFSET: u8 = 1;
    }

    #[test]
    #[should_panic(expected: 'ERC4626: decimals overflow')]
    fn test_initializer_invalid_config_panics() {
        let mut state = COMPONENT_STATE();
        let asset = 'ASSET'.try_into().unwrap();

        state.initializer(asset);
    }
}
