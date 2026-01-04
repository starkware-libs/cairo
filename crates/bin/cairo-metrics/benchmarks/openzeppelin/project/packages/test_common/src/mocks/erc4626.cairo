#[starknet::contract]
#[with_components(ERC20, ERC4626)]
pub mod ERC4626Mock {
    use openzeppelin_token::erc20::extensions::erc4626::{
        DefaultConfig, ERC4626DefaultLimits, ERC4626DefaultNoFees, ERC4626HooksEmptyImpl,
    };
    use openzeppelin_token::erc20::{DefaultConfig as ERC20DefaultConfig, ERC20HooksEmptyImpl};
    use starknet::ContractAddress;

    // ERC4626
    #[abi(embed_v0)]
    impl ERC4626ComponentImpl = ERC4626Component::ERC4626Impl<ContractState>;
    // ERC4626MetadataImpl is a custom impl of IERC20Metadata
    #[abi(embed_v0)]
    impl ERC4626MetadataImpl = ERC4626Component::ERC4626MetadataImpl<ContractState>;

    // ERC20
    #[abi(embed_v0)]
    impl ERC20Impl = ERC20Component::ERC20Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC20CamelOnlyImpl = ERC20Component::ERC20CamelOnlyImpl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(
        ref self: ContractState,
        name: ByteArray,
        symbol: ByteArray,
        underlying_asset: ContractAddress,
        initial_supply: u256,
        recipient: ContractAddress,
    ) {
        self.erc20.initializer(name, symbol);
        self.erc20.mint(recipient, initial_supply);
        self.erc4626.initializer(underlying_asset);
    }
}

#[starknet::contract]
#[with_components(ERC20, ERC4626)]
pub mod ERC4626OffsetMock {
    use openzeppelin_token::erc20::extensions::erc4626::{
        ERC4626DefaultLimits, ERC4626DefaultNoFees, ERC4626HooksEmptyImpl,
    };
    use openzeppelin_token::erc20::{DefaultConfig, ERC20HooksEmptyImpl};
    use starknet::ContractAddress;

    // ERC4626
    #[abi(embed_v0)]
    impl ERC4626ComponentImpl = ERC4626Component::ERC4626Impl<ContractState>;
    // ERC4626MetadataImpl is a custom impl of IERC20Metadata
    #[abi(embed_v0)]
    impl ERC4626MetadataImpl = ERC4626Component::ERC4626MetadataImpl<ContractState>;

    // ERC20
    #[abi(embed_v0)]
    impl ERC20Impl = ERC20Component::ERC20Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC20CamelOnlyImpl = ERC20Component::ERC20CamelOnlyImpl<ContractState>;

    #[storage]
    pub struct Storage {}

    pub impl OffsetConfig of ERC4626Component::ImmutableConfig {
        const UNDERLYING_DECIMALS: u8 = ERC4626Component::DEFAULT_UNDERLYING_DECIMALS;
        const DECIMALS_OFFSET: u8 = 1;
    }

    #[constructor]
    fn constructor(
        ref self: ContractState,
        name: ByteArray,
        symbol: ByteArray,
        underlying_asset: ContractAddress,
        initial_supply: u256,
        recipient: ContractAddress,
    ) {
        self.erc20.initializer(name, symbol);
        self.erc20.mint(recipient, initial_supply);
        self.erc4626.initializer(underlying_asset);
    }
}

#[starknet::contract]
#[with_components(ERC20, ERC4626)]
pub mod ERC4626LimitsMock {
    use openzeppelin_token::erc20::extensions::erc4626::{
        ERC4626DefaultNoFees, ERC4626HooksEmptyImpl,
    };
    use openzeppelin_token::erc20::{DefaultConfig, ERC20HooksEmptyImpl};
    use starknet::ContractAddress;

    // ERC4626
    #[abi(embed_v0)]
    impl ERC4626ComponentImpl = ERC4626Component::ERC4626Impl<ContractState>;
    // ERC4626MetadataImpl is a custom impl of IERC20Metadata
    #[abi(embed_v0)]
    impl ERC4626MetadataImpl = ERC4626Component::ERC4626MetadataImpl<ContractState>;

    // ERC20
    #[abi(embed_v0)]
    impl ERC20Impl = ERC20Component::ERC20Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC20CamelOnlyImpl = ERC20Component::ERC20CamelOnlyImpl<ContractState>;

    #[storage]
    pub struct Storage {}

    pub impl OffsetConfig of ERC4626Component::ImmutableConfig {
        const UNDERLYING_DECIMALS: u8 = ERC4626Component::DEFAULT_UNDERLYING_DECIMALS;
        const DECIMALS_OFFSET: u8 = 1;
    }

    pub const CUSTOM_LIMIT: u256 = 100_000_000_000_000_000_000;

    impl ERC4626LimitsImpl of ERC4626Component::LimitConfigTrait<ContractState> {
        fn deposit_limit(
            self: @ERC4626Component::ComponentState<ContractState>, receiver: ContractAddress,
        ) -> Option<u256> {
            Option::Some(CUSTOM_LIMIT)
        }

        fn mint_limit(
            self: @ERC4626Component::ComponentState<ContractState>, receiver: ContractAddress,
        ) -> Option<u256> {
            Option::Some(CUSTOM_LIMIT)
        }

        fn withdraw_limit(
            self: @ERC4626Component::ComponentState<ContractState>, owner: ContractAddress,
        ) -> Option<u256> {
            Option::Some(CUSTOM_LIMIT)
        }

        fn redeem_limit(
            self: @ERC4626Component::ComponentState<ContractState>, owner: ContractAddress,
        ) -> Option<u256> {
            Option::Some(CUSTOM_LIMIT)
        }
    }

    #[constructor]
    fn constructor(
        ref self: ContractState,
        name: ByteArray,
        symbol: ByteArray,
        underlying_asset: ContractAddress,
        initial_supply: u256,
        recipient: ContractAddress,
    ) {
        self.erc20.initializer(name, symbol);
        self.erc20.mint(recipient, initial_supply);
        self.erc4626.initializer(underlying_asset);
    }
}

/// The mock contract charges fees in terms of assets, not shares.
/// This means that the fees are calculated based on the amount of assets that are being deposited
/// or withdrawn, and not based on the amount of shares that are being minted or redeemed.
/// This is an opinionated design decision for the purpose of testing.
/// DO NOT USE IN PRODUCTION
#[starknet::contract]
#[with_components(ERC20, ERC4626)]
pub mod ERC4626FeesMock {
    use openzeppelin_token::erc20::extensions::erc4626::ERC4626Component::FeeConfigTrait;
    use openzeppelin_token::erc20::extensions::erc4626::{DefaultConfig, ERC4626DefaultLimits};
    use openzeppelin_token::erc20::interface::{IERC20Dispatcher, IERC20DispatcherTrait};
    use openzeppelin_token::erc20::{DefaultConfig as ERC20DefaultConfig, ERC20HooksEmptyImpl};
    use openzeppelin_utils::math;
    use openzeppelin_utils::math::Rounding;
    use starknet::ContractAddress;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};

    // ERC4626
    #[abi(embed_v0)]
    impl ERC4626ComponentImpl = ERC4626Component::ERC4626Impl<ContractState>;
    // ERC4626MetadataImpl is a custom impl of IERC20Metadata
    #[abi(embed_v0)]
    impl ERC4626MetadataImpl = ERC4626Component::ERC4626MetadataImpl<ContractState>;

    // ERC20
    #[abi(embed_v0)]
    impl ERC20Impl = ERC20Component::ERC20Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC20CamelOnlyImpl = ERC20Component::ERC20CamelOnlyImpl<ContractState>;

    #[storage]
    pub struct Storage {
        pub entry_fee_basis_point_value: u256,
        pub entry_fee_recipient: ContractAddress,
        pub exit_fee_basis_point_value: u256,
        pub exit_fee_recipient: ContractAddress,
    }

    const _BASIS_POINT_SCALE: u256 = 10_000;

    /// Hooks
    impl ERC4626HooksEmptyImpl of ERC4626Component::ERC4626HooksTrait<ContractState> {
        fn after_deposit(
            ref self: ERC4626Component::ComponentState<ContractState>, assets: u256, shares: u256,
        ) {
            let mut contract_state = self.get_contract_mut();
            let entry_basis_points = contract_state.entry_fee_basis_point_value.read();
            let fee = contract_state.fee_on_total(assets, entry_basis_points);
            let recipient = contract_state.entry_fee_recipient.read();

            if (fee > 0 && recipient != starknet::get_contract_address()) {
                contract_state.transfer_fees(recipient, fee);
            }
        }

        fn before_withdraw(
            ref self: ERC4626Component::ComponentState<ContractState>, assets: u256, shares: u256,
        ) {
            let mut contract_state = self.get_contract_mut();
            let exit_basis_points = contract_state.exit_fee_basis_point_value.read();
            let fee = contract_state.fee_on_raw(assets, exit_basis_points);
            let recipient = contract_state.exit_fee_recipient.read();

            if (fee > 0 && recipient != starknet::get_contract_address()) {
                contract_state.transfer_fees(recipient, fee);
            }
        }
    }

    /// Adjust fees
    impl AdjustFeesImpl of FeeConfigTrait<ContractState> {
        fn adjust_deposit(
            self: @ERC4626Component::ComponentState<ContractState>, assets: u256,
        ) -> u256 {
            let contract_state = self.get_contract();
            contract_state.remove_fee_from_deposit(assets)
        }

        fn adjust_mint(
            self: @ERC4626Component::ComponentState<ContractState>, assets: u256,
        ) -> u256 {
            let contract_state = self.get_contract();
            contract_state.add_fee_to_mint(assets)
        }

        fn adjust_withdraw(
            self: @ERC4626Component::ComponentState<ContractState>, assets: u256,
        ) -> u256 {
            let contract_state = self.get_contract();
            contract_state.add_fee_to_withdraw(assets)
        }

        fn adjust_redeem(
            self: @ERC4626Component::ComponentState<ContractState>, assets: u256,
        ) -> u256 {
            let contract_state = self.get_contract();
            contract_state.remove_fee_from_redeem(assets)
        }
    }

    #[constructor]
    fn constructor(
        ref self: ContractState,
        name: ByteArray,
        symbol: ByteArray,
        underlying_asset: ContractAddress,
        initial_supply: u256,
        recipient: ContractAddress,
        entry_fee: u256,
        entry_treasury: ContractAddress,
        exit_fee: u256,
        exit_treasury: ContractAddress,
    ) {
        self.erc20.initializer(name, symbol);
        self.erc20.mint(recipient, initial_supply);
        self.erc4626.initializer(underlying_asset);

        self.entry_fee_basis_point_value.write(entry_fee);
        self.entry_fee_recipient.write(entry_treasury);
        self.exit_fee_basis_point_value.write(exit_fee);
        self.exit_fee_recipient.write(exit_treasury);
    }

    #[generate_trait]
    pub impl InternalImpl of InternalTrait {
        fn transfer_fees(ref self: ContractState, recipient: ContractAddress, fee: u256) {
            let asset_address = self.asset();
            let asset_dispatcher = IERC20Dispatcher { contract_address: asset_address };
            assert(asset_dispatcher.transfer(recipient, fee), 'Fee transfer failed');
        }

        fn remove_fee_from_deposit(self: @ContractState, assets: u256) -> u256 {
            let fee = self.fee_on_total(assets, self.entry_fee_basis_point_value.read());
            assets - fee
        }

        fn add_fee_to_mint(self: @ContractState, assets: u256) -> u256 {
            assets + self.fee_on_raw(assets, self.entry_fee_basis_point_value.read())
        }

        fn add_fee_to_withdraw(self: @ContractState, assets: u256) -> u256 {
            let fee = self.fee_on_raw(assets, self.exit_fee_basis_point_value.read());
            assets + fee
        }

        fn remove_fee_from_redeem(self: @ContractState, assets: u256) -> u256 {
            assets - self.fee_on_total(assets, self.exit_fee_basis_point_value.read())
        }

        ///
        /// Fee operations
        ///

        /// Calculates the fees that should be added to an amount `assets` that does not already
        /// include fees.
        /// Used in IERC4626::mint and IERC4626::withdraw operations.
        fn fee_on_raw(self: @ContractState, assets: u256, fee_basis_points: u256) -> u256 {
            math::u256_mul_div(assets, fee_basis_points, _BASIS_POINT_SCALE, Rounding::Ceil)
        }

        /// Calculates the fee part of an amount `assets` that already includes fees.
        /// Used in IERC4626::deposit and IERC4626::redeem operations.
        fn fee_on_total(self: @ContractState, assets: u256, fee_basis_points: u256) -> u256 {
            math::u256_mul_div(
                assets, fee_basis_points, fee_basis_points + _BASIS_POINT_SCALE, Rounding::Ceil,
            )
        }
    }
}
