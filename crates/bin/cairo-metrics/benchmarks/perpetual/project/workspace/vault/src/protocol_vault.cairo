#[starknet::contract]
pub mod ProtocolVault {
    use ERC4626Component::Fee;
    use core::num::traits::Zero;
    use openzeppelin::access::accesscontrol::AccessControlComponent;
    use openzeppelin::interfaces::erc20::{IERC20Dispatcher, IERC20DispatcherTrait};
    use openzeppelin::introspection::src5::SRC5Component;
    use openzeppelin::token::erc20::extensions::erc4626::{
        ERC4626Component, ERC4626DefaultNoFees, ERC4626DefaultNoLimits,
    };
    use openzeppelin::token::erc20::{ERC20Component, ERC20HooksEmptyImpl};
    use perpetuals::core::components::positions::interface::{
        IPositionsDispatcher, IPositionsDispatcherTrait,
    };
    use starknet::ContractAddress;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use starkware_utils::components::replaceability::ReplaceabilityComponent;
    use starkware_utils::components::replaceability::ReplaceabilityComponent::InternalReplaceabilityTrait;
    use starkware_utils::components::roles::RolesComponent;
    use starkware_utils::components::roles::RolesComponent::InternalTrait as RolesInternal;
    use starkware_utils::math::abs::Abs;
    use vault::errors::{
        INVALID_ZERO_ADDRESS, INVALID_ZERO_POSITION_ID, NEGATIVE_TOTAL_VALUE,
        ONLY_PERPS_CAN_DEPOSIT, ONLY_PERPS_CAN_OWN, ONLY_PERPS_CAN_RECEIVE, ONLY_PERPS_CAN_WITHDRAW,
    };
    use vault::interface::IProtocolVault;

    const SCALE: u64 = 1000000_u64;

    component!(path: AccessControlComponent, storage: accesscontrol, event: AccessControlEvent);
    component!(path: ReplaceabilityComponent, storage: replaceability, event: ReplaceabilityEvent);
    component!(path: RolesComponent, storage: roles, event: RolesEvent);
    component!(path: SRC5Component, storage: src5, event: SRC5Event);
    component!(path: ERC4626Component, storage: erc4626, event: ERC4626Event);
    component!(path: ERC20Component, storage: erc20, event: ERC20Event);


    #[abi(embed_v0)]
    impl ERC4626Impl = ERC4626Component::ERC4626Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC4626MetadataImpl = ERC4626Component::ERC4626MetadataImpl<ContractState>;
    #[abi(embed_v0)]
    impl ERC20Impl = ERC20Component::ERC20Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC20CamelOnlyImpl = ERC20Component::ERC20CamelOnlyImpl<ContractState>;
    #[abi(embed_v0)]
    impl ReplaceabilityImpl =
        ReplaceabilityComponent::ReplaceabilityImpl<ContractState>;
    #[abi(embed_v0)]
    impl RolesImpl = RolesComponent::RolesImpl<ContractState>;


    impl ERC4626InternalImpl = ERC4626Component::InternalImpl<ContractState>;
    impl ERC20InternalImpl = ERC20Component::InternalImpl<ContractState>;

    #[storage]
    pub struct Storage {
        #[substorage(v0)]
        accesscontrol: AccessControlComponent::Storage,
        #[substorage(v0)]
        replaceability: ReplaceabilityComponent::Storage,
        #[substorage(v0)]
        roles: RolesComponent::Storage,
        #[substorage(v0)]
        src5: SRC5Component::Storage,
        #[substorage(v0)]
        erc4626: ERC4626Component::Storage,
        #[substorage(v0)]
        erc20: ERC20Component::Storage,
        perps_contract: ContractAddress,
        owning_position_id: u32,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        #[flat]
        AccessControlEvent: AccessControlComponent::Event,
        #[flat]
        ReplaceabilityEvent: ReplaceabilityComponent::Event,
        #[flat]
        RolesEvent: RolesComponent::Event,
        #[flat]
        SRC5Event: SRC5Component::Event,
        #[flat]
        ERC4626Event: ERC4626Component::Event,
        #[flat]
        ERC20Event: ERC20Component::Event,
    }

    pub impl DecimalsConfig of ERC4626Component::ImmutableConfig {
        /// The decimals of the underlying asset. (i.e. 6 for USDC)
        const UNDERLYING_DECIMALS: u8 = 6;
        const DECIMALS_OFFSET: u8 = 0;
    }

    #[constructor]
    pub fn constructor(
        ref self: ContractState,
        governance_admin: ContractAddress,
        upgrade_delay: u64,
        name: ByteArray,
        symbol: ByteArray,
        pnl_collateral_contract: ContractAddress,
        perps_contract: ContractAddress,
        owning_position_id: u32,
        recipient: ContractAddress,
        initial_price: u64,
    ) -> u256 {
        self.roles.initialize(:governance_admin);
        self.replaceability.initialize(:upgrade_delay);

        assert(perps_contract.is_non_zero(), INVALID_ZERO_ADDRESS);
        assert(owning_position_id.is_non_zero(), INVALID_ZERO_POSITION_ID);
        self.perps_contract.write(perps_contract);
        self.owning_position_id.write(owning_position_id);
        self.erc20.initializer(name, symbol);
        self.erc4626.initializer(pnl_collateral_contract);

        let total_assets = self.erc4626.get_total_assets();
        assert(total_assets > 0_u256, 'INITIAL_ASSETS_MUST_BE_POSITIVE');
        assert(recipient != perps_contract, 'RECIPIENT_CANNOT_BE_PERPS');
        assert(initial_price.is_non_zero(), 'INVALID_ZERO_INIT_PRICE');
        let amount_to_mint = (total_assets * SCALE.into()) / initial_price.into();
        self.erc20.mint(recipient, amount_to_mint);

        return total_assets;
    }

    #[abi(embed_v0)]
    pub impl Impl of IProtocolVault<ContractState> {
        fn redeem_with_price(ref self: ContractState, shares: u256, value_of_shares: u256) -> u256 {
            let perps = self.perps_contract.read();
            self
                .erc4626
                ._withdraw(
                    caller: perps,
                    receiver: perps,
                    owner: perps,
                    assets: value_of_shares,
                    :shares,
                    fee: Option::None,
                );
            value_of_shares
        }

        fn get_owning_position_id(self: @ContractState) -> u32 {
            self.owning_position_id.read()
        }

        fn get_perps_contract(self: @ContractState) -> ContractAddress {
            self.perps_contract.read()
        }
    }


    impl ERC4626ExternalAssetsManagement of ERC4626Component::AssetsManagementTrait<ContractState> {
        fn transfer_assets_in(
            ref self: ERC4626Component::ComponentState<ContractState>,
            from: ContractAddress,
            assets: u256,
        ) {
            let this = starknet::get_contract_address();
            let asset_dispatcher = IERC20Dispatcher { contract_address: self.ERC4626_asset.read() };
            assert(
                asset_dispatcher.transfer_from(from, this, assets),
                ERC4626Component::Errors::TOKEN_TRANSFER_FAILED,
            );
        }

        fn transfer_assets_out(
            ref self: ERC4626Component::ComponentState<ContractState>,
            to: ContractAddress,
            assets: u256,
        ) {
            let asset_dispatcher = IERC20Dispatcher { contract_address: self.ERC4626_asset.read() };
            assert(
                asset_dispatcher.transfer(to, assets),
                ERC4626Component::Errors::TOKEN_TRANSFER_FAILED,
            );
        }

        fn get_total_assets(self: @ERC4626Component::ComponentState<ContractState>) -> u256 {
            let asset_storage = self.get_contract().get_perps_contract();
            let asset_dispatcher = IPositionsDispatcher { contract_address: asset_storage };
            let position_tvtr = asset_dispatcher
                .get_position_tv_tr(self.get_contract().get_owning_position_id().into());
            assert(position_tvtr.total_value >= 0, NEGATIVE_TOTAL_VALUE);
            return position_tvtr.total_value.abs().into();
        }
    }


    impl ERC4626Hooks of ERC4626Component::ERC4626HooksTrait<ContractState> {
        fn before_deposit(
            ref self: ERC4626Component::ComponentState<ContractState>,
            caller: ContractAddress,
            receiver: ContractAddress,
            assets: u256,
            shares: u256,
            fee: Option<Fee>,
        ) {
            let perps_contract = self.get_contract().get_perps_contract();
            assert(perps_contract == caller, ONLY_PERPS_CAN_DEPOSIT);
            assert(perps_contract == receiver, ONLY_PERPS_CAN_RECEIVE);
        }

        /// Hooks into `InternalImpl::_deposit`.
        /// Executes logic after transferring assets and minting shares.
        /// The fee is calculated via `FeeConfigTrait`. Assets and shares
        /// represent the actual amounts the user will spend and receive, respectively.
        /// Asset fees are included in assets; share fees are excluded from shares.
        fn after_deposit(
            ref self: ERC4626Component::ComponentState<ContractState>,
            caller: ContractAddress,
            receiver: ContractAddress,
            assets: u256,
            shares: u256,
            fee: Option<Fee>,
        ) {
            let perps_contract = self.get_contract().get_perps_contract();

            // after a deposit we need to send back the underlying asset to the perps contract
            self.transfer_assets_out(to: perps_contract, :assets);
        }
        /// Hooks into `InternalImpl::_withdraw`.
        /// Executes logic before burning shares and transferring assets.
        /// The fee is calculated via `FeeConfigTrait`. Assets and shares
        /// represent the actual amounts the user will receive and spend, respectively.
        /// Asset fees are excluded from assets; share fees are included in shares.
        fn before_withdraw(
            ref self: ERC4626Component::ComponentState<ContractState>,
            caller: ContractAddress,
            receiver: ContractAddress,
            owner: ContractAddress,
            assets: u256,
            shares: u256,
            fee: Option<Fee>,
        ) {
            let perps_contract = self.get_contract().get_perps_contract();
            assert(perps_contract == caller, ONLY_PERPS_CAN_WITHDRAW);
            assert(perps_contract == receiver, ONLY_PERPS_CAN_RECEIVE);
            assert(perps_contract == owner, ONLY_PERPS_CAN_OWN);

            // before withdraw we need to pull the underlying asset from the perps contract
            self.transfer_assets_in(from: perps_contract, :assets);
        }
        /// Hooks into `InternalImpl::_withdraw`.
        /// Executes logic after burning shares and transferring assets.
        /// The fee is calculated via `FeeConfigTrait`. Assets and shares
        /// represent the actual amounts the user will receive and spend, respectively.
        /// Asset fees are excluded from assets; share fees are included in shares.
        fn after_withdraw(
            ref self: ERC4626Component::ComponentState<ContractState>,
            caller: ContractAddress,
            receiver: ContractAddress,
            owner: ContractAddress,
            assets: u256,
            shares: u256,
            fee: Option<Fee>,
        ) {}
    }
}
