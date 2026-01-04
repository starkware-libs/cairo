#[starknet::component]
pub mod AssetsComponent {
    use RolesComponent::InternalTrait as RolesInternalTrait;
    use core::cmp::min;
    use core::num::traits::{Pow, Zero};
    use core::panic_with_felt252;
    use core::panics::panic_with_byte_array;
    use openzeppelin::access::accesscontrol::AccessControlComponent;
    use openzeppelin::interfaces::erc20::IERC20Dispatcher;
    use openzeppelin::introspection::src5::SRC5Component;
    use perpetuals::core::components::assets::assets_manager::IAssetsExternalDispatcherTrait;
    use perpetuals::core::components::assets::errors::{
        ALREADY_INITIALIZED, ASSET_NOT_EXISTS, COLLATERAL_NOT_REGISTERED, FUNDING_EXPIRED,
        FUNDING_TICKS_NOT_SORTED, INACTIVE_ASSET, INVALID_FUNDING_TICK_LEN, INVALID_MEDIAN,
        INVALID_PRICE_TIMESTAMP, INVALID_TIMESTAMP, INVALID_ZERO_ASSET_ID, INVALID_ZERO_QUANTUM,
        INVALID_ZERO_TOKEN_ADDRESS, NOT_COLLATERAL, NOT_SYNTHETIC, QUORUM_NOT_REACHED,
        SIGNED_PRICES_UNSORTED, SYNTHETIC_EXPIRED_PRICE, SYNTHETIC_NOT_ACTIVE,
        ZERO_MAX_FUNDING_INTERVAL, ZERO_MAX_FUNDING_RATE, ZERO_MAX_ORACLE_PRICE,
        ZERO_MAX_PRICE_INTERVAL, oracle_public_key_not_registered,
    };
    use perpetuals::core::components::assets::events;
    use perpetuals::core::components::assets::interface::{IAssets, IAssetsManager};
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent;
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent::InternalTrait as NonceInternal;
    use perpetuals::core::types::asset::synthetic::{
        AssetConfig, AssetType, SyntheticTrait, TimelyData,
    };
    use perpetuals::core::types::asset::{AssetId, AssetStatus};
    use perpetuals::core::types::balance::{Balance, BalanceImpl};
    use perpetuals::core::types::funding::{FundingIndex, FundingTick, validate_funding_rate};
    use perpetuals::core::types::price::{
        Price, PriceImpl, PriceMulTrait, SignedPrice, convert_oracle_to_perps_price,
    };
    use perpetuals::core::types::risk_factor::RiskFactor;
    use starknet::ContractAddress;
    use starknet::storage::{
        Map, StorageAsPointer, StorageMapReadAccess, StoragePathEntry, StoragePointerReadAccess,
        StoragePointerWriteAccess, Vec, VecTrait,
    };
    use starkware_utils::components::pausable::PausableComponent;
    use starkware_utils::components::pausable::PausableComponent::InternalTrait as PausableInternal;
    use starkware_utils::components::roles::RolesComponent;
    use starkware_utils::constants::{MINUTE, TWO_POW_32};
    use starkware_utils::math::abs::Abs;
    use starkware_utils::signature::stark::{PublicKey, validate_stark_signature};
    use starkware_utils::storage::iterable_map::{
        IterableMap, IterableMapIntoIterImpl, IterableMapReadAccessImpl, IterableMapTrait,
        IterableMapWriteAccessImpl,
    };
    use starkware_utils::storage::utils::AddToStorage;
    use starkware_utils::time::time::{Time, TimeDelta, Timestamp};
    use crate::core::components::external_components::external_component_manager::ExternalComponents as ExternalComponentsComponent;
    use crate::core::components::external_components::external_component_manager::ExternalComponents::InternalTrait as ExternalComponentsInternalTrait;

    const MAX_TIME: u64 = 2_u64.pow(56);

    #[storage]
    pub struct Storage {
        /// 32-bit fixed-point number with a 32-bit fractional part.
        pub max_funding_rate: u32,
        pub max_price_interval: TimeDelta,
        pub max_funding_interval: TimeDelta,
        // Updates each price validation.
        pub last_price_validation: Timestamp,
        // Updates every funding tick.
        pub last_funding_tick: Timestamp,
        pub collateral_token_contract: IERC20Dispatcher,
        pub collateral_quantum: u64,
        pub num_of_active_synthetic_assets: usize,
        #[rename("synthetic_config")]
        pub asset_config: Map<AssetId, Option<AssetConfig>>,
        #[rename("synthetic_timely_data")]
        pub timely_data: IterableMap<AssetId, TimelyData>,
        pub risk_factor_tiers: Map<AssetId, Vec<RiskFactor>>,
        #[allow(starknet::colliding_storage_paths)]
        #[rename("risk_factor_tiers")]
        pub unchecked_access_risk_factor_tiers: Map<AssetId, Map<u64, RiskFactor>>,
        pub asset_oracle: Map<AssetId, Map<PublicKey, felt252>>,
        pub max_oracle_price_validity: TimeDelta,
        pub collateral_id: Option<AssetId>,
    }

    #[event]
    #[derive(Drop, PartialEq, starknet::Event)]
    pub enum Event {
        AssetActivated: events::AssetActivated,
        FundingTick: events::FundingTick,
        PriceTick: events::PriceTick,
    }

    #[embeddable_as(AssetsImpl)]
    impl Assets<
        TContractState,
        +HasComponent<TContractState>,
        +Drop<TContractState>,
        +AccessControlComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl OperatorNonce: OperatorNonceComponent::HasComponent<TContractState>,
        impl Pausable: PausableComponent::HasComponent<TContractState>,
        impl Roles: RolesComponent::HasComponent<TContractState>,
    > of IAssets<ComponentState<TContractState>> {
        /// Funding tick is called by the operator to update the funding index of all synthetic
        /// assets.
        ///
        /// Funding ticks asset ids MUST be in ascending order.
        /// Validations:
        /// - Only the operator can call this function.
        /// - The contract must not be paused.
        /// - The system nonce must be valid.
        /// - The number of funding ticks must be equal to the number of active synthetic assets.
        ///
        /// Execution:
        /// - Initialize the previous asset id to zero.
        /// - For each funding tick in funding_ticks:
        ///     - Validate the the funding tick asset_id is larger then the previous asset id.
        ///     - The funding tick synthetic asset_id asset exists in the system.
        ///     - The funding tick synthetic asset_id asset is active.
        ///     - The funding index must be within the max funding rate using the following formula:
        ///         |prev_funding_index-new_funding_index| <= max_funding_rate * time_diff *
        ///         synthetic_price
        ///    - Update the synthetic asset's funding index.
        ///    - Update the previous asset id to the current funding tick asset id.
        /// - Update the last funding tick time.
        fn funding_tick(
            ref self: ComponentState<TContractState>,
            operator_nonce: u64,
            funding_ticks: Span<FundingTick>,
            timestamp: Timestamp,
        ) {
            get_dep_component!(@self, Pausable).assert_not_paused();
            let mut operator_nonce_component = get_dep_component_mut!(ref self, OperatorNonce);
            operator_nonce_component.use_checked_nonce(:operator_nonce);

            assert(timestamp.into() < MAX_TIME, INVALID_TIMESTAMP);

            assert(
                funding_ticks.len() == self.get_num_of_active_synthetic_assets(),
                INVALID_FUNDING_TICK_LEN,
            );
            self.validate_price_interval_integrity(current_time: Time::now());

            let last_funding_tick = self.last_funding_tick.read();
            let time_diff: u64 = (Time::now().sub(other: last_funding_tick)).into();
            let mut prev_synthetic_id: AssetId = Zero::zero();
            let max_funding_rate = self.max_funding_rate.read();
            for funding_tick in funding_ticks {
                let synthetic_id = *funding_tick.asset_id;
                assert(synthetic_id > prev_synthetic_id, FUNDING_TICKS_NOT_SORTED);
                let asset_config = self._get_asset_config(asset_id: synthetic_id);
                assert(asset_config.asset_type == AssetType::SYNTHETIC, NOT_SYNTHETIC);
                assert(asset_config.status == AssetStatus::ACTIVE, SYNTHETIC_NOT_ACTIVE);
                self
                    ._process_funding_tick(
                        :time_diff,
                        :max_funding_rate,
                        new_funding_index: *funding_tick.funding_index,
                        :synthetic_id,
                    );
                prev_synthetic_id = synthetic_id;
            }
            self.last_funding_tick.write(Time::now());
        }

        /// Price tick for an asset to update the price of the asset.
        ///
        /// Validations:
        /// - Contract is not paused
        /// - Only the operator can call this function.
        /// - Operator nonce is valid.
        /// - Prices array is sorted according to the signer public key.
        /// - The price is the median of the prices.
        /// - The signature is valid.
        /// - The timestamp is valid(less than the max oracle price validity).
        ///
        /// Execution:
        /// - Update the asset price.
        ///     The updated price is: (price * 2^28)/ (resolution_factor * 10^12).
        fn price_tick(
            ref self: ComponentState<TContractState>,
            operator_nonce: u64,
            asset_id: AssetId,
            oracle_price: u128,
            signed_prices: Span<SignedPrice>,
        ) {
            get_dep_component!(@self, Pausable).assert_not_paused();
            let mut operator_nonce_component = get_dep_component_mut!(ref self, OperatorNonce);
            operator_nonce_component.use_checked_nonce(:operator_nonce);

            self._validate_price_tick(:asset_id, :oracle_price, :signed_prices);
            self._set_price(:asset_id, :oracle_price);
        }

        fn get_collateral_token_contract_address(
            self: @ComponentState<TContractState>, asset_id: AssetId,
        ) -> ContractAddress {
            // Base collateral is not registered in `asset_config`.
            if asset_id == self.get_collateral_id() {
                self.get_base_collateral_token_contract().contract_address
            } else {
                let token_contract = self._get_asset_config(asset_id).token_contract;
                token_contract.expect(NOT_COLLATERAL)
            }
        }

        fn get_base_collateral_token_contract(
            self: @ComponentState<TContractState>,
        ) -> IERC20Dispatcher {
            self.collateral_token_contract.read()
        }

        fn get_collateral_quantum(self: @ComponentState<TContractState>) -> u64 {
            self.collateral_quantum.read()
        }

        fn get_last_funding_tick(self: @ComponentState<TContractState>) -> Timestamp {
            self.last_funding_tick.read()
        }
        fn get_last_price_validation(self: @ComponentState<TContractState>) -> Timestamp {
            self.last_price_validation.read()
        }
        fn get_num_of_active_synthetic_assets(self: @ComponentState<TContractState>) -> usize {
            self.num_of_active_synthetic_assets.read()
        }
        fn get_collateral_id(self: @ComponentState<TContractState>) -> AssetId {
            self.collateral_id.read().expect(COLLATERAL_NOT_REGISTERED)
        }
        fn get_asset_config(
            self: @ComponentState<TContractState>, asset_id: AssetId,
        ) -> AssetConfig {
            self._get_asset_config(:asset_id)
        }
        fn get_timely_data(self: @ComponentState<TContractState>, asset_id: AssetId) -> TimelyData {
            self._get_timely_data(:asset_id)
        }

        fn get_risk_factor_tiers(
            self: @ComponentState<TContractState>, asset_id: AssetId,
        ) -> Span<RiskFactor> {
            let mut tiers = array![];
            let risk_factor_tiers = self.risk_factor_tiers.entry(asset_id);
            for i in 0..risk_factor_tiers.len() {
                tiers.append(risk_factor_tiers.at(i).read());
            }
            tiers.span()
        }
    }

    #[embeddable_as(AssetsManagerImpl)]
    impl AssetsManager<
        TContractState,
        +HasComponent<TContractState>,
        +Drop<TContractState>,
        +AccessControlComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl OperatorNonce: OperatorNonceComponent::HasComponent<TContractState>,
        impl Pausable: PausableComponent::HasComponent<TContractState>,
        impl Roles: RolesComponent::HasComponent<TContractState>,
        impl ExternalComponents: ExternalComponentsComponent::HasComponent<TContractState>,
    > of IAssetsManager<ComponentState<TContractState>> {
        fn add_oracle_to_asset(
            ref self: ComponentState<TContractState>,
            asset_id: AssetId,
            oracle_public_key: PublicKey,
            oracle_name: felt252,
            asset_name: felt252,
        ) {
            get_dep_component!(@self, Roles).only_app_governor();
            let external_components = get_dep_component!(@self, ExternalComponents);
            external_components
                ._get_assets_manager_dispatcher()
                .add_oracle_to_asset(
                    asset_id: asset_id,
                    oracle_public_key: oracle_public_key,
                    oracle_name: oracle_name,
                    asset_name: asset_name,
                );
        }

        fn add_synthetic_asset(
            ref self: ComponentState<TContractState>,
            asset_id: AssetId,
            risk_factor_tiers: Span<u16>,
            risk_factor_first_tier_boundary: u128,
            risk_factor_tier_size: u128,
            quorum: u8,
            resolution_factor: u64,
        ) {
            get_dep_component!(@self, Roles).only_app_governor();
            let external_components = get_dep_component!(@self, ExternalComponents);
            external_components
                ._get_assets_manager_dispatcher()
                .add_synthetic_asset(
                    asset_id: asset_id,
                    risk_factor_tiers: risk_factor_tiers,
                    risk_factor_first_tier_boundary: risk_factor_first_tier_boundary,
                    risk_factor_tier_size: risk_factor_tier_size,
                    quorum: quorum,
                    resolution_factor: resolution_factor,
                );
        }

        fn add_vault_collateral_asset(
            ref self: ComponentState<TContractState>,
            asset_id: AssetId,
            erc20_contract_address: ContractAddress,
            quantum: u64,
            risk_factor_tiers: Span<u16>,
            risk_factor_first_tier_boundary: u128,
            risk_factor_tier_size: u128,
            quorum: u8,
        ) {
            get_dep_component!(@self, Roles).only_app_governor();
            let external_components = get_dep_component!(@self, ExternalComponents);
            external_components
                ._get_assets_manager_dispatcher()
                .add_vault_collateral_asset(
                    asset_id: asset_id,
                    erc20_contract_address: erc20_contract_address,
                    quantum: quantum,
                    risk_factor_tiers: risk_factor_tiers,
                    risk_factor_first_tier_boundary: risk_factor_first_tier_boundary,
                    risk_factor_tier_size: risk_factor_tier_size,
                    quorum: quorum,
                );
        }

        fn add_spot_asset(
            ref self: ComponentState<TContractState>,
            asset_id: AssetId,
            erc20_contract_address: ContractAddress,
            quantum: u64,
            resolution_factor: u64,
            risk_factor_tiers: Span<u16>,
            risk_factor_first_tier_boundary: u128,
            risk_factor_tier_size: u128,
            quorum: u8,
        ) {
            get_dep_component!(@self, Roles).only_app_governor();
            let external_components = get_dep_component!(@self, ExternalComponents);
            external_components
                ._get_assets_manager_dispatcher()
                .add_spot_asset(
                    :asset_id,
                    :erc20_contract_address,
                    :quantum,
                    :resolution_factor,
                    :risk_factor_tiers,
                    :risk_factor_first_tier_boundary,
                    :risk_factor_tier_size,
                    :quorum,
                );
        }

        fn update_asset_risk_factor(
            ref self: ComponentState<TContractState>,
            operator_nonce: u64,
            asset_id: AssetId,
            risk_factor_tiers: Span<u16>,
            risk_factor_first_tier_boundary: u128,
            risk_factor_tier_size: u128,
        ) {
            // Validations:
            get_dep_component!(@self, Pausable).assert_not_paused();
            let mut nonce = get_dep_component_mut!(ref self, OperatorNonce);
            nonce.use_checked_nonce(:operator_nonce);
            let external_components = get_dep_component!(@self, ExternalComponents);
            external_components
                ._get_assets_manager_dispatcher()
                .update_asset_risk_factor(
                    operator_nonce: operator_nonce,
                    asset_id: asset_id,
                    risk_factor_tiers: risk_factor_tiers,
                    risk_factor_first_tier_boundary: risk_factor_first_tier_boundary,
                    risk_factor_tier_size: risk_factor_tier_size,
                );
        }

        fn deactivate_synthetic(ref self: ComponentState<TContractState>, synthetic_id: AssetId) {
            get_dep_component!(@self, Roles).only_app_governor();
            let external_components = get_dep_component!(@self, ExternalComponents);
            external_components
                ._get_assets_manager_dispatcher()
                .deactivate_synthetic(synthetic_id: synthetic_id);
        }

        fn remove_oracle_from_asset(
            ref self: ComponentState<TContractState>,
            asset_id: AssetId,
            oracle_public_key: PublicKey,
        ) {
            get_dep_component!(@self, Roles).only_app_governor();
            let external_components = get_dep_component!(@self, ExternalComponents);
            external_components
                ._get_assets_manager_dispatcher()
                .remove_oracle_from_asset(asset_id: asset_id, oracle_public_key: oracle_public_key);
        }

        fn update_asset_quorum(
            ref self: ComponentState<TContractState>, asset_id: AssetId, quorum: u8,
        ) {
            get_dep_component!(@self, Roles).only_app_governor();
            let external_components = get_dep_component!(@self, ExternalComponents);
            external_components
                ._get_assets_manager_dispatcher()
                .update_asset_quorum(:asset_id, :quorum);
        }

        // View functions for manager
        fn get_max_price_interval(self: @ComponentState<TContractState>) -> TimeDelta {
            let external_components = get_dep_component!(self, ExternalComponents);
            external_components._get_assets_manager_dispatcher().get_max_price_interval()
        }

        fn get_max_funding_interval(self: @ComponentState<TContractState>) -> TimeDelta {
            let external_components = get_dep_component!(self, ExternalComponents);
            external_components._get_assets_manager_dispatcher().get_max_funding_interval()
        }

        fn get_max_oracle_price_validity(self: @ComponentState<TContractState>) -> TimeDelta {
            let external_components = get_dep_component!(self, ExternalComponents);
            external_components._get_assets_manager_dispatcher().get_max_oracle_price_validity()
        }

        fn get_max_funding_rate(self: @ComponentState<TContractState>) -> u32 {
            let external_components = get_dep_component!(self, ExternalComponents);
            external_components._get_assets_manager_dispatcher().get_max_funding_rate()
        }
    }

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        +Drop<TContractState>,
        +AccessControlComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl OperatorNonce: OperatorNonceComponent::HasComponent<TContractState>,
        impl Pausable: PausableComponent::HasComponent<TContractState>,
        impl Roles: RolesComponent::HasComponent<TContractState>,
    > of InternalTrait<TContractState> {
        fn initialize(
            ref self: ComponentState<TContractState>,
            collateral_id: AssetId,
            collateral_token_address: ContractAddress,
            collateral_quantum: u64,
            max_price_interval: TimeDelta,
            max_funding_interval: TimeDelta,
            max_funding_rate: u32,
            max_oracle_price_validity: TimeDelta,
        ) {
            // Checks that the component has not been initialized yet.
            assert(self.collateral_id.read().is_none(), ALREADY_INITIALIZED);
            assert(collateral_id.is_non_zero(), INVALID_ZERO_ASSET_ID);
            assert(collateral_token_address.is_non_zero(), INVALID_ZERO_TOKEN_ADDRESS);
            assert(collateral_quantum.is_non_zero(), INVALID_ZERO_QUANTUM);
            assert(max_price_interval.is_non_zero(), ZERO_MAX_PRICE_INTERVAL);
            assert(max_funding_interval.is_non_zero(), ZERO_MAX_FUNDING_INTERVAL);
            assert(max_funding_rate.is_non_zero(), ZERO_MAX_FUNDING_RATE);
            assert(max_oracle_price_validity.is_non_zero(), ZERO_MAX_ORACLE_PRICE);
            self.collateral_id.write(Option::Some(collateral_id));
            self
                .collateral_token_contract
                .write(IERC20Dispatcher { contract_address: collateral_token_address });
            self.collateral_quantum.write(collateral_quantum);
            self.max_price_interval.write(max_price_interval);
            self.max_funding_interval.write(max_funding_interval);
            self.max_funding_rate.write(max_funding_rate);
            self.max_oracle_price_validity.write(max_oracle_price_validity);
            self.last_funding_tick.write(Time::now());
            self.last_price_validation.write(Time::now());
        }

        fn get_asset_price(self: @ComponentState<TContractState>, asset_id: AssetId) -> Price {
            let entry = self.timely_data.pointer(asset_id);
            match SyntheticTrait::get_price(entry) {
                Option::None => panic_with_felt252(NOT_SYNTHETIC),
                Option::Some(price) => price,
            }
        }

        /// Returns the stored price directly without checking whether it exists.
        fn get_asset_price_unsafe(
            self: @ComponentState<TContractState>, asset_id: AssetId,
        ) -> Price {
            let entry = self.timely_data.pointer(asset_id);
            SyntheticTrait::at_price(entry)
        }

        /// Returns both the stored price and funding index directly without checking their
        /// existence.
        fn get_price_and_funding_index(
            self: @ComponentState<TContractState>, asset_id: AssetId,
        ) -> (Price, FundingIndex) {
            let entry = self.timely_data.pointer(asset_id);
            (SyntheticTrait::at_price(entry), SyntheticTrait::at_funding_index(entry))
        }


        /// Get the risk factor of a synthetic asset.
        ///   - synthetic_value = |price * balance|
        ///   - If the synthetic value is less than or equal to the first tier boundary, return the
        ///   first risk factor.
        ///   - index = (synthetic_value - risk_factor_first_tier_boundary) / risk_factor_tier_size
        ///   - risk_factor = risk_factor_tiers[index]
        ///   - If the index is out of bounds, return the last risk factor.
        /// - If the asset is not synthetic, panic.
        fn get_asset_risk_factor(
            self: @ComponentState<TContractState>,
            asset_id: AssetId,
            balance: Balance,
            price: Price,
        ) -> RiskFactor {
            let synthetic_value: u128 = price.mul(rhs: balance).abs();
            return self.get_synthetic_risk_factor_for_value(asset_id, synthetic_value);
        }

        /// Get the risk factor of a synthetic asset.
        ///   - synthetic_value = |price * balance|
        ///   - If the synthetic value is less than or equal to the first tier boundary, return the
        ///   first risk factor.
        ///   - index = (synthetic_value - risk_factor_first_tier_boundary) / risk_factor_tier_size
        ///   - risk_factor = risk_factor_tiers[index]
        ///   - If the index is out of bounds, return the last risk factor.
        /// - If the asset is not synthetic, panic.
        fn get_synthetic_risk_factor_for_value(
            self: @ComponentState<TContractState>, synthetic_id: AssetId, synthetic_value: u128,
        ) -> RiskFactor {
            let entry = self.asset_config.entry(synthetic_id).as_ptr();
            if (!SyntheticTrait::is_some_config(entry)) {
                panic_with_felt252(NOT_SYNTHETIC)
            }
            let risk_factor_first_tier_boundary =
                SyntheticTrait::at_risk_factor_first_tier_boundary(
                entry,
            );
            let risk_factor_tier_size = SyntheticTrait::at_risk_factor_tier_size(entry);
            let asset_risk_factor_tiers = self.risk_factor_tiers.entry(synthetic_id);

            let unchecked_access_risk_factor_tiers = self
                .unchecked_access_risk_factor_tiers
                .entry(synthetic_id);

            let index = if synthetic_value < risk_factor_first_tier_boundary {
                0_u128
            } else {
                let tier_size = risk_factor_tier_size;
                let first_tier_offset = synthetic_value - risk_factor_first_tier_boundary;
                min(
                    1_u128 + (first_tier_offset / tier_size),
                    asset_risk_factor_tiers.len().into() - 1,
                )
            };

            unchecked_access_risk_factor_tiers.entry(index.try_into().unwrap()).read()
        }

        fn get_funding_index(
            self: @ComponentState<TContractState>, synthetic_id: AssetId,
        ) -> FundingIndex {
            let entry = self.timely_data.pointer(synthetic_id);

            match SyntheticTrait::get_funding_index(entry) {
                Option::None => panic_with_felt252(ASSET_NOT_EXISTS),
                Option::Some(funding_index) => funding_index,
            }
        }

        /// Returns the stored funding index directly without checking whether it exists.
        fn get_funding_index_unsafe(
            self: @ComponentState<TContractState>, asset_id: AssetId,
        ) -> FundingIndex {
            let entry = self.timely_data.pointer(asset_id);
            SyntheticTrait::at_funding_index(entry)
        }


        fn validate_asset_active(self: @ComponentState<TContractState>, synthetic_id: AssetId) {
            if let Option::Some(config) = self.asset_config.read(synthetic_id) {
                assert(config.status == AssetStatus::ACTIVE, SYNTHETIC_NOT_ACTIVE);
            } else {
                panic_with_felt252(ASSET_NOT_EXISTS);
            }
        }

        /// Validates assets integrity prerequisites:
        /// - Funding interval validation.
        /// - Prices validation.
        fn validate_assets_integrity(ref self: ComponentState<TContractState>) {
            let current_time = Time::now();
            // Funding validation.
            assert(
                current_time.sub(self.last_funding_tick.read()) <= self.max_funding_interval.read(),
                FUNDING_EXPIRED,
            );
            self.validate_price_interval_integrity(:current_time);
        }

        fn validate_price_interval_integrity(
            ref self: ComponentState<TContractState>, current_time: Timestamp,
        ) {
            /// If `max_price_interval` has passed since `last_price_validation`, validate
            /// synthetic prices and update `last_price_validation` to current time.
            let max_price_interval = self.max_price_interval.read();
            if current_time.sub(self.last_price_validation.read()) > max_price_interval {
                self._validate_asset_prices(current_time, max_price_interval);
                self.last_price_validation.write(current_time);
            }
        }
    }

    #[generate_trait]
    impl PrivateImpl<
        TContractState,
        +HasComponent<TContractState>,
        +Drop<TContractState>,
        +AccessControlComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl OperatorNonce: OperatorNonceComponent::HasComponent<TContractState>,
        impl Pausable: PausableComponent::HasComponent<TContractState>,
        impl Roles: RolesComponent::HasComponent<TContractState>,
    > of PrivateTrait<TContractState> {
        fn _get_asset_config(
            self: @ComponentState<TContractState>, asset_id: AssetId,
        ) -> AssetConfig {
            self.asset_config.read(asset_id).expect(ASSET_NOT_EXISTS)
        }

        fn _get_timely_data(
            self: @ComponentState<TContractState>, asset_id: AssetId,
        ) -> TimelyData {
            self.timely_data.read(asset_id).expect(ASSET_NOT_EXISTS)
        }

        fn _process_funding_tick(
            ref self: ComponentState<TContractState>,
            time_diff: u64,
            max_funding_rate: u32,
            new_funding_index: FundingIndex,
            synthetic_id: AssetId,
        ) {
            let mut timely_data = self._get_timely_data(asset_id: synthetic_id);
            let last_funding_index = timely_data.funding_index;
            let index_diff: i64 = (new_funding_index - last_funding_index).into();
            validate_funding_rate(
                :synthetic_id,
                index_diff: index_diff.abs(),
                :max_funding_rate,
                :time_diff,
                synthetic_price: self.get_asset_price(asset_id: synthetic_id),
            );
            timely_data.funding_index = new_funding_index;
            self.timely_data.write(synthetic_id, timely_data);
            self
                .emit(
                    events::FundingTick {
                        asset_id: synthetic_id, funding_index: new_funding_index,
                    },
                );
        }

        /// Validates a price tick.
        /// - The signed prices must be sorted by the public key.
        /// - The signed prices are signed by the oracles.
        /// - The number of signed prices (i.e. signing oracles) must not be smaller than the
        /// quorum.
        /// - The signed price time must not be in the future, and must not lag more than
        /// `max_oracle_price_validity`.
        /// - The `oracle_price` is the median of the signed_prices.
        fn _validate_price_tick(
            self: @ComponentState<TContractState>,
            asset_id: AssetId,
            oracle_price: u128,
            signed_prices: Span<SignedPrice>,
        ) {
            let asset_config = self._get_asset_config(:asset_id);
            assert(asset_config.status != AssetStatus::INACTIVE, INACTIVE_ASSET);
            let signed_prices_len = signed_prices.len();
            assert(signed_prices_len >= asset_config.quorum.into(), QUORUM_NOT_REACHED);

            let mut lower_amount: usize = 0;
            let mut higher_amount: usize = 0;
            let mut equal_amount: usize = 0;

            let mut previous_public_key_opt: Option<PublicKey> = Option::None;

            let now: u64 = Time::now().into();
            let max_oracle_price_validity = self.max_oracle_price_validity.read();
            let from = now - max_oracle_price_validity.into();
            // Add 2 minutes to allow timestamps that were signed after the block timestamp as the
            // timestamp is the open block timestamp and there could be a scenario where the oracle
            // signed the price after the block was opened and still got into the block.
            let to = now + 2 * MINUTE;

            for signed_price in signed_prices {
                if *signed_price.oracle_price < oracle_price {
                    lower_amount += 1;
                } else if *signed_price.oracle_price > oracle_price {
                    higher_amount += 1;
                } else {
                    equal_amount += 1;
                }

                assert(
                    from <= (*signed_price).timestamp.into()
                        && (*signed_price).timestamp.into() <= to,
                    INVALID_PRICE_TIMESTAMP,
                );

                self._validate_oracle_signature(:asset_id, signed_price: *signed_price);

                if let Option::Some(previous_public_key) = previous_public_key_opt {
                    let prev: u256 = previous_public_key.into();
                    let current: u256 = (*signed_price.signer_public_key).into();
                    assert(prev < current, SIGNED_PRICES_UNSORTED);
                }
                previous_public_key_opt = Option::Some((*signed_price.signer_public_key));
            }

            assert(2 * (lower_amount + equal_amount) >= signed_prices_len, INVALID_MEDIAN);
            assert(2 * (higher_amount + equal_amount) >= signed_prices_len, INVALID_MEDIAN);
        }

        fn _set_price(
            ref self: ComponentState<TContractState>, asset_id: AssetId, oracle_price: u128,
        ) {
            let mut asset_config = self._get_asset_config(:asset_id);
            let price = convert_oracle_to_perps_price(
                :oracle_price, resolution_factor: asset_config.resolution_factor,
            );

            let mut timely_data = self._get_timely_data(:asset_id);
            timely_data.price = price;
            timely_data.last_price_update = Time::now();
            self.timely_data.write(asset_id, timely_data);

            // If the asset is pending, it'll be activated.
            if asset_config.status == AssetStatus::PENDING {
                // Activates the synthetic asset.
                asset_config.status = AssetStatus::ACTIVE;
                if (asset_config.asset_type == AssetType::SYNTHETIC) {
                    self.num_of_active_synthetic_assets.add_and_write(1);
                }
                self.asset_config.write(asset_id, Option::Some(asset_config));
                self.emit(events::AssetActivated { asset_id });
            }
            self.emit(events::PriceTick { asset_id, price });
        }

        fn _validate_oracle_signature(
            self: @ComponentState<TContractState>, asset_id: AssetId, signed_price: SignedPrice,
        ) {
            let packed_asset_oracle = self
                .asset_oracle
                .entry(asset_id)
                .read(signed_price.signer_public_key);

            if packed_asset_oracle.is_zero() {
                panic_with_byte_array(
                    @oracle_public_key_not_registered(asset_id, signed_price.signer_public_key),
                );
            }
            let packed_price_timestamp: felt252 = signed_price.oracle_price.into()
                * TWO_POW_32.into()
                + signed_price.timestamp.into();
            let msg_hash = core::pedersen::pedersen(packed_asset_oracle, packed_price_timestamp);
            validate_stark_signature(
                public_key: signed_price.signer_public_key,
                :msg_hash,
                signature: signed_price.signature,
            );
        }

        fn _validate_asset_prices(
            self: @ComponentState<TContractState>,
            current_time: Timestamp,
            max_price_interval: TimeDelta,
        ) {
            for (asset_id, timely_data) in self.timely_data {
                // Validate only active asset
                if self._get_asset_config(:asset_id).status == AssetStatus::ACTIVE {
                    assert(
                        max_price_interval >= current_time.sub(timely_data.last_price_update),
                        SYNTHETIC_EXPIRED_PRICE,
                    );
                }
            };
        }
    }
}
