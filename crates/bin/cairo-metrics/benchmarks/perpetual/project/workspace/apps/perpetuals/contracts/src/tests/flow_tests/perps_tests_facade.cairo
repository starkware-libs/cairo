use core::array;
use core::dict::{Felt252Dict, Felt252DictTrait};
use core::fmt::Debug;
use core::nullable::{FromNullableResult, match_nullable};
use core::num::traits::{WideMul, Zero};
use external_components::interface::{
    EXTERNAL_COMPONENT_ASSETS, EXTERNAL_COMPONENT_DELEVERAGES, EXTERNAL_COMPONENT_DEPOSITS,
    EXTERNAL_COMPONENT_LIQUIDATIONS, EXTERNAL_COMPONENT_TRANSFERS, EXTERNAL_COMPONENT_VAULT,
    EXTERNAL_COMPONENT_WITHDRAWALS,
};
use openzeppelin::interfaces::erc20::IERC20Dispatcher;
use openzeppelin::interfaces::erc4626::{IERC4626Dispatcher, IERC4626DispatcherTrait};
use perpetuals::core::components::assets::assets_manager::{
    IAssetsExternalDispatcher, IAssetsExternalDispatcherTrait,
};
use perpetuals::core::components::assets::interface::{IAssetsDispatcher, IAssetsDispatcherTrait};
use perpetuals::core::components::deposit::deposit_manager::deposit_hash;
use perpetuals::core::components::deposit::interface::{
    DepositStatus, IDepositDispatcher, IDepositDispatcherTrait,
};
use perpetuals::core::components::operator_nonce::interface::{
    IOperatorNonceDispatcher, IOperatorNonceDispatcherTrait,
};
use perpetuals::core::components::positions::Positions::{FEE_POSITION, INSURANCE_FUND_POSITION};
use perpetuals::core::components::positions::interface::{
    IPositionsDispatcher, IPositionsDispatcherTrait,
};
use perpetuals::core::components::snip::SNIP12MetadataImpl;
use perpetuals::core::interface::{ICoreDispatcher, ICoreDispatcherTrait, Settlement};
use perpetuals::core::types::asset::synthetic::{AssetBalanceInfo, AssetType};
use perpetuals::core::types::asset::{AssetId, AssetIdTrait, AssetStatus};
use perpetuals::core::types::balance::Balance;
use perpetuals::core::types::funding::FundingTick;
use perpetuals::core::types::order::{LimitOrder, Order};
use perpetuals::core::types::position::{PositionData, PositionId};
use perpetuals::core::types::price::{Price, SignedPrice};
use perpetuals::core::types::transfer::TransferArgs;
use perpetuals::core::types::vault::ConvertPositionToVault;
use perpetuals::core::types::withdraw::WithdrawArgs;
use perpetuals::core::value_risk_calculator::PositionTVTR;
use perpetuals::tests::constants::*;
use perpetuals::tests::event_test_utils::{
    assert_add_spot_event_with_expected, assert_add_synthetic_event_with_expected,
    assert_deactivate_synthetic_asset_event_with_expected, assert_deleverage_event_with_expected,
    assert_deposit_canceled_event_with_expected, assert_deposit_event_with_expected,
    assert_deposit_processed_event_with_expected, assert_liquidate_event_with_expected,
    assert_trade_event_with_expected, assert_transfer_event_with_expected,
    assert_transfer_request_event_with_expected, assert_withdraw_event_with_expected,
    assert_withdraw_request_event_with_expected,
};
use perpetuals::tests::test_utils::{deploy_account, validate_balance};
use snforge_std::cheatcodes::events::{Event, EventSpy, EventSpyTrait, EventsFilterTrait};
use snforge_std::signature::stark_curve::{StarkCurveKeyPairImpl, StarkCurveSignerImpl};
use snforge_std::{
    CheatSpan, ContractClassTrait, DeclareResultTrait, cheat_caller_address,
    start_cheat_block_timestamp_global, stop_cheat_caller_address,
};
use starknet::ContractAddress;
use starkware_utils::components::request_approvals::interface::{
    IRequestApprovalsDispatcher, IRequestApprovalsDispatcherTrait, RequestStatus,
};
use starkware_utils::components::roles::interface::{IRolesDispatcher, IRolesDispatcherTrait};
use starkware_utils::constants::{DAY, MAX_U128, MINUTE, TEN_POW_12, TWO_POW_32, TWO_POW_40};
use starkware_utils::hash::message_hash::OffchainMessageHash;
use starkware_utils::signature::stark::{PublicKey, Signature};
use starkware_utils::time::time::{Time, TimeDelta, Timestamp};
use starkware_utils_testing::signing::StarkKeyPair;
use starkware_utils_testing::test_utils::{
    Deployable, TokenState, TokenTrait, cheat_caller_address_once,
};
use vault::interface::IProtocolVaultDispatcher;
use crate::core::components::deposit::events as deposit_events;
use crate::core::components::external_components;
use crate::core::components::external_components::interface::{
    IExternalComponentsDispatcher, IExternalComponentsDispatcherTrait,
};
use crate::core::types::funding::FundingIndex;
use crate::tests::constants::{KEY_PAIR_1, PREMIUM_COST};

pub const TIME_STEP: u64 = MINUTE;
const BEGINNING_OF_TIME: u64 = DAY * 365 * 50;
const ORACLE_SECRET_KEY_OFFSET: felt252 = 1000;

#[derive(Drop, Copy)]
pub struct VaultState {
    pub position_id: PositionId,
    pub asset_id: AssetId,
    pub deployed_vault: DeployedVault,
    pub asset_info: AssetInfo,
    pub user: User,
}

#[generate_trait]
pub impl VaultStateImpl of VaultStateTrait {}

#[derive(Drop, Copy)]
pub struct DeployedVault {
    pub contract_address: ContractAddress,
    pub erc20: IERC20Dispatcher,
    pub erc4626: IERC4626Dispatcher,
    pub protocol_vault: IProtocolVaultDispatcher,
    pub owning_account: Account,
}

#[generate_trait]
pub impl DeployedVaultImp of DeployedVaultTrait {}

pub fn deploy_protocol_vault_with_dispatcher(
    perps_address: ContractAddress, vault_position_id: PositionId, usdc_token_state: TokenState,
) -> DeployedVault {
    let stark_key_pair = StarkCurveKeyPairImpl::generate();
    let owning_account_address = deploy_account(stark_key_pair);
    let owning_account = Account { address: owning_account_address, key_pair: stark_key_pair };
    let mut calldata = ArrayTrait::new();
    let governance_admin = GOVERNANCE_ADMIN();
    let upgrade_delay = UPGRADE_DELAY;
    let name: ByteArray = "Perpetuals Protocol Vault";
    let symbol: ByteArray = "PPV";
    let initial_price = 1_000_000_u64; // 1 USDC with 6 decimals
    governance_admin.serialize(ref calldata);
    upgrade_delay.serialize(ref calldata);
    name.serialize(ref calldata);
    symbol.serialize(ref calldata);
    usdc_token_state.address.serialize(ref calldata);
    perps_address.serialize(ref calldata);
    vault_position_id.value.serialize(ref calldata);
    owning_account_address.serialize(ref calldata);
    initial_price.serialize(ref calldata);
    let contract = snforge_std::declare("ProtocolVault").unwrap().contract_class();
    let output = contract.deploy(@calldata);
    if output.is_err() {
        panic(output.unwrap_err());
    }
    let (contract_address, _) = output.unwrap();
    let erc20 = IERC20Dispatcher { contract_address: contract_address };
    let erc4626 = IERC4626Dispatcher { contract_address: contract_address };
    let protocol_vault = IProtocolVaultDispatcher { contract_address: contract_address };
    DeployedVault {
        contract_address: contract_address, erc20, erc4626, protocol_vault, owning_account,
    }
}

#[derive(Copy, Drop)]
pub struct DepositInfo {
    depositor: Account,
    position_id: PositionId,
    quantized_amount: u64,
    salt: felt252,
    asset_id: AssetId,
}

#[derive(Copy, Drop)]
pub struct RequestInfo {
    asset_id: AssetId,
    recipient: User,
    position_id: PositionId,
    amount: u64,
    expiration: Timestamp,
    salt: felt252,
    request_hash: felt252,
}

#[derive(Copy, Drop)]
pub struct OrderInfo {
    pub order: Order,
    signature: Signature,
    hash: felt252,
}

/// Account is a representation of any user account that can interact with the contracts.
#[derive(Copy, Drop)]
pub struct Account {
    pub address: ContractAddress,
    pub key_pair: StarkKeyPair,
}

#[generate_trait]
pub impl AccountImpl of AccountTrait {
    fn new(secret_key: felt252) -> Account {
        let key_pair = StarkCurveKeyPairImpl::from_secret_key(secret_key);
        let contract_class = snforge_std::declare("AccountUpgradeable").unwrap().contract_class();
        let (address, _) = contract_class.deploy(@array![key_pair.public_key]).unwrap();

        Account { key_pair, address }
    }

    fn set_as_caller(self: @Account, contract_address: ContractAddress) {
        cheat_caller_address_once(:contract_address, caller_address: *self.address);
    }

    fn sign_message(self: @Account, message: felt252) -> Signature {
        let (r, s) = (*self).key_pair.sign(message).unwrap();
        array![r, s].span()
    }
}

#[derive(Copy, Drop)]
pub struct User {
    pub position_id: PositionId,
    pub account: Account,
    initial_balance: u64,
}

#[generate_trait]
pub impl UserTraitImpl of UserTrait {
    fn new(token_state: TokenState, secret_key: felt252, position_id: PositionId) -> User {
        let account = AccountTrait::new(secret_key);

        let initial_balance = USER_INIT_BALANCE.try_into().expect('Value should not overflow');
        token_state.fund(account.address, initial_balance.into());

        User { position_id, account, initial_balance }
    }
    fn set_as_caller(self: @User, contract_address: ContractAddress) {
        self.account.set_as_caller(:contract_address);
    }
}

#[derive(Copy, Drop)]
struct Oracle {
    key_pair: StarkKeyPair,
    name: felt252,
}

#[generate_trait]
impl OracleImpl of OracleTrait {
    fn new(secret_key: felt252, name: felt252) -> Oracle {
        let key_pair = StarkCurveKeyPairImpl::from_secret_key(secret_key);
        Oracle { key_pair, name }
    }
    fn sign_price(
        self: @Oracle, oracle_price: u128, timestamp: u32, asset_name: felt252,
    ) -> SignedPrice {
        let packed_timestamp_price = (timestamp.into() + oracle_price * TWO_POW_32.into()).into();
        let oracle_name_asset_name = *self.name + asset_name * TWO_POW_40.into();
        let msg_hash = core::pedersen::pedersen(oracle_name_asset_name, packed_timestamp_price);
        let (r, s) = (*self).key_pair.sign(msg_hash).unwrap();
        let signature = array![r, s].span();

        SignedPrice {
            signature, signer_public_key: *self.key_pair.public_key, timestamp, oracle_price,
        }
    }
}

#[derive(Drop)]
struct PerpetualsConfig {
    operator: Account,
    governance_admin: ContractAddress,
    role_admin: ContractAddress,
    app_governor: ContractAddress,
    upgrade_delay: u64,
    collateral_id: AssetId,
    collateral_token_address: ContractAddress,
    collateral_quantum: u64,
    max_price_interval: TimeDelta,
    max_funding_interval: TimeDelta,
    max_funding_rate: u32,
    cancel_delay: TimeDelta,
    max_oracle_price_validity: TimeDelta,
    fee_position_owner_account: ContractAddress,
    fee_position_owner_public_key: PublicKey,
    insurance_fund_position_owner_account: ContractAddress,
    insurance_fund_position_owner_public_key: PublicKey,
    forced_action_timelock: u64,
    premium_cost: u64,
}

#[generate_trait]
pub impl PerpetualsConfigImpl of PerpetualsConfigTrait {
    fn new(collateral_token_address: ContractAddress, collateral_quantum: u64) -> PerpetualsConfig {
        let operator = AccountTrait::new('OPERATOR');
        PerpetualsConfig {
            operator,
            governance_admin: GOVERNANCE_ADMIN(),
            role_admin: APP_ROLE_ADMIN(),
            app_governor: APP_GOVERNOR(),
            upgrade_delay: UPGRADE_DELAY,
            collateral_id: COLLATERAL_ASSET_ID(),
            collateral_token_address,
            collateral_quantum,
            max_price_interval: MAX_PRICE_INTERVAL,
            max_funding_interval: MAX_FUNDING_INTERVAL,
            max_funding_rate: MAX_FUNDING_RATE,
            cancel_delay: CANCEL_DELAY,
            max_oracle_price_validity: MAX_ORACLE_PRICE_VALIDITY,
            fee_position_owner_account: operator.address,
            fee_position_owner_public_key: operator.key_pair.public_key,
            insurance_fund_position_owner_account: operator.address,
            insurance_fund_position_owner_public_key: operator.key_pair.public_key,
            forced_action_timelock: FORCED_ACTION_TIMELOCK,
            premium_cost: PREMIUM_COST,
        }
    }
}

impl PerpetualsContractStateImpl of Deployable<PerpetualsConfig, ContractAddress> {
    fn deploy(self: @PerpetualsConfig) -> ContractAddress {
        let mut calldata = ArrayTrait::new();
        self.governance_admin.serialize(ref calldata);
        self.upgrade_delay.serialize(ref calldata);
        self.collateral_id.serialize(ref calldata);
        self.collateral_token_address.serialize(ref calldata);
        self.collateral_quantum.serialize(ref calldata);
        self.max_price_interval.serialize(ref calldata);
        self.max_oracle_price_validity.serialize(ref calldata);
        self.max_funding_interval.serialize(ref calldata);
        self.max_funding_rate.serialize(ref calldata);
        self.cancel_delay.serialize(ref calldata);
        self.fee_position_owner_public_key.serialize(ref calldata);
        self.insurance_fund_position_owner_public_key.serialize(ref calldata);
        self.forced_action_timelock.serialize(ref calldata);
        self.premium_cost.serialize(ref calldata);

        let perpetuals_contract = snforge_std::declare("Core").unwrap().contract_class();
        let (address, _) = perpetuals_contract.deploy(@calldata).unwrap();
        address
    }
}

#[derive(Drop, Copy)]
pub struct AssetInfo {
    pub asset_name: felt252,
    pub asset_id: AssetId,
    pub risk_factor_data: RiskFactorTiers,
    pub oracles: Span<Oracle>,
    pub resolution_factor: u64,
    pub erc20_contract_address: ContractAddress,
    pub quantum: u64,
}

#[derive(Drop, Copy)]
pub struct RiskFactorTiers {
    pub tiers: Span<u16>,
    pub first_tier_boundary: u128,
    pub tier_size: u128,
}

#[generate_trait]
pub impl AssetInfoImpl of AssetInfoTrait {
    fn new(asset_name: felt252, risk_factor_data: RiskFactorTiers, oracles_len: u8) -> AssetInfo {
        let mut oracles = array![];
        for i in 1..oracles_len + 1 {
            oracles
                .append(
                    OracleTrait::new(
                        secret_key: i.into() + ORACLE_SECRET_KEY_OFFSET, name: i.into(),
                    ),
                );
        }

        AssetInfo {
            asset_name,
            asset_id: AssetIdTrait::new(value: asset_name),
            risk_factor_data,
            oracles: oracles.span(),
            resolution_factor: SYNTHETIC_RESOLUTION_FACTOR,
            erc20_contract_address: Zero::zero(),
            quantum: 0,
        }
    }

    fn new_with_resolution(
        asset_name: felt252, risk_factor_data: RiskFactorTiers, oracles_len: u8, resolution: u64,
    ) -> AssetInfo {
        let mut oracles = array![];
        for i in 1..oracles_len + 1 {
            oracles
                .append(
                    OracleTrait::new(
                        secret_key: i.into() + ORACLE_SECRET_KEY_OFFSET, name: i.into(),
                    ),
                );
        }

        AssetInfo {
            asset_name,
            asset_id: AssetIdTrait::new(value: asset_name),
            risk_factor_data,
            oracles: oracles.span(),
            resolution_factor: resolution,
            erc20_contract_address: Zero::zero(),
            quantum: 0,
        }
    }

    fn new_collateral(
        asset_name: felt252,
        risk_factor_data: RiskFactorTiers,
        oracles_len: u8,
        erc20_contract_address: ContractAddress,
    ) -> AssetInfo {
        Self::new_collateral_with_resolution(
            :asset_name,
            :risk_factor_data,
            :oracles_len,
            resolution: COLLATERAL_RESOLUTION_FACTOR,
            :erc20_contract_address,
            quantum: COLLATERAL_QUANTUM,
        )
    }

    fn new_collateral_with_resolution(
        asset_name: felt252,
        risk_factor_data: RiskFactorTiers,
        oracles_len: u8,
        resolution: u64,
        erc20_contract_address: ContractAddress,
        quantum: u64,
    ) -> AssetInfo {
        let mut oracles = array![];
        for i in 1..oracles_len + 1 {
            oracles
                .append(
                    OracleTrait::new(
                        secret_key: i.into() + ORACLE_SECRET_KEY_OFFSET, name: i.into(),
                    ),
                );
        }

        AssetInfo {
            asset_name,
            asset_id: AssetIdTrait::new(value: asset_name),
            risk_factor_data,
            oracles: oracles.span(),
            resolution_factor: resolution,
            erc20_contract_address,
            quantum,
        }
    }

    fn sign_price(self: @AssetInfo, oracle_price: u128) -> Span<SignedPrice> {
        let timestamp = Time::now().seconds.try_into().unwrap();
        let mut signed_prices = array![];
        for oracle in self.oracles {
            let signed_price = oracle
                .sign_price(:oracle_price, :timestamp, asset_name: *self.asset_name);
            signed_prices.append(signed_price);
        }

        signed_prices.span()
    }
}


/// PerpsTestsFacade is the main struct that holds the state of the flow tests.
#[derive(Drop)]
pub struct PerpsTestsFacade {
    pub governance_admin: ContractAddress,
    pub role_admin: ContractAddress,
    pub app_governor: ContractAddress,
    pub perpetuals_contract: ContractAddress,
    pub token_state: TokenState,
    pub collateral_quantum: u64,
    pub collateral_id: AssetId,
    pub operator: Account,
    pub event_info: EventSpy,
    salt_gen: felt252,
    pub registered_spots: Array<(AssetId, ContractAddress)>,
}

#[generate_trait]
impl PrivatePerpsTestsFacadeImpl of PrivatePerpsTestsFacadeTrait {
    fn get_last_event(
        ref self: PerpsTestsFacade, contract_address: ContractAddress,
    ) -> @(ContractAddress, Event) {
        let events = self.event_info.get_events().emitted_by(contract_address).events;
        events[events.len() - 1]
    }

    fn get_last_events(
        ref self: PerpsTestsFacade, contract_address: ContractAddress, count: u32,
    ) -> Span<@Event> {
        let events = self.event_info.get_events().emitted_by(contract_address).events;
        let mut a: Array<@Event> = ArrayTrait::new();
        for i in 0..count {
            let x = count - i;
            let offset = events.len() - x;
            let (_, event) = events.at(offset);
            a.append(event);
        }
        a.span()
    }

    fn generate_salt(ref self: PerpsTestsFacade) -> felt252 {
        self.salt_gen += 1;
        self.salt_gen
    }
    fn get_nonce(self: @PerpsTestsFacade) -> u64 {
        let dispatcher = IOperatorNonceDispatcher { contract_address: *self.perpetuals_contract };
        self.operator.set_as_caller(*self.perpetuals_contract);
        dispatcher.get_operator_nonce()
    }

    fn set_app_governor_as_caller(self: @PerpsTestsFacade) {
        cheat_caller_address_once(
            contract_address: *self.perpetuals_contract, caller_address: *self.app_governor,
        );
    }
    fn set_app_role_admin_as_caller(self: @PerpsTestsFacade) {
        cheat_caller_address_once(
            contract_address: *self.perpetuals_contract, caller_address: *self.role_admin,
        );
    }
    fn set_governance_admin_as_caller(self: @PerpsTestsFacade) {
        cheat_caller_address_once(
            contract_address: *self.perpetuals_contract, caller_address: *self.governance_admin,
        );
    }

    fn set_roles(self: @PerpsTestsFacade) {
        let dispatcher = IRolesDispatcher { contract_address: *self.perpetuals_contract };

        self.set_governance_admin_as_caller();
        dispatcher.register_app_role_admin(*self.role_admin);
        self.set_governance_admin_as_caller();
        dispatcher.register_upgrade_governor(*self.governance_admin);

        self.set_app_role_admin_as_caller();
        dispatcher.register_app_governor(*self.app_governor);

        self.set_app_role_admin_as_caller();
        dispatcher.register_operator(account: *self.operator.address);
    }
}

/// FlowTestTrait is the interface for the PerpsTestsFacade struct. It is the sole way to interact
/// with the contract by calling the following wrapper functions.
#[generate_trait]
pub impl PerpsTestsFacadeImpl of PerpsTestsFacadeTrait {
    fn new(token_state: TokenState) -> PerpsTestsFacade {
        start_cheat_block_timestamp_global(BEGINNING_OF_TIME);

        let vault_external_component = snforge_std::declare("VaultsManager")
            .unwrap()
            .contract_class();

        let withdrawal_external_component = snforge_std::declare("WithdrawalManager")
            .unwrap()
            .contract_class();

        let transfers_external_component = snforge_std::declare("TransferManager")
            .unwrap()
            .contract_class();

        let liquidations_external_component = snforge_std::declare("LiquidationManager")
            .unwrap()
            .contract_class();

        let deleverage_external_component = snforge_std::declare("DeleverageManager")
            .unwrap()
            .contract_class();

        let deposit_external_component = snforge_std::declare("DepositManager")
            .unwrap()
            .contract_class();

        let assets_external_component = snforge_std::declare("AssetsManager")
            .unwrap()
            .contract_class();

        let collateral_quantum = COLLATERAL_QUANTUM;
        let perpetuals_config: PerpetualsConfig = PerpetualsConfigTrait::new(
            collateral_token_address: token_state.address, :collateral_quantum,
        );
        let perpetuals_contract = Deployable::deploy(@perpetuals_config);

        let perpetual_wrapper = PerpsTestsFacade {
            governance_admin: perpetuals_config.governance_admin,
            role_admin: perpetuals_config.role_admin,
            app_governor: perpetuals_config.app_governor,
            perpetuals_contract,
            token_state,
            collateral_quantum,
            collateral_id: perpetuals_config.collateral_id,
            operator: perpetuals_config.operator,
            event_info: snforge_std::spy_events(),
            salt_gen: 0,
            registered_spots: array![(COLLATERAL_ASSET_ID(), token_state.address)],
        };
        perpetual_wrapper.set_roles();

        cheat_caller_address(
            contract_address: perpetuals_contract,
            caller_address: GOVERNANCE_ADMIN(),
            span: CheatSpan::Indefinite,
        );
        let external_components_dispatcher = IExternalComponentsDispatcher {
            contract_address: perpetuals_contract,
        };
        external_components_dispatcher
            .register_external_component(
                component_type: EXTERNAL_COMPONENT_VAULT,
                component_address: *vault_external_component.class_hash,
            );

        external_components_dispatcher
            .register_external_component(
                component_type: EXTERNAL_COMPONENT_WITHDRAWALS,
                component_address: *withdrawal_external_component.class_hash,
            );

        external_components_dispatcher
            .register_external_component(
                component_type: EXTERNAL_COMPONENT_TRANSFERS,
                component_address: *transfers_external_component.class_hash,
            );

        external_components_dispatcher
            .register_external_component(
                component_type: EXTERNAL_COMPONENT_LIQUIDATIONS,
                component_address: *liquidations_external_component.class_hash,
            );

        external_components_dispatcher
            .register_external_component(
                component_type: EXTERNAL_COMPONENT_DELEVERAGES,
                component_address: *deleverage_external_component.class_hash,
            );

        external_components_dispatcher
            .register_external_component(
                component_type: EXTERNAL_COMPONENT_DEPOSITS,
                component_address: *deposit_external_component.class_hash,
            );

        external_components_dispatcher
            .register_external_component(
                component_type: EXTERNAL_COMPONENT_ASSETS,
                component_address: *assets_external_component.class_hash,
            );

        external_components_dispatcher
            .activate_external_component(
                component_type: EXTERNAL_COMPONENT_DELEVERAGES,
                component_address: *deleverage_external_component.class_hash,
            );
        external_components_dispatcher
            .activate_external_component(
                component_type: EXTERNAL_COMPONENT_LIQUIDATIONS,
                component_address: *liquidations_external_component.class_hash,
            );
        external_components_dispatcher
            .activate_external_component(
                component_type: EXTERNAL_COMPONENT_TRANSFERS,
                component_address: *transfers_external_component.class_hash,
            );
        external_components_dispatcher
            .activate_external_component(
                component_type: EXTERNAL_COMPONENT_WITHDRAWALS,
                component_address: *withdrawal_external_component.class_hash,
            );
        external_components_dispatcher
            .activate_external_component(
                component_type: EXTERNAL_COMPONENT_VAULT,
                component_address: *vault_external_component.class_hash,
            );

        external_components_dispatcher
            .activate_external_component(
                component_type: EXTERNAL_COMPONENT_DEPOSITS,
                component_address: *deposit_external_component.class_hash,
            );

        external_components_dispatcher
            .activate_external_component(
                component_type: EXTERNAL_COMPONENT_ASSETS,
                component_address: *assets_external_component.class_hash,
            );

        stop_cheat_caller_address(contract_address: perpetuals_contract);
        perpetual_wrapper
    }

    fn find_contract_for_asset_id(ref self: PerpsTestsFacade, asset_id: AssetId) -> TokenState {
        let mut i = 0;
        while i < self.registered_spots.len() {
            let (registered_asset_id, contract_address) = self.registered_spots[i];
            if *registered_asset_id == asset_id {
                return TokenState { address: *contract_address, owner: self.token_state.owner };
            }
            i += 1;
        }
        panic!("Asset ID not found");
    }

    fn register_vault_share_spot_asset(
        ref self: PerpsTestsFacade, position_vault: User,
    ) -> VaultState {
        self.operator.set_as_caller(self.perpetuals_contract);

        let vault = deploy_protocol_vault_with_dispatcher(
            perps_address: self.perpetuals_contract,
            vault_position_id: position_vault.position_id,
            usdc_token_state: self.token_state,
        );

        let risk_factor_first_tier_boundary = MAX_U128;
        let risk_factor_tier_size = 1;
        let risk_factor_1 = array![100].span();

        let asset_info = AssetInfoTrait::new_with_resolution(
            asset_name: 'VS_1',
            risk_factor_data: RiskFactorTiers {
                tiers: risk_factor_1,
                first_tier_boundary: risk_factor_first_tier_boundary,
                tier_size: risk_factor_tier_size,
            },
            oracles_len: 1,
            resolution: 1000000,
        );

        let asset_id = asset_info.asset_id;
        let assets_external_dispatcher = IAssetsExternalDispatcher {
            contract_address: self.perpetuals_contract,
        };

        self.set_app_governor_as_caller();
        assets_external_dispatcher
            .add_vault_collateral_asset(
                asset_id,
                erc20_contract_address: vault.contract_address,
                quantum: 1,
                risk_factor_tiers: risk_factor_1,
                :risk_factor_first_tier_boundary,
                :risk_factor_tier_size,
                quorum: asset_info.oracles.len().try_into().unwrap(),
            );

        assert_add_spot_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            asset_id: asset_info.asset_id,
            risk_factor_tiers: asset_info.risk_factor_data.tiers,
            risk_factor_first_tier_boundary: asset_info.risk_factor_data.first_tier_boundary,
            risk_factor_tier_size: asset_info.risk_factor_data.tier_size,
            resolution_factor: asset_info.resolution_factor,
            quorum: asset_info.oracles.len().try_into().unwrap(),
            contract_address: vault.contract_address,
            quantum: 1,
        );

        for oracle in asset_info.oracles {
            self.set_app_governor_as_caller();
            assets_external_dispatcher
                .add_oracle_to_asset(
                    asset_info.asset_id,
                    *oracle.key_pair.public_key,
                    *oracle.name,
                    asset_info.asset_name,
                );
        }

        let operator_nonce = self.get_nonce();

        let order = ConvertPositionToVault {
            position_to_convert: position_vault.position_id,
            vault_asset_id: asset_id,
            expiration: Time::now(),
        };
        let signature = position_vault
            .account
            .sign_message(order.get_message_hash(position_vault.account.key_pair.public_key));

        self.operator.set_as_caller(self.perpetuals_contract);

        ICoreDispatcher { contract_address: self.perpetuals_contract }
            .activate_vault(operator_nonce: operator_nonce, order: order, signature: signature);

        self.registered_spots.append((asset_id, vault.contract_address));
        return VaultState {
            position_id: position_vault.position_id,
            asset_id,
            deployed_vault: vault,
            asset_info,
            user: position_vault,
        };
    }

    fn new_position(
        ref self: PerpsTestsFacade,
        position_id: PositionId,
        owner_public_key: felt252,
        owner_account: ContractAddress,
    ) {
        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);
        IPositionsDispatcher { contract_address: self.perpetuals_contract }
            .new_position(
                :operator_nonce,
                :position_id,
                :owner_public_key,
                :owner_account,
                owner_protection_enabled: true,
            );
    }

    fn price_tick(ref self: PerpsTestsFacade, asset_info: @AssetInfo, price: u128) {
        // 10^12 == ORACLE_SCALE_SN_PERPS_RATIO.
        let oracle_price = price * (*asset_info.resolution_factor).into() * TEN_POW_12.into();

        let signed_prices = asset_info.sign_price(:oracle_price);

        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);
        IAssetsDispatcher { contract_address: self.perpetuals_contract }
            .price_tick(
                :operator_nonce,
                asset_id: *asset_info.asset_id,
                :oracle_price,
                signed_prices: signed_prices,
            );
    }

    fn deposit(
        ref self: PerpsTestsFacade,
        depositor: Account,
        position_id: PositionId,
        quantized_amount: u64,
    ) -> DepositInfo {
        self._deposit(:depositor, :position_id, :quantized_amount, asset_id: self.collateral_id)
    }

    fn deposit_spot(
        ref self: PerpsTestsFacade,
        depositor: Account,
        asset_id: AssetId,
        position_id: PositionId,
        quantized_amount: u64,
    ) -> DepositInfo {
        self._deposit(:depositor, :position_id, :quantized_amount, :asset_id)
    }

    fn _deposit(
        ref self: PerpsTestsFacade,
        depositor: Account,
        position_id: PositionId,
        quantized_amount: u64,
        asset_id: AssetId,
    ) -> DepositInfo {
        let quantum = if (asset_id == self.collateral_id) {
            self.collateral_quantum
        } else {
            let dispatcher = IAssetsDispatcher { contract_address: self.perpetuals_contract };
            dispatcher.get_asset_config(asset_id: asset_id).quantum
        };
        let unquantized_amount = quantized_amount * quantum;
        let address = depositor.address;

        let token_state = self.find_contract_for_asset_id(:asset_id);

        let user_balance_before = token_state.balance_of(account: address);
        let contract_balance_before = token_state.balance_of(self.perpetuals_contract);
        let now = Time::now();

        token_state
            .approve(
                owner: address,
                spender: self.perpetuals_contract,
                amount: unquantized_amount.into(),
            );
        let salt = self.generate_salt();

        depositor.set_as_caller(self.perpetuals_contract);

        if (asset_id == self.collateral_id) {
            IDepositDispatcher { contract_address: self.perpetuals_contract }
                .deposit(:position_id, :quantized_amount, :salt);
        } else {
            IDepositDispatcher { contract_address: self.perpetuals_contract }
                .deposit_asset(asset_id: asset_id, :position_id, :quantized_amount, :salt);
        }

        validate_balance(
            token_state: token_state,
            :address,
            expected_balance: user_balance_before - unquantized_amount.into(),
        );
        validate_balance(
            token_state: token_state,
            address: self.perpetuals_contract,
            expected_balance: contract_balance_before + unquantized_amount.into(),
        );

        let deposit_hash = deposit_hash(
            token_address: token_state.address,
            depositor: address,
            :position_id,
            :quantized_amount,
            :salt,
        );
        self.validate_deposit_status(:deposit_hash, expected_status: DepositStatus::PENDING(now));

        assert_deposit_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            :position_id,
            depositing_address: address,
            collateral_id: asset_id,
            :quantized_amount,
            :unquantized_amount,
            deposit_request_hash: deposit_hash,
            :salt,
        );

        DepositInfo { depositor, position_id, quantized_amount, salt, asset_id }
    }

    fn cancel_deposit(ref self: PerpsTestsFacade, deposit_info: DepositInfo) {
        let DepositInfo { depositor, position_id, quantized_amount, salt, asset_id } = deposit_info;
        let token_state = self.find_contract_for_asset_id(:asset_id);
        let user_balance_before = token_state.balance_of(depositor.address);
        let contract_balance_before = token_state.balance_of(self.perpetuals_contract);

        depositor.set_as_caller(self.perpetuals_contract);
        IDepositDispatcher { contract_address: self.perpetuals_contract }
            .cancel_deposit(:asset_id, :position_id, :quantized_amount, :salt);
        let deposit_hash = deposit_hash(
            token_address: token_state.address,
            depositor: depositor.address,
            :position_id,
            :quantized_amount,
            :salt,
        );

        let unquantized_amount = quantized_amount * self.collateral_quantum;

        validate_balance(
            token_state: token_state,
            address: depositor.address,
            expected_balance: user_balance_before + unquantized_amount.into(),
        );
        validate_balance(
            token_state: token_state,
            address: self.perpetuals_contract,
            expected_balance: contract_balance_before - unquantized_amount.into(),
        );

        self.validate_deposit_status(:deposit_hash, expected_status: DepositStatus::CANCELED);

        assert_deposit_canceled_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            :position_id,
            depositing_address: depositor.address,
            collateral_id: asset_id,
            :quantized_amount,
            :unquantized_amount,
            deposit_request_hash: deposit_hash,
            :salt,
        );
    }

    fn process_deposit(ref self: PerpsTestsFacade, deposit_info: DepositInfo) {
        let DepositInfo { depositor, position_id, quantized_amount, salt, asset_id } = deposit_info;
        let collateral_balance_before = if (asset_id == self.collateral_id) {
            self.get_position_collateral_balance(position_id)
        } else {
            self.get_position_asset_balance(position_id, asset_id)
        };

        let token_state = self.find_contract_for_asset_id(:asset_id);

        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);
        IDepositDispatcher { contract_address: self.perpetuals_contract }
            .process_deposit(
                :operator_nonce,
                depositor: depositor.address,
                :asset_id,
                :position_id,
                :quantized_amount,
                :salt,
            );

        if (asset_id == self.collateral_id) {
            self
                .validate_collateral_balance(
                    :position_id,
                    expected_balance: collateral_balance_before + quantized_amount.into(),
                );
        } else {
            self
                .validate_asset_balance(
                    :position_id, asset_id: asset_id, expected_balance: quantized_amount.into(),
                );
        }

        let deposit_hash = deposit_hash(
            token_address: token_state.address,
            depositor: depositor.address,
            :position_id,
            :quantized_amount,
            :salt,
        );

        self.validate_deposit_status(:deposit_hash, expected_status: DepositStatus::PROCESSED);

        assert_deposit_processed_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            :position_id,
            depositing_address: depositor.address,
            collateral_id: asset_id,
            :quantized_amount,
            unquantized_amount: quantized_amount * self.collateral_quantum,
            deposit_request_hash: deposit_hash,
            :salt,
        );
    }

    fn withdraw_request(ref self: PerpsTestsFacade, user: User, amount: u64) -> RequestInfo {
        self
            .withdraw_request_with_caller(
                :user, asset_id: self.collateral_id, :amount, caller: user,
            )
    }

    fn withdraw_spot_request(
        ref self: PerpsTestsFacade, user: User, asset_id: AssetId, amount: u64,
    ) -> RequestInfo {
        self.withdraw_request_with_caller(:user, :asset_id, :amount, caller: user)
    }

    fn withdraw_request_with_caller(
        ref self: PerpsTestsFacade, user: User, asset_id: AssetId, amount: u64, caller: User,
    ) -> RequestInfo {
        let account = user.account;
        let position_id = user.position_id;
        let recipient = account.address;
        let expiration = Time::now().add(Time::seconds(10));
        let salt = self.generate_salt();

        let request_hash = WithdrawArgs {
            recipient, position_id, collateral_id: asset_id, amount, expiration, salt,
        }
            .get_message_hash(public_key: account.key_pair.public_key);
        let signature = account.sign_message(message: request_hash);

        caller.account.set_as_caller(self.perpetuals_contract);
        ICoreDispatcher { contract_address: self.perpetuals_contract }
            .withdraw_request(
                :signature,
                collateral_id: asset_id,
                :recipient,
                :position_id,
                :amount,
                :expiration,
                :salt,
            );

        self.validate_request_approval(:request_hash, expected_status: RequestStatus::PENDING);

        assert_withdraw_request_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            :position_id,
            :recipient,
            collateral_id: asset_id,
            :amount,
            expiration: expiration,
            withdraw_request_hash: request_hash,
            :salt,
        );

        RequestInfo {
            asset_id, recipient: user, position_id, amount, expiration, salt, request_hash,
        }
    }

    fn withdraw(ref self: PerpsTestsFacade, withdraw_info: RequestInfo) {
        let RequestInfo {
            asset_id, recipient, position_id, amount, expiration, salt, request_hash,
        } = withdraw_info;
        let address = recipient.account.address;

        let token_state = self.find_contract_for_asset_id(:asset_id);
        let user_balance_before = token_state.balance_of(account: address);
        let contract_balance_before = token_state.balance_of(self.perpetuals_contract);

        let position_balance_before = if (asset_id == self.collateral_id) {
            self.get_position_collateral_balance(position_id)
        } else {
            self.get_position_asset_balance(position_id, asset_id)
        };

        let quantum = if (asset_id == self.collateral_id) {
            self.collateral_quantum
        } else {
            let dispatcher = IAssetsDispatcher { contract_address: self.perpetuals_contract };
            dispatcher.get_asset_config(asset_id: asset_id).quantum
        };

        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);
        ICoreDispatcher { contract_address: self.perpetuals_contract }
            .withdraw(
                :operator_nonce,
                collateral_id: asset_id,
                recipient: address,
                :position_id,
                :amount,
                :expiration,
                :salt,
            );

        if (asset_id == self.collateral_id) {
            self
                .validate_collateral_balance(
                    :position_id, expected_balance: position_balance_before - amount.into(),
                );
        } else {
            self
                .validate_asset_balance(
                    :position_id,
                    asset_id: asset_id,
                    expected_balance: position_balance_before - amount.into(),
                );
        }

        let unquantized_amount = (amount * quantum).into();
        validate_balance(
            token_state: token_state,
            :address,
            expected_balance: user_balance_before + unquantized_amount,
        );
        validate_balance(
            token_state: token_state,
            address: self.perpetuals_contract,
            expected_balance: contract_balance_before - unquantized_amount,
        );

        self.validate_request_approval(:request_hash, expected_status: RequestStatus::PROCESSED);

        assert_withdraw_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            :position_id,
            recipient: address,
            collateral_id: asset_id,
            :amount,
            :expiration,
            withdraw_request_hash: request_hash,
            :salt,
        );
    }

    fn transfer_request(
        ref self: PerpsTestsFacade, sender: User, recipient: User, amount: u64,
    ) -> RequestInfo {
        let expiration = Time::now().add(delta: Time::weeks(1));

        let salt = self.generate_salt();
        let transfer_args = TransferArgs {
            position_id: sender.position_id,
            salt,
            expiration,
            collateral_id: self.collateral_id,
            amount,
            recipient: recipient.position_id,
        };
        let request_hash = transfer_args
            .get_message_hash(public_key: sender.account.key_pair.public_key);
        let signature = sender.account.sign_message(message: request_hash);

        sender.account.set_as_caller(self.perpetuals_contract);
        ICoreDispatcher { contract_address: self.perpetuals_contract }
            .transfer_request(
                signature,
                asset_id: self.collateral_id,
                recipient: recipient.position_id,
                position_id: sender.position_id,
                :amount,
                :expiration,
                :salt,
            );

        self.validate_request_approval(:request_hash, expected_status: RequestStatus::PENDING);

        assert_transfer_request_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            position_id: sender.position_id,
            recipient: recipient.position_id,
            collateral_id: self.collateral_id,
            :amount,
            :expiration,
            transfer_request_hash: request_hash,
            :salt,
        );

        RequestInfo {
            asset_id: self.collateral_id,
            recipient,
            position_id: sender.position_id,
            amount,
            expiration,
            salt,
            request_hash,
        }
    }

    fn transfer(ref self: PerpsTestsFacade, transfer_info: RequestInfo) {
        let RequestInfo {
            asset_id, recipient, position_id, amount, expiration, salt, request_hash,
        } = transfer_info;
        let dispatcher = IPositionsDispatcher { contract_address: self.perpetuals_contract };
        let sender_balance_before = dispatcher
            .get_position_assets(position_id: position_id)
            .collateral_balance;
        let recipient_balance_before = dispatcher
            .get_position_assets(position_id: recipient.position_id)
            .collateral_balance;

        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);
        ICoreDispatcher { contract_address: self.perpetuals_contract }
            .transfer(
                :operator_nonce,
                :asset_id,
                recipient: recipient.position_id,
                position_id: position_id,
                amount: amount,
                expiration: expiration,
                salt: salt,
            );

        self
            .validate_request_approval(
                request_hash: request_hash, expected_status: RequestStatus::PROCESSED,
            );

        self
            .validate_collateral_balance(
                position_id: position_id, expected_balance: sender_balance_before - amount.into(),
            );

        self
            .validate_collateral_balance(
                position_id: recipient.position_id,
                expected_balance: recipient_balance_before + amount.into(),
            );

        assert_transfer_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            position_id: position_id,
            recipient: recipient.position_id,
            collateral_id: asset_id,
            :amount,
            expiration: expiration,
            transfer_request_hash: request_hash,
            :salt,
        );
    }

    fn create_order(
        ref self: PerpsTestsFacade,
        user: User,
        base_amount: i64,
        base_asset_id: AssetId,
        quote_amount: i64,
        fee_amount: u64,
    ) -> OrderInfo {
        let expiration = Time::now().add(delta: Time::weeks(1));
        let salt = self.generate_salt();
        let order = Order {
            position_id: user.position_id,
            base_asset_id,
            base_amount,
            quote_asset_id: self.collateral_id,
            quote_amount,
            fee_asset_id: self.collateral_id,
            fee_amount,
            expiration,
            salt,
        };
        let hash = order.get_message_hash(user.account.key_pair.public_key);
        OrderInfo { order, signature: user.account.sign_message(hash), hash }
    }

    fn trade(
        ref self: PerpsTestsFacade,
        order_info_a: OrderInfo,
        order_info_b: OrderInfo,
        base: i64,
        quote: i64,
        fee_a: u64,
        fee_b: u64,
    ) {
        let OrderInfo { order: order_a, signature: signature_a, hash: hash_a } = order_info_a;
        let OrderInfo { order: order_b, signature: signature_b, hash: hash_b } = order_info_b;
        let asset_id = order_a.base_asset_id;
        let dispatcher = IPositionsDispatcher { contract_address: self.perpetuals_contract };
        let user_a_balance_before = dispatcher
            .get_position_assets(position_id: order_a.position_id);
        let user_a_collateral_balance_before = user_a_balance_before.collateral_balance;
        let user_a_synthetic_balance_before = get_synthetic_balance(
            assets: user_a_balance_before.assets, :asset_id,
        );
        let user_b_balance_before = dispatcher
            .get_position_assets(position_id: order_b.position_id);
        let user_b_collateral_balance_before = user_b_balance_before.collateral_balance;
        let user_b_synthetic_balance_before = get_synthetic_balance(
            assets: user_b_balance_before.assets, :asset_id,
        );
        let fee_position_balance_before = dispatcher
            .get_position_assets(position_id: FEE_POSITION)
            .collateral_balance;

        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);
        ICoreDispatcher { contract_address: self.perpetuals_contract }
            .trade(
                :operator_nonce,
                :signature_a,
                :signature_b,
                :order_a,
                :order_b,
                actual_amount_base_a: base,
                actual_amount_quote_a: quote,
                actual_fee_a: fee_a,
                actual_fee_b: fee_b,
            );

        self
            .validate_collateral_balance(
                position_id: order_a.position_id,
                expected_balance: user_a_collateral_balance_before
                    + (quote - fee_a.try_into().unwrap()).into(),
            );

        self
            .validate_collateral_balance(
                position_id: order_b.position_id,
                expected_balance: user_b_collateral_balance_before
                    - (quote + fee_b.try_into().unwrap()).into(),
            );

        self
            .validate_synthetic_balance(
                position_id: order_a.position_id,
                :asset_id,
                expected_balance: user_a_synthetic_balance_before + base.into(),
            );

        self
            .validate_synthetic_balance(
                position_id: order_b.position_id,
                :asset_id,
                expected_balance: user_b_synthetic_balance_before - base.into(),
            );

        self
            .validate_collateral_balance(
                position_id: FEE_POSITION,
                expected_balance: fee_position_balance_before + (fee_a + fee_b).into(),
            );

        assert_trade_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            order_base_asset_id: asset_id,
            order_a_position_id: order_a.position_id,
            order_a_base_amount: order_a.base_amount,
            order_a_quote_amount: order_a.quote_amount,
            fee_a_amount: order_a.fee_amount,
            collateral_id: self.collateral_id,
            order_b_position_id: order_b.position_id,
            order_b_base_amount: order_b.base_amount,
            order_b_quote_amount: order_b.quote_amount,
            fee_b_amount: order_b.fee_amount,
            actual_amount_base_a: base,
            actual_amount_quote_a: quote,
            actual_fee_a: fee_a,
            actual_fee_b: fee_b,
            order_a_hash: hash_a,
            order_b_hash: hash_b,
        );
    }

    fn create_settlement(
        ref self: PerpsTestsFacade,
        order_a: OrderInfo,
        order_b: OrderInfo,
        base: i64,
        quote: i64,
        fee_a: u64,
        fee_b: u64,
    ) -> Settlement {
        Settlement {
            signature_a: order_a.signature,
            signature_b: order_b.signature,
            order_a: order_a.order,
            order_b: order_b.order,
            actual_amount_base_a: base,
            actual_amount_quote_a: quote,
            actual_fee_a: fee_a,
            actual_fee_b: fee_b,
        }
    }

    fn create_updated_position_data(
        ref self: PerpsTestsFacade,
        position_data_a: PositionData,
        position_data_b: PositionData,
        asset_id: AssetId,
        settlement: Settlement,
    ) -> (PositionData, PositionData) {
        let mut new_synthetics_a = ArrayTrait::new();
        for synthetic in position_data_a.assets {
            if *synthetic.id == asset_id {
                let new_balance = *synthetic.balance + settlement.actual_amount_base_a.into();
                new_synthetics_a
                    .append(
                        AssetBalanceInfo {
                            id: *synthetic.id,
                            balance: new_balance,
                            price: *synthetic.price,
                            risk_factor: *synthetic.risk_factor,
                            cached_funding_index: FundingIndex { value: 0 },
                        },
                    );
            } else {
                new_synthetics_a.append(*synthetic);
            }
        }

        let mut new_synthetics_b = ArrayTrait::new();
        for synthetic in position_data_b.assets {
            if *synthetic.id == asset_id {
                let new_balance = *synthetic.balance - settlement.actual_amount_base_a.into();
                new_synthetics_b
                    .append(
                        AssetBalanceInfo {
                            id: *synthetic.id,
                            balance: new_balance,
                            price: *synthetic.price,
                            risk_factor: *synthetic.risk_factor,
                            cached_funding_index: FundingIndex { value: 0 },
                        },
                    );
            } else {
                new_synthetics_b.append(*synthetic);
            }
        }

        (
            PositionData {
                collateral_balance: position_data_a.collateral_balance
                    + settlement.actual_amount_quote_a.into()
                    - settlement.actual_fee_a.into(),
                assets: new_synthetics_a.span(),
            },
            PositionData {
                collateral_balance: position_data_b.collateral_balance
                    - settlement.actual_amount_quote_a.into()
                    - settlement.actual_fee_b.into(),
                assets: new_synthetics_b.span(),
            },
        )
    }

    fn multi_trade(ref self: PerpsTestsFacade, trades: Span<Settlement>) {
        let dispatcher = IPositionsDispatcher { contract_address: self.perpetuals_contract };
        let mut positions_dict: Felt252Dict<Nullable<PositionData>> = Default::default();
        let mut cached_positions: Array<PositionId> = ArrayTrait::new();
        let mut total_fee: u64 = 0;
        for trade in trades {
            let settlement = *trade;
            let asset_id = settlement.order_a.base_asset_id;
            total_fee += settlement.actual_fee_a + settlement.actual_fee_b;

            let mut position_data_a =
                match match_nullable(
                    positions_dict.get(settlement.order_a.position_id.value.into()),
                ) {
                FromNullableResult::Null => {
                    cached_positions.append(settlement.order_a.position_id);
                    dispatcher.get_position_assets(position_id: settlement.order_a.position_id)
                },
                FromNullableResult::NotNull(value) => value.unbox(),
            };

            let mut position_data_b =
                match match_nullable(
                    positions_dict.get(settlement.order_b.position_id.value.into()),
                ) {
                FromNullableResult::Null => {
                    cached_positions.append(settlement.order_b.position_id);
                    dispatcher.get_position_assets(position_id: settlement.order_b.position_id)
                },
                FromNullableResult::NotNull(value) => value.unbox(),
            };

            let (updated_position_data_a, updated_position_data_b) = self
                .create_updated_position_data(
                    position_data_a, position_data_b, asset_id, settlement,
                );

            positions_dict
                .insert(
                    settlement.order_a.position_id.value.into(),
                    NullableTrait::new(updated_position_data_a),
                );

            positions_dict
                .insert(
                    settlement.order_b.position_id.value.into(),
                    NullableTrait::new(updated_position_data_b),
                );
        }

        let fee_position_balance_before = dispatcher
            .get_position_assets(position_id: FEE_POSITION)
            .collateral_balance;

        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);
        ICoreDispatcher { contract_address: self.perpetuals_contract }
            .multi_trade(:operator_nonce, :trades);
        self
            .validate_collateral_balance(
                position_id: FEE_POSITION,
                expected_balance: fee_position_balance_before + total_fee.into(),
            );
        for position_id in cached_positions {
            let balance = positions_dict.get(position_id.value.into());
            let position_data = match match_nullable(balance) {
                FromNullableResult::Null => panic!("Position not found"),
                FromNullableResult::NotNull(value) => value.unbox(),
            };
            self
                .validate_collateral_balance(
                    :position_id, expected_balance: position_data.collateral_balance,
                );

            for synthetic in position_data.assets {
                self
                    .validate_synthetic_balance(
                        :position_id, asset_id: *synthetic.id, expected_balance: *synthetic.balance,
                    );
            }
        }
    }

    fn liquidate(
        ref self: PerpsTestsFacade,
        liquidated_user: User,
        liquidator_order: OrderInfo,
        liquidated_base: i64,
        liquidated_quote: i64,
        liquidated_insurance_fee: u64,
        liquidator_fee: u64,
    ) {
        let OrderInfo {
            order: liquidator_order, signature: liquidator_signature, hash: liquidator_hash,
        } = liquidator_order;
        let asset_id = liquidator_order.base_asset_id;
        let dispatcher = IPositionsDispatcher { contract_address: self.perpetuals_contract };
        let liquidated_balance_before = dispatcher
            .get_position_assets(position_id: liquidated_user.position_id);
        let liquidated_collateral_balance_before = liquidated_balance_before.collateral_balance;
        let liquidated_synthetic_balance_before = get_synthetic_balance(
            assets: liquidated_balance_before.assets, :asset_id,
        );
        let liquidator_balance_before = dispatcher
            .get_position_assets(position_id: liquidator_order.position_id);
        let liquidator_collateral_balance_before = liquidator_balance_before.collateral_balance;
        let liquidator_synthetic_balance_before = get_synthetic_balance(
            assets: liquidator_balance_before.assets, :asset_id,
        );
        let fee_position_balance_before = dispatcher
            .get_position_assets(position_id: FEE_POSITION)
            .collateral_balance;
        let insurance_fee_position_balance_before = dispatcher
            .get_position_assets(position_id: INSURANCE_FUND_POSITION)
            .collateral_balance;

        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);

        ICoreDispatcher { contract_address: self.perpetuals_contract }
            .liquidate(
                :operator_nonce,
                :liquidator_signature,
                liquidated_position_id: liquidated_user.position_id,
                :liquidator_order,
                actual_amount_base_liquidated: liquidated_base,
                actual_amount_quote_liquidated: liquidated_quote,
                actual_liquidator_fee: liquidator_fee,
                liquidated_fee_amount: liquidated_insurance_fee,
            );

        self
            .validate_collateral_balance(
                position_id: liquidated_user.position_id,
                expected_balance: liquidated_collateral_balance_before
                    - liquidated_insurance_fee.into()
                    + liquidated_quote.into(),
            );

        self
            .validate_synthetic_balance(
                position_id: liquidated_user.position_id,
                :asset_id,
                expected_balance: liquidated_synthetic_balance_before + liquidated_base.into(),
            );

        self
            .validate_collateral_balance(
                position_id: liquidator_order.position_id,
                expected_balance: liquidator_collateral_balance_before
                    - liquidator_fee.into()
                    - liquidated_quote.into(),
            );

        self
            .validate_synthetic_balance(
                position_id: liquidator_order.position_id,
                :asset_id,
                expected_balance: liquidator_synthetic_balance_before - liquidated_base.into(),
            );

        self
            .validate_collateral_balance(
                position_id: FEE_POSITION,
                expected_balance: fee_position_balance_before + liquidator_fee.into(),
            );

        self
            .validate_collateral_balance(
                position_id: INSURANCE_FUND_POSITION,
                expected_balance: insurance_fee_position_balance_before
                    + liquidated_insurance_fee.into(),
            );

        assert_liquidate_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            liquidated_position_id: liquidated_user.position_id,
            liquidator_order_position_id: liquidator_order.position_id,
            liquidator_order_base_asset_id: asset_id,
            liquidator_order_base_amount: liquidator_order.base_amount,
            collateral_id: self.collateral_id,
            liquidator_order_quote_amount: liquidator_order.quote_amount,
            liquidator_order_fee_amount: liquidator_order.fee_amount,
            actual_amount_base_liquidated: liquidated_base,
            actual_amount_quote_liquidated: liquidated_quote,
            actual_liquidator_fee: liquidator_fee,
            insurance_fund_fee_amount: liquidated_insurance_fee,
            liquidator_order_hash: liquidator_hash,
        );
    }

    fn deleverage(
        ref self: PerpsTestsFacade,
        deleveraged_user: User,
        deleverager_user: User,
        base_asset_id: AssetId,
        deleveraged_base: i64,
        deleveraged_quote: i64,
    ) {
        let dispatcher = IPositionsDispatcher { contract_address: self.perpetuals_contract };
        let deleveraged_balance_before = dispatcher
            .get_position_assets(position_id: deleveraged_user.position_id);
        let deleveraged_collateral_balance_before = deleveraged_balance_before.collateral_balance;
        let deleveraged_synthetic_balance_before = get_synthetic_balance(
            assets: deleveraged_balance_before.assets, asset_id: base_asset_id,
        );
        let deleverager_balance_before = dispatcher
            .get_position_assets(position_id: deleverager_user.position_id);
        let deleverager_collateral_balance_before = deleverager_balance_before.collateral_balance;
        let deleverager_synthetic_balance_before = get_synthetic_balance(
            assets: deleverager_balance_before.assets, asset_id: base_asset_id,
        );

        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);
        ICoreDispatcher { contract_address: self.perpetuals_contract }
            .deleverage(
                :operator_nonce,
                deleveraged_position_id: deleveraged_user.position_id,
                deleverager_position_id: deleverager_user.position_id,
                base_asset_id: base_asset_id,
                deleveraged_base_amount: deleveraged_base,
                deleveraged_quote_amount: deleveraged_quote,
            );

        self
            .validate_collateral_balance(
                position_id: deleveraged_user.position_id,
                expected_balance: deleveraged_collateral_balance_before + deleveraged_quote.into(),
            );

        self
            .validate_synthetic_balance(
                position_id: deleveraged_user.position_id,
                asset_id: base_asset_id,
                expected_balance: deleveraged_synthetic_balance_before + deleveraged_base.into(),
            );

        self
            .validate_collateral_balance(
                position_id: deleverager_user.position_id,
                expected_balance: deleverager_collateral_balance_before - deleveraged_quote.into(),
            );

        self
            .validate_synthetic_balance(
                position_id: deleverager_user.position_id,
                asset_id: base_asset_id,
                expected_balance: deleverager_synthetic_balance_before - deleveraged_base.into(),
            );

        assert_deleverage_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            deleveraged_position_id: deleveraged_user.position_id,
            deleverager_position_id: deleverager_user.position_id,
            base_asset_id: base_asset_id,
            collateral_id: self.collateral_id,
            deleveraged_base_amount: deleveraged_base,
            deleveraged_quote_amount: deleveraged_quote,
        );
    }

    fn add_active_synthetic(
        ref self: PerpsTestsFacade, synthetic_info: @AssetInfo, initial_price: u128,
    ) {
        let dispatcher = IAssetsDispatcher { contract_address: self.perpetuals_contract };
        let asset_external_dispatcher = IAssetsExternalDispatcher {
            contract_address: self.perpetuals_contract,
        };
        let risk_factor_data = *synthetic_info.risk_factor_data;
        self.set_app_governor_as_caller();
        asset_external_dispatcher
            .add_synthetic_asset(
                *synthetic_info.asset_id,
                risk_factor_tiers: risk_factor_data.tiers,
                risk_factor_first_tier_boundary: risk_factor_data.first_tier_boundary,
                risk_factor_tier_size: risk_factor_data.tier_size,
                quorum: synthetic_info.oracles.len().try_into().unwrap(),
                resolution_factor: *synthetic_info.resolution_factor,
            );

        assert_add_synthetic_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            asset_id: *synthetic_info.asset_id,
            risk_factor_tiers: risk_factor_data.tiers,
            risk_factor_first_tier_boundary: risk_factor_data.first_tier_boundary,
            risk_factor_tier_size: risk_factor_data.tier_size,
            resolution_factor: *synthetic_info.resolution_factor,
            quorum: synthetic_info.oracles.len().try_into().unwrap(),
        );

        assert_eq!(
            dispatcher.get_asset_config(asset_id: *synthetic_info.asset_id).status,
            AssetStatus::PENDING,
        );

        for oracle in synthetic_info.oracles {
            self.set_app_governor_as_caller();
            asset_external_dispatcher
                .add_oracle_to_asset(
                    *synthetic_info.asset_id,
                    *oracle.key_pair.public_key,
                    *oracle.name,
                    *synthetic_info.asset_name,
                );
        }
        // Activate the synthetic asset.
        self.price_tick(asset_info: synthetic_info, price: initial_price);
    }

    fn add_active_collateral(
        ref self: PerpsTestsFacade, asset_info: @AssetInfo, initial_price: u128,
    ) {
        let dispatcher = IAssetsDispatcher { contract_address: self.perpetuals_contract };
        let asset_external_dispatcher = IAssetsExternalDispatcher {
            contract_address: self.perpetuals_contract,
        };
        let risk_factor_data = *asset_info.risk_factor_data;
        self.set_app_governor_as_caller();

        asset_external_dispatcher
            .add_spot_asset(
                *asset_info.asset_id,
                erc20_contract_address: *asset_info.erc20_contract_address,
                quantum: *asset_info.quantum,
                resolution_factor: *asset_info.resolution_factor,
                risk_factor_tiers: risk_factor_data.tiers,
                risk_factor_first_tier_boundary: risk_factor_data.first_tier_boundary,
                risk_factor_tier_size: risk_factor_data.tier_size,
                quorum: asset_info.oracles.len().try_into().unwrap(),
            );

        assert_add_spot_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            asset_id: *asset_info.asset_id,
            risk_factor_tiers: risk_factor_data.tiers,
            risk_factor_first_tier_boundary: risk_factor_data.first_tier_boundary,
            risk_factor_tier_size: risk_factor_data.tier_size,
            resolution_factor: *asset_info.resolution_factor,
            quorum: asset_info.oracles.len().try_into().unwrap(),
            contract_address: *asset_info.erc20_contract_address,
            quantum: *asset_info.quantum,
        );

        assert_eq!(
            dispatcher.get_asset_config(asset_id: *asset_info.asset_id).status,
            AssetStatus::PENDING,
        );
        assert_eq!(
            dispatcher.get_asset_config(asset_id: *asset_info.asset_id).asset_type,
            AssetType::SPOT_COLLATERAL,
        );

        for oracle in asset_info.oracles {
            self.set_app_governor_as_caller();
            asset_external_dispatcher
                .add_oracle_to_asset(
                    *asset_info.asset_id,
                    *oracle.key_pair.public_key,
                    *oracle.name,
                    *asset_info.asset_name,
                );
        }
        // Activate the collateral asset.
        self.price_tick(:asset_info, price: initial_price);

        // Register the spot asset for future lookups.
        self.registered_spots.append((*asset_info.asset_id, *asset_info.erc20_contract_address));
    }

    fn deactivate_synthetic(ref self: PerpsTestsFacade, synthetic_id: AssetId) {
        self.set_app_governor_as_caller();
        let assets_dispatcher = IAssetsDispatcher { contract_address: self.perpetuals_contract };
        let asset_external_dispatcher = IAssetsExternalDispatcher {
            contract_address: self.perpetuals_contract,
        };
        asset_external_dispatcher.deactivate_synthetic(:synthetic_id);
        assert_deactivate_synthetic_asset_event_with_expected(
            spied_event: self.get_last_event(contract_address: self.perpetuals_contract),
            asset_id: synthetic_id,
        );
        assert_eq!(
            assets_dispatcher.get_asset_config(asset_id: synthetic_id).status,
            AssetStatus::INACTIVE,
        );
    }

    fn reduce_asset_position(
        ref self: PerpsTestsFacade,
        position_id_a: PositionId,
        position_id_b: PositionId,
        base_asset_id: AssetId,
        base_amount_a: i64,
    ) {
        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);
        ICoreDispatcher { contract_address: self.perpetuals_contract }
            .reduce_asset_position(
                :operator_nonce, :position_id_a, :position_id_b, :base_asset_id, :base_amount_a,
            );
    }

    fn funding_tick(ref self: PerpsTestsFacade, funding_ticks: Span<FundingTick>) {
        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);
        IAssetsDispatcher { contract_address: self.perpetuals_contract }
            .funding_tick(:operator_nonce, :funding_ticks, timestamp: Time::now());
    }

    fn get_position_asset_balance(
        self: @PerpsTestsFacade, position_id: PositionId, synthetic_id: AssetId,
    ) -> Balance {
        let assets = IPositionsDispatcher { contract_address: *self.perpetuals_contract }
            .get_position_assets(position_id);
        get_synthetic_balance(assets: assets.assets, asset_id: synthetic_id)
    }

    fn get_position_collateral_balance(
        self: @PerpsTestsFacade, position_id: PositionId,
    ) -> Balance {
        IPositionsDispatcher { contract_address: *self.perpetuals_contract }
            .get_position_assets(position_id)
            .collateral_balance
    }

    fn get_asset_price(self: @PerpsTestsFacade, synthetic_id: AssetId) -> Price {
        IAssetsDispatcher { contract_address: *self.perpetuals_contract }
            .get_timely_data(asset_id: synthetic_id)
            .price
    }

    fn is_deleveragable(self: @PerpsTestsFacade, position_id: PositionId) -> bool {
        IPositionsDispatcher { contract_address: *self.perpetuals_contract }
            .is_deleveragable(position_id)
    }

    fn is_healthy(self: @PerpsTestsFacade, position_id: PositionId) -> bool {
        IPositionsDispatcher { contract_address: *self.perpetuals_contract }.is_healthy(position_id)
    }

    fn is_liquidatable(self: @PerpsTestsFacade, position_id: PositionId) -> bool {
        IPositionsDispatcher { contract_address: *self.perpetuals_contract }
            .is_liquidatable(position_id)
    }

    fn tv_tr_of_position(self: @PerpsTestsFacade, position_id: PositionId) -> PositionTVTR {
        IPositionsDispatcher { contract_address: *self.perpetuals_contract }
            .get_position_tv_tr(position_id)
    }

    fn preview_vault_deposit(ref self: PerpsTestsFacade, vault: VaultState, amount: u64) -> u64 {
        let preview_result = vault
            .deployed_vault
            .erc4626
            .preview_deposit(assets: amount.wide_mul(self.collateral_quantum).into());

        return (preview_result / self.collateral_quantum.into()).try_into().unwrap();
    }

    fn redeem_from_vault(
        ref self: PerpsTestsFacade,
        vault: VaultState,
        withdrawing_user: User,
        receiving_user: User,
        shares_to_burn_user: u64,
        value_of_shares_user: u64,
        shares_to_burn_vault: u64,
        value_of_shares_vault: u64,
        actual_shares_user: u64,
        actual_collateral_user: u64,
    ) {
        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);

        let user_order = LimitOrder {
            source_position: withdrawing_user.position_id,
            receive_position: receiving_user.position_id,
            base_asset_id: vault.asset_id,
            base_amount: -shares_to_burn_user.try_into().unwrap(),
            quote_asset_id: self.collateral_id,
            quote_amount: value_of_shares_user.try_into().unwrap(),
            fee_asset_id: self.collateral_id,
            fee_amount: 0_u64,
            expiration: Time::now().add(Time::weeks(1)),
            salt: self.generate_salt(),
        };

        let user_order_signature = withdrawing_user
            .account
            .sign_message(
                user_order.get_message_hash(withdrawing_user.account.key_pair.public_key),
            );

        let vault_order = LimitOrder {
            source_position: vault.position_id,
            receive_position: vault.position_id,
            base_asset_id: vault.asset_id,
            base_amount: shares_to_burn_vault.try_into().unwrap(),
            quote_asset_id: self.collateral_id,
            quote_amount: -value_of_shares_vault.try_into().unwrap(),
            fee_asset_id: self.collateral_id,
            fee_amount: 0_u64,
            expiration: Time::now().add(Time::weeks(1)),
            salt: self.generate_salt(),
        };

        let vault_order_signature = vault
            .user
            .account
            .sign_message(vault_order.get_message_hash(vault.user.account.key_pair.public_key));

        ICoreDispatcher { contract_address: self.perpetuals_contract }
            .redeem_from_vault(
                :operator_nonce,
                signature: user_order_signature,
                order: user_order,
                vault_approval: vault_order,
                vault_signature: vault_order_signature,
                actual_shares_user: -actual_shares_user.try_into().unwrap(),
                actual_collateral_user: actual_collateral_user.try_into().unwrap(),
            );
    }

    fn liquidate_shares(
        ref self: PerpsTestsFacade,
        vault: VaultState,
        liquidated_user: User,
        shares_to_burn_vault: u64,
        value_of_shares_vault: u64,
        actual_shares_user: u64,
        actual_collateral_user: u64,
    ) {
        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);

        let vault_order = LimitOrder {
            source_position: vault.position_id,
            receive_position: vault.position_id,
            base_asset_id: vault.asset_id,
            base_amount: shares_to_burn_vault.try_into().unwrap(),
            quote_asset_id: self.collateral_id,
            quote_amount: -value_of_shares_vault.try_into().unwrap(),
            fee_asset_id: self.collateral_id,
            fee_amount: 0_u64,
            expiration: Time::now().add(Time::weeks(1)),
            salt: self.generate_salt(),
        };

        let vault_order_signature = vault
            .user
            .account
            .sign_message(vault_order.get_message_hash(vault.user.account.key_pair.public_key));

        ICoreDispatcher { contract_address: self.perpetuals_contract }
            .liquidate_vault_shares(
                :operator_nonce,
                liquidated_position_id: liquidated_user.position_id,
                vault_approval: vault_order,
                vault_signature: vault_order_signature,
                liquidated_asset_id: vault.asset_id,
                actual_shares_user: -actual_shares_user.try_into().unwrap(),
                actual_collateral_user: actual_collateral_user.try_into().unwrap(),
            );
    }

    fn deposit_into_vault(
        ref self: PerpsTestsFacade,
        vault: VaultState,
        amount_to_invest: u64,
        min_shares_to_receive: u64,
        depositing_user: User,
        receiving_user: User,
    ) -> DepositInfo {
        let operator_nonce = self.get_nonce();
        self.operator.set_as_caller(self.perpetuals_contract);
        let salt = self.generate_salt();

        let order = LimitOrder {
            source_position: depositing_user.position_id,
            receive_position: receiving_user.position_id,
            base_asset_id: vault.asset_id,
            base_amount: min_shares_to_receive.try_into().unwrap(),
            quote_asset_id: self.collateral_id,
            quote_amount: -(amount_to_invest.try_into().unwrap()),
            fee_asset_id: self.collateral_id,
            fee_amount: 0_u64,
            expiration: Time::now().add(Time::weeks(1)),
            salt,
        };

        let order_hash = order.get_message_hash(depositing_user.account.key_pair.public_key);
        let signature = depositing_user.account.sign_message(order_hash);

        ICoreDispatcher { contract_address: self.perpetuals_contract }
            .invest_in_vault(
                :operator_nonce, signature: signature, order: order, correlation_id: 1,
            );

        let last_event = self.get_last_event(contract_address: self.perpetuals_contract);
        let deposit_event: deposit_events::Deposit = event_as::<
            deposit_events::Deposit,
        >(
            spied_event: last_event,
            expected_event_selector: @selector!("Deposit"),
            expected_event_name: "Deposit",
        );

        DepositInfo {
            depositor: Account { address: self.perpetuals_contract, key_pair: KEY_PAIR_1() },
            position_id: receiving_user.position_id,
            quantized_amount: deposit_event.quantized_amount,
            salt,
            asset_id: vault.asset_id,
        }
    }
}


#[generate_trait]
pub impl PerpsTestsFacadeValidationsImpl of PerpsTestsFacadeValidationsTrait {
    fn validate_request_approval(
        self: @PerpsTestsFacade, request_hash: felt252, expected_status: RequestStatus,
    ) {
        let status = IRequestApprovalsDispatcher { contract_address: *self.perpetuals_contract }
            .get_request_status(request_hash);
        assert_eq!(status, expected_status);
    }

    fn validate_deposit_status(
        self: @PerpsTestsFacade, deposit_hash: felt252, expected_status: DepositStatus,
    ) {
        let status = IDepositDispatcher { contract_address: *self.perpetuals_contract }
            .get_deposit_status(deposit_hash);
        assert_eq!(status, expected_status);
    }

    fn validate_collateral_balance(
        self: @PerpsTestsFacade, position_id: PositionId, expected_balance: Balance,
    ) {
        assert_eq!(self.get_position_collateral_balance(position_id), expected_balance);
    }

    fn validate_asset_balance(
        self: @PerpsTestsFacade,
        position_id: PositionId,
        asset_id: AssetId,
        expected_balance: Balance,
    ) {
        assert_eq!(self.get_position_asset_balance(position_id, asset_id), expected_balance);
    }

    fn validate_synthetic_balance(
        self: @PerpsTestsFacade,
        position_id: PositionId,
        asset_id: AssetId,
        expected_balance: Balance,
    ) {
        let synthetic_assets = IPositionsDispatcher { contract_address: *self.perpetuals_contract }
            .get_position_assets(:position_id)
            .assets;
        let asset_balances = get_synthetic_balance(assets: synthetic_assets, :asset_id);

        assert_eq!(asset_balances, expected_balance);
    }

    fn validate_total_value(
        self: @PerpsTestsFacade, position_id: PositionId, expected_total_value: i128,
    ) {
        let dispatcher = IPositionsDispatcher { contract_address: *self.perpetuals_contract };
        let PositionTVTR { total_value, .. } = dispatcher.get_position_tv_tr(position_id);
        assert_eq!(total_value, expected_total_value);
    }

    fn validate_total_risk(
        self: @PerpsTestsFacade, position_id: PositionId, expected_total_risk: u128,
    ) {
        let dispatcher = IPositionsDispatcher { contract_address: *self.perpetuals_contract };
        let PositionTVTR { total_risk, .. } = dispatcher.get_position_tv_tr(position_id);
        assert_eq!(total_risk, expected_total_risk);
    }
}

pub fn advance_time(seconds: u64) {
    start_cheat_block_timestamp_global(Time::now().add(Time::seconds(seconds)).into());
}

fn get_synthetic_balance(assets: Span<AssetBalanceInfo>, asset_id: AssetId) -> Balance {
    for asset in assets {
        if asset.id == @asset_id {
            return asset.balance.clone();
        }
    }
    0_i64.into()
}

pub fn event_as<T, +starknet::Event<T>, +Drop<T>, +Debug<T>, +PartialEq<T>>(
    spied_event: @(ContractAddress, Event),
    expected_event_selector: @felt252,
    expected_event_name: ByteArray,
) -> T {
    let (_, raw_event) = spied_event;
    let mut data = raw_event.data.span();
    let mut keys = raw_event.keys.span();

    assert!(
        keys.pop_front() == Option::Some(expected_event_selector),
        "The expected event type '{expected_event_name}' does not match the actual event type",
    );
    let actual_event = starknet::Event::<T>::deserialize(ref :keys, ref :data);

    return actual_event.unwrap();
}
