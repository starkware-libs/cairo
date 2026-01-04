use core::num::traits::{Pow, Zero};
use perpetuals::core::components::assets::interface::{
    IAssets, IAssetsDispatcher, IAssetsDispatcherTrait, IAssetsManager, IAssetsManagerDispatcher,
    IAssetsManagerDispatcherTrait,
};
use perpetuals::core::components::deposit::deposit_manager::deposit_hash;
use perpetuals::core::components::deposit::interface::{
    DepositStatus, IDeposit, IDepositDispatcher, IDepositDispatcherTrait,
};
use perpetuals::core::components::operator_nonce::interface::IOperatorNonce;
use perpetuals::core::components::positions::Positions::{
    FEE_POSITION, INSURANCE_FUND_POSITION, InternalTrait as PositionsInternal,
};
use perpetuals::core::components::positions::interface::{
    IPositions, IPositionsDispatcher, IPositionsDispatcherTrait,
};
use perpetuals::core::components::snip::SNIP12MetadataImpl;
use perpetuals::core::errors::SIGNED_TX_EXPIRED;
use perpetuals::core::interface::{
    ICore, ICoreDispatcher, ICoreDispatcherTrait, ICoreSafeDispatcher, ICoreSafeDispatcherTrait,
};
use perpetuals::core::types::asset::AssetStatus;
use perpetuals::core::types::balance::BalanceTrait;
use perpetuals::core::types::funding::{FUNDING_SCALE, FundingIndex, FundingTick};
use perpetuals::core::types::order::{ForcedTrade, Order};
use perpetuals::core::types::position::{POSITION_VERSION, PositionMutableTrait};
use perpetuals::core::types::price::{
    PRICE_SCALE, PriceTrait, SignedPrice, convert_oracle_to_perps_price,
};
use perpetuals::core::types::risk_factor::RiskFactorTrait;
use perpetuals::core::types::set_owner_account::SetOwnerAccountArgs;
use perpetuals::core::types::set_public_key::SetPublicKeyArgs;
use perpetuals::core::types::transfer::TransferArgs;
use perpetuals::core::types::withdraw::{ForcedWithdrawArgs, WithdrawArgs};
use perpetuals::tests::constants::*;
use perpetuals::tests::event_test_utils::{
    assert_add_oracle_event_with_expected, assert_add_synthetic_event_with_expected,
    assert_asset_activated_event_with_expected, assert_change_synthetic_event_with_expected,
    assert_deactivate_synthetic_asset_event_with_expected, assert_deleverage_event_with_expected,
    assert_deposit_canceled_event_with_expected, assert_deposit_event_with_expected,
    assert_deposit_processed_event_with_expected, assert_forced_withdraw_event_with_expected,
    assert_forced_withdraw_request_event_with_expected, assert_funding_tick_event_with_expected,
    assert_liquidate_event_with_expected, assert_new_position_event_with_expected,
    assert_price_tick_event_with_expected, assert_remove_oracle_event_with_expected,
    assert_set_owner_account_event_with_expected, assert_set_public_key_event_with_expected,
    assert_set_public_key_request_event_with_expected, assert_trade_event_with_expected,
    assert_transfer_event_with_expected, assert_transfer_request_event_with_expected,
    assert_update_asset_quorum_event_with_expected,
};
use perpetuals::tests::test_utils::{
    Oracle, OracleTrait, PerpetualsInitConfig, User, UserTrait, add_synthetic_to_position,
    check_synthetic_asset, init_by_dispatcher, init_position, init_position_with_owner,
    setup_state_with_active_asset, setup_state_with_pending_asset,
    setup_state_with_pending_vault_share, validate_asset_balance, validate_balance,
};
use snforge_std::cheatcodes::events::{EventSpyTrait, EventsFilterTrait};
use snforge_std::{start_cheat_block_timestamp_global, test_address};
use starknet::get_block_info;
use starknet::storage::{StoragePathEntry, StoragePointerReadAccess};
use starkware_utils::components::replaceability::interface::IReplaceable;
use starkware_utils::components::request_approvals::interface::{IRequestApprovals, RequestStatus};
use starkware_utils::components::roles::interface::IRoles;
use starkware_utils::constants::{HOUR, MAX_U128};
use starkware_utils::hash::message_hash::OffchainMessageHash;
use starkware_utils::math::abs::Abs;
use starkware_utils::storage::iterable_map::*;
use starkware_utils::time::time::{Time, TimeDelta, Timestamp};
use starkware_utils_testing::test_utils::{
    Deployable, TokenTrait, assert_panic_with_felt_error, cheat_caller_address_once,
};
use crate::tests::event_test_utils::{
    assert_add_spot_event_with_expected, assert_forced_trade_event_with_expected,
    assert_forced_trade_request_event_with_expected,
};
use crate::tests::test_utils::init_state;


#[test]
fn test_constructor() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = init_state(cfg: @cfg, token_state: @token_state);
    assert!(state.roles.is_governance_admin(GOVERNANCE_ADMIN()));
    assert!(state.replaceability.get_upgrade_delay() == UPGRADE_DELAY);
    assert!(state.assets.get_max_price_interval() == MAX_PRICE_INTERVAL);
    assert!(state.assets.get_max_funding_interval() == MAX_FUNDING_INTERVAL);
    assert!(state.assets.get_max_funding_rate() == MAX_FUNDING_RATE);
    assert!(state.assets.get_max_oracle_price_validity() == MAX_ORACLE_PRICE_VALIDITY);
    assert!(state.deposits.get_cancel_delay() == CANCEL_DELAY);
    assert!(state.assets.get_last_funding_tick() == Time::now());
    assert!(state.assets.get_last_price_validation() == Time::now());

    assert!(
        state
            .positions
            .get_position_mut(position_id: FEE_POSITION)
            .get_owner_public_key() == OPERATOR_PUBLIC_KEY(),
    );
    assert!(
        state
            .positions
            .get_position_mut(position_id: INSURANCE_FUND_POSITION)
            .get_owner_public_key() == OPERATOR_PUBLIC_KEY(),
    );
}

#[test]
#[feature("safe_dispatcher")]
fn test_expiration_validation() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let user: User = Default::default();
    let position_id = user.position_id;

    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);
    let dispatcher = ICoreSafeDispatcher { contract_address };
    let position_dispatcher = IPositionsDispatcher { contract_address };
    let deposit_dispatcher = IDepositDispatcher { contract_address };

    // Create a position.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    position_dispatcher
        .new_position(
            operator_nonce: 0,
            :position_id,
            owner_public_key: user.get_public_key(),
            owner_account: Zero::zero(),
            owner_protection_enabled: true,
        );

    // Deposit money for user.
    let amount = 1000_u64;
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state
        .approve(
            owner: user.address,
            spender: contract_address,
            amount: amount.into() * cfg.collateral_cfg.quantum.into(),
        );

    cheat_caller_address_once(:contract_address, caller_address: user.address);
    deposit_dispatcher.deposit(:position_id, quantized_amount: amount, salt: user.salt_counter);

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    deposit_dispatcher
        .process_deposit(
            operator_nonce: 1,
            depositor: user.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: amount,
            salt: user.salt_counter,
        );

    // Test:

    let mut withdraw_args = WithdrawArgs {
        position_id,
        salt: user.salt_counter,
        expiration: Time::now(),
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount,
        recipient: user.address,
    };

    // Healthy scenario.
    let hash = withdraw_args.get_message_hash(user.get_public_key());
    let signature = user.sign_message(hash);

    cheat_caller_address_once(:contract_address, caller_address: user.address);
    dispatcher
        .withdraw_request(
            :signature,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        )
        .unwrap();

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    dispatcher
        .withdraw(
            operator_nonce: 2,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        )
        .unwrap();

    // Invalid expiration.
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::seconds(1)).into(),
    );

    withdraw_args.salt = user.salt_counter + 1;
    let hash = withdraw_args.get_message_hash(user.get_public_key());
    let signature = user.sign_message(hash);

    cheat_caller_address_once(:contract_address, caller_address: user.address);
    dispatcher
        .withdraw_request(
            :signature,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        )
        .unwrap();
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    let result = dispatcher
        .withdraw(
            operator_nonce: 3,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );
    assert_panic_with_felt_error(:result, expected_error: SIGNED_TX_EXPIRED);
}

#[test]
#[feature("safe_dispatcher")]
fn test_signature_validation() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let dispatcher = ICoreSafeDispatcher { contract_address };
    let asset_dispatcher = IAssetsDispatcher { contract_address };
    let assets_manager_dispatcher = IAssetsManagerDispatcher { contract_address };
    let deposit_dispatcher = IDepositDispatcher { contract_address };
    let position_dispatcher = IPositionsDispatcher { contract_address };

    let user_a: User = Default::default();
    let user_b = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    let collateral_id = cfg.collateral_cfg.collateral_id;
    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();
    let synthetic_id_2 = SYNTHETIC_ASSET_ID_2();

    let risk_factor_first_tier_boundary = MAX_U128;
    let risk_factor_tier_size = 1;
    let risk_factor_tiers = array![10].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    let oracle_price: u128 = ORACLE_PRICE;
    let asset_name = 'ASSET_NAME';
    let oracle1_name = 'ORCL1';
    let oracle1 = Oracle { oracle_name: oracle1_name, asset_name, key_pair: KEY_PAIR_1() };
    let old_time: u64 = Time::now().into();
    let new_time = Time::now().add(delta: MAX_ORACLE_PRICE_VALIDITY);
    start_cheat_block_timestamp_global(block_timestamp: new_time.into());

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    assets_manager_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    assets_manager_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_2,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    // Add to oracle.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    assets_manager_dispatcher
        .add_oracle_to_asset(
            asset_id: synthetic_id_1,
            oracle_public_key: oracle1.key_pair.public_key,
            oracle_name: oracle1_name,
            :asset_name,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    assets_manager_dispatcher
        .add_oracle_to_asset(
            asset_id: synthetic_id_2,
            oracle_public_key: oracle1.key_pair.public_key,
            oracle_name: oracle1_name,
            :asset_name,
        );

    // Activate synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    asset_dispatcher
        .price_tick(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            :oracle_price,
            signed_prices: [
                oracle1.get_signed_price(:oracle_price, timestamp: old_time.try_into().unwrap())
            ]
                .span(),
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    asset_dispatcher
        .price_tick(
            operator_nonce: 1,
            asset_id: synthetic_id_2,
            :oracle_price,
            signed_prices: [
                oracle1.get_signed_price(:oracle_price, timestamp: old_time.try_into().unwrap())
            ]
                .span(),
        );

    // Add positions, so signatures can be checked.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    position_dispatcher
        .new_position(
            operator_nonce: 2,
            position_id: POSITION_ID_100,
            owner_public_key: KEY_PAIR_1().public_key,
            owner_account: Zero::zero(),
            owner_protection_enabled: true,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    position_dispatcher
        .new_position(
            operator_nonce: 3,
            position_id: POSITION_ID_200,
            owner_public_key: KEY_PAIR_2().public_key,
            owner_account: Zero::zero(),
            owner_protection_enabled: true,
        );

    // Deposit money for users.
    let amount = 1000_u64;
    token_state.fund(recipient: user_a.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.fund(recipient: user_b.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state
        .approve(
            owner: user_a.address,
            spender: contract_address,
            amount: amount.into() * cfg.collateral_cfg.quantum.into(),
        );
    token_state
        .approve(
            owner: user_b.address,
            spender: contract_address,
            amount: amount.into() * cfg.collateral_cfg.quantum.into(),
        );

    cheat_caller_address_once(:contract_address, caller_address: user_a.address);
    deposit_dispatcher
        .deposit(
            position_id: user_a.position_id, quantized_amount: amount, salt: user_a.salt_counter,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    deposit_dispatcher
        .process_deposit(
            operator_nonce: 4,
            depositor: user_a.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user_a.position_id,
            quantized_amount: amount,
            salt: user_a.salt_counter,
        );

    cheat_caller_address_once(:contract_address, caller_address: user_b.address);
    deposit_dispatcher
        .deposit(
            position_id: user_b.position_id, quantized_amount: amount, salt: user_b.salt_counter,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    deposit_dispatcher
        .process_deposit(
            operator_nonce: 5,
            depositor: user_b.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user_b.position_id,
            quantized_amount: amount,
            salt: user_b.salt_counter,
        );

    // Build orders.
    let mut order_a = Order {
        position_id: POSITION_ID_100,
        base_asset_id: synthetic_id_1,
        base_amount: 1,
        quote_asset_id: collateral_id,
        quote_amount: -1,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration: Time::now(),
        salt: 0,
    };

    let mut order_b = Order {
        position_id: POSITION_ID_200,
        base_asset_id: synthetic_id_1,
        base_amount: -1,
        quote_asset_id: collateral_id,
        quote_amount: 1,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration: Time::now(),
        salt: 0,
    };

    // Test:

    // Send empty signature.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    let result = dispatcher
        .trade(
            operator_nonce: 6,
            signature_a: array![].span(),
            signature_b: array![].span(),
            :order_a,
            :order_b,
            actual_amount_base_a: 1,
            actual_amount_quote_a: -1,
            actual_fee_a: 0,
            actual_fee_b: 0,
        );
    assert_panic_with_felt_error(:result, expected_error: 'INVALID_STARK_KEY_SIGNATURE');

    let hash_a = order_a.get_message_hash(user_a.get_public_key());
    let signature_a = user_a.sign_message(hash_a);
    let hash_b = order_b.get_message_hash(user_b.get_public_key());
    let signature_b = user_b.sign_message(hash_b);

    // Send Correct signature.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    dispatcher
        .trade(
            operator_nonce: 7,
            :signature_a,
            :signature_b,
            :order_a,
            :order_b,
            actual_amount_base_a: 1,
            actual_amount_quote_a: -1,
            actual_fee_a: 0,
            actual_fee_b: 0,
        )
        .unwrap();

    let hash_a = order_a.get_message_hash(KEY_PAIR_2().public_key);
    let signature_a = user_a.sign_message(hash_a);

    // Send a signature created by different key.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    let result = dispatcher
        .trade(
            operator_nonce: 8,
            :signature_a,
            :signature_b,
            :order_a,
            :order_b,
            actual_amount_base_a: 1,
            actual_amount_quote_a: -1,
            actual_fee_a: 0,
            actual_fee_b: 0,
        );
    assert_panic_with_felt_error(:result, expected_error: 'INVALID_STARK_KEY_SIGNATURE');

    let hash_a = order_a.get_message_hash(user_a.get_public_key());
    let signature_a = user_a.sign_message(hash_a);
    order_a.base_asset_id = synthetic_id_2;
    order_b.base_asset_id = synthetic_id_2;

    // Send different order message, than the signed one.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    let result = dispatcher
        .trade(
            operator_nonce: 9,
            :signature_a,
            :signature_b,
            :order_a,
            :order_b,
            actual_amount_base_a: 1,
            actual_amount_quote_a: -1,
            actual_fee_a: 0,
            actual_fee_b: 0,
        );
    assert_panic_with_felt_error(:result, expected_error: 'INVALID_STARK_KEY_SIGNATURE');

    // Revert the previous change.
    order_a.base_asset_id = synthetic_id_1;
    order_b.base_asset_id = synthetic_id_1;
    // Change the salt.
    order_a.salt = 123;

    // Send different order message, than the signed one.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    let result = dispatcher
        .trade(
            operator_nonce: 10,
            :signature_a,
            :signature_b,
            :order_a,
            :order_b,
            actual_amount_base_a: 1,
            actual_amount_quote_a: -1,
            actual_fee_a: 0,
            actual_fee_b: 0,
        );
    assert_panic_with_felt_error(:result, expected_error: 'INVALID_STARK_KEY_SIGNATURE');
}

// New position tests.

#[test]
fn test_new_position() {
    // Setup state, token:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let mut spy = snforge_std::spy_events();

    // Parameters:
    let position_id = POSITION_ID_100;
    let owner_public_key = KEY_PAIR_1().public_key;
    let owner_account = POSITION_OWNER_1();

    // Test.
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .new_position(
            operator_nonce: state.get_operator_nonce(),
            :position_id,
            :owner_public_key,
            :owner_account,
            owner_protection_enabled: true,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_new_position_event_with_expected(
        spied_event: events[0], :position_id, :owner_public_key, :owner_account,
    );

    // Check.
    assert!(state.positions.get_position_mut(:position_id).get_version() == POSITION_VERSION);
    assert!(
        state.positions.get_position_mut(:position_id).get_owner_public_key() == owner_public_key,
    );
    assert!(
        state
            .positions
            .get_position_mut(:position_id)
            .get_owner_account()
            .unwrap() == owner_account,
    );

    let position_tv_tr = state.positions.get_position_tv_tr(:position_id);
    assert!(position_tv_tr.total_value.is_zero());
    assert!(position_tv_tr.total_risk.is_zero());
}

// Set owner account tests.

#[test]
fn test_successful_set_owner_account_request_using_public_key() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position(cfg: @cfg, ref :state, :user);

    // Setup parameters:
    let expected_time = Time::now().add(delta: Time::days(1));
    start_cheat_block_timestamp_global(block_timestamp: expected_time.into());
    let expiration = expected_time.add(delta: Time::days(1));

    let set_owner_account_args = SetOwnerAccountArgs {
        public_key: user.get_public_key(),
        new_owner_account: user.address,
        position_id: user.position_id,
        expiration,
    };
    let msg_hash = set_owner_account_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(msg_hash);

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .positions
        .set_owner_account_request(
            :signature, position_id: user.position_id, new_owner_account: user.address, :expiration,
        );

    // Check:
    let status = state.request_approvals.get_request_status(request_hash: msg_hash);
    assert!(status == RequestStatus::PENDING);
}

#[test]
#[should_panic(expected: 'CALLER_IS_NOT_OWNER_ACCOUNT')]
fn test_set_owner_account_request_invalid_caller() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position(cfg: @cfg, ref :state, :user);

    // Setup parameters:
    let expected_time = Time::now().add(delta: Time::days(1));
    start_cheat_block_timestamp_global(block_timestamp: expected_time.into());
    let expiration = expected_time.add(delta: Time::days(1));

    let set_owner_account_args = SetOwnerAccountArgs {
        public_key: user.get_public_key(),
        new_owner_account: user.address,
        position_id: user.position_id,
        expiration,
    };
    let msg_hash = set_owner_account_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(msg_hash);

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .positions
        .set_owner_account_request(
            :signature, position_id: user.position_id, new_owner_account: user.address, :expiration,
        );
}

#[test]
#[should_panic(expected: 'POSITION_HAS_OWNER_ACCOUNT')]
fn test_set_owner_account_request_position_has_owner() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position_with_owner(cfg: @cfg, ref :state, :user);

    // Setup parameters:
    let expected_time = Time::now().add(delta: Time::days(1));
    start_cheat_block_timestamp_global(block_timestamp: expected_time.into());
    let expiration = expected_time.add(delta: Time::days(1));

    let set_owner_account_args = SetOwnerAccountArgs {
        public_key: user.get_public_key(),
        new_owner_account: user.address,
        position_id: user.position_id,
        expiration,
    };
    let msg_hash = set_owner_account_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(msg_hash);

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .positions
        .set_owner_account_request(
            :signature, position_id: user.position_id, new_owner_account: user.address, :expiration,
        );
}

#[test]
fn test_successful_set_owner_account() {
    // Setup state, token:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let user: User = Default::default();
    init_position(cfg: @cfg, ref :state, :user);

    // Parameters:
    let position_id = user.position_id;
    let public_key = user.get_public_key();
    let new_owner_account = user.address;
    let expiration = Time::now().add(Time::days(1));

    let set_owner_account_args = SetOwnerAccountArgs {
        position_id, public_key, new_owner_account, expiration,
    };
    let set_owner_account_hash = set_owner_account_args.get_message_hash(user.get_public_key());
    let signature = user.sign_message(set_owner_account_hash);
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .positions
        .set_owner_account_request(:signature, :position_id, :new_owner_account, :expiration);

    // Test.
    let mut spy = snforge_std::spy_events();
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .positions
        .set_owner_account(
            operator_nonce: state.get_operator_nonce(),
            :position_id,
            :new_owner_account,
            :expiration,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_set_owner_account_event_with_expected(
        spied_event: events[0],
        :position_id,
        :public_key,
        :new_owner_account,
        :set_owner_account_hash,
    );

    // Check.
    assert!(
        state
            .positions
            .get_position_mut(:position_id)
            .get_owner_account()
            .unwrap() == new_owner_account,
    );
    let status = state.request_approvals.get_request_status(request_hash: set_owner_account_hash);
    assert!(status == RequestStatus::PROCESSED);
}

#[test]
#[should_panic(expected: 'POSITION_HAS_OWNER_ACCOUNT')]
fn test_set_existed_owner_account() {
    // Setup state, token:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let user: User = Default::default();
    init_position_with_owner(cfg: @cfg, ref :state, :user);

    // Parameters:
    let position_id = POSITION_ID_100;
    let new_owner_account = POSITION_OWNER_1();
    let expiration = Time::now().add(Time::days(1));

    // Test.

    let set_owner_account_args = SetOwnerAccountArgs {
        public_key: user.get_public_key(),
        new_owner_account: user.address,
        position_id: user.position_id,
        expiration,
    };
    let msg_hash = set_owner_account_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(msg_hash);
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .positions
        .set_owner_account_request(:signature, :position_id, :new_owner_account, :expiration);
}

// Add synthetic asset tests.

#[test]
fn test_successful_add_synthetic_asset() {
    // Setup state, token:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let mut spy = snforge_std::spy_events();

    // Setup test parameters:
    let synthetic_id_1 = SYNTHETIC_ASSET_ID_2();
    let synthetic_id_2 = SYNTHETIC_ASSET_ID_3();
    let risk_factor_first_tier_boundary = MAX_U128;
    let risk_factor_tier_size = 1;
    let risk_factor_1 = array![10].span();
    let risk_factor_2 = array![20].span();
    let quorum_1 = 1_u8;
    let quorum_2 = 2_u8;
    let resolution_1 = 1_000_000_000;
    let resolution_2 = 2_000_000_000;

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_1,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            quorum: quorum_1,
            resolution_factor: resolution_1,
        );

    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_synthetic_asset(
            asset_id: synthetic_id_2,
            risk_factor_tiers: risk_factor_2,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            quorum: quorum_2,
            resolution_factor: resolution_2,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_add_synthetic_event_with_expected(
        spied_event: events[0],
        asset_id: synthetic_id_1,
        risk_factor_tiers: risk_factor_1,
        :risk_factor_first_tier_boundary,
        :risk_factor_tier_size,
        resolution_factor: resolution_1,
        quorum: quorum_1,
    );

    // Check:
    check_synthetic_asset(
        state: @state,
        synthetic_id: synthetic_id_1,
        status: AssetStatus::PENDING,
        risk_factor_tiers: risk_factor_1,
        :risk_factor_first_tier_boundary,
        :risk_factor_tier_size,
        quorum: quorum_1,
        resolution_factor: resolution_1,
        price: Zero::zero(),
        last_price_update: Zero::zero(),
        funding_index: Zero::zero(),
    );
    check_synthetic_asset(
        state: @state,
        synthetic_id: synthetic_id_2,
        status: AssetStatus::PENDING,
        risk_factor_tiers: risk_factor_2,
        :risk_factor_first_tier_boundary,
        :risk_factor_tier_size,
        quorum: quorum_2,
        resolution_factor: resolution_2,
        price: Zero::zero(),
        last_price_update: Zero::zero(),
        funding_index: Zero::zero(),
    );
}

#[test]
#[should_panic(expected: 'SYNTHETIC_ALREADY_EXISTS')]
fn test_add_synthetic_asset_existed_asset() {
    // Setup state, token:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_synthetic_asset(
            // Setup state already added `SYNTHETIC_ASSET_ID_1`.
            asset_id: SYNTHETIC_ASSET_ID_1(),
            risk_factor_tiers: array![10].span(),
            risk_factor_first_tier_boundary: MAX_U128,
            risk_factor_tier_size: 1,
            quorum: 13,
            resolution_factor: 10000000,
        );
}
// Update risk factor

#[test]
#[feature("safe_dispatcher")]
fn test_rf_update_valid_same_array() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tiers = array![1, 2, 3, 5, 10, 20, 40].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
        );
}

#[test]
#[feature("safe_dispatcher")]
fn test_rf_update_valid_same_short_array() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tiers = array![1, 2].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
        );
}


#[test]
#[feature("safe_dispatcher")]
#[should_panic(expected: 'INVALID_RF_VALUE')]
fn test_rf_update_invalid_same_short_array() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tiers = array![1, 2].span();
    let risk_factor_tiers_2 = array![1, 3].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
        );
}

#[test]
#[feature("safe_dispatcher")]
#[should_panic(expected: 'INVALID_RF_VALUE')]
fn test_rf_update_invalid_super_short_array() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tiers = array![1].span();
    let risk_factor_tiers_2 = array![2].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
        );
}

#[test]
#[feature("safe_dispatcher")]
fn test_rf_update_valid_super_short_array() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tiers = array![1].span();
    let risk_factor_tiers_2 = array![1].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
        );
}


#[test]
#[feature("safe_dispatcher")]
fn test_rf_update_valid_same_super_short_array_increase() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tiers = array![5].span();
    let risk_factor_tiers_2 = array![1, 2, 5].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
        );
}

#[test]
#[feature("safe_dispatcher")]
#[should_panic(expected: 'INVALID_RF_VALUE')]
fn test_rf_update_invalid_same_short_array_increase() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tiers = array![5].span();
    let risk_factor_tiers_2 = array![1, 2, 6].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
        );
}

#[test]
#[feature("safe_dispatcher")]
fn test_rf_update_valid_lower_array() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsDispatcher { contract_address };
    let assets_manager_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tiers = array![1, 2, 3, 5, 10, 20, 40].span();
    let risk_factor_tiers_2 = array![1, 2, 3, 4, 5, 10, 20].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    assets_manager_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    assets_manager_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
        );

    let synthetic_config = asset_dispatcher.get_asset_config(asset_id: synthetic_id_1);
    let tiers = asset_dispatcher.get_risk_factor_tiers(asset_id: synthetic_id_1);
    for i in 0..risk_factor_tiers_2.len() {
        assert!(*tiers[i] == RiskFactorTrait::new(*risk_factor_tiers_2[i]));
    }
    assert!(synthetic_config.risk_factor_first_tier_boundary == risk_factor_first_tier_boundary);
    assert!(synthetic_config.risk_factor_tier_size == risk_factor_tier_size);
}

#[test]
#[feature("safe_dispatcher")]
#[should_panic(expected: 'INVALID_RF_VALUE')]
fn test_rf_update_invalid_higher_last_element_array() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tiers = array![1, 2, 3, 5, 10, 20, 40].span();
    let risk_factor_tiers_2 = array![1, 1, 1, 1, 5, 10, 41].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
        );
}

#[test]
#[feature("safe_dispatcher")]
#[should_panic(expected: 'INVALID_RF_VALUE')]
fn test_rf_update_invalid_median_last_element_array() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tiers = array![1, 2, 3, 5, 10, 20, 40].span();
    let risk_factor_tiers_2 = array![1, 2, 3, 6, 10, 20, 40].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
        );
}

#[test]
#[feature("safe_dispatcher")]
fn test_rf_update_valid_more_frequent_array() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tier_size_2 = 10_000;
    let risk_factor_tiers = array![1, 4, 6, 10, 20, 40, 80].span();
    let risk_factor_tiers_2 = array![1, 2, 3, 4, 5, 6, 9, 10, 19, 20, 39, 40, 79, 80].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            :risk_factor_first_tier_boundary,
            risk_factor_tier_size: risk_factor_tier_size_2,
        );
}


#[test]
#[feature("safe_dispatcher")]
#[should_panic(expected: 'INVALID_RF_VALUE')]
fn test_rf_update_invalid_more_frequent_array() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);
    let mut spy = snforge_std::spy_events();

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tier_size_2 = 10_000;
    let risk_factor_tiers = array![1, 4, 6, 10, 20, 40, 80].span();
    let risk_factor_tiers_2 = array![1, 2, 3, 4, 5, 7, 9, 10, 19, 20, 41, 80].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            :risk_factor_first_tier_boundary,
            risk_factor_tier_size: risk_factor_tier_size_2,
        );
    let events = spy.get_events().emitted_by(contract_address).events;
    assert_change_synthetic_event_with_expected(
        spied_event: events[1],
        asset_id: synthetic_id_1,
        risk_factor_tiers: risk_factor_tiers_2,
        risk_factor_first_tier_boundary: risk_factor_first_tier_boundary,
        risk_factor_tier_size: risk_factor_tier_size_2,
        resolution_factor: resolution_factor,
        quorum: quorum,
    );
}

#[test]
#[feature("safe_dispatcher")]
fn test_rf_update_valid_less_frequent_array() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tier_size_2 = 40_000;
    let risk_factor_tiers = array![1, 2, 3, 5, 10, 20, 40].span();
    let risk_factor_tiers_2 = array![1, 2, 5].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            :risk_factor_first_tier_boundary,
            risk_factor_tier_size: risk_factor_tier_size_2,
        );
}

#[test]
#[feature("safe_dispatcher")]
#[should_panic(expected: 'INVALID_RF_VALUE')]
fn test_rf_update_invalid_less_frequent_array() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tier_size_2 = 40_000;
    let risk_factor_tiers = array![1, 2, 3, 5, 10, 20, 40].span();
    let risk_factor_tiers_2 = array![1, 2, 10].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            :risk_factor_first_tier_boundary,
            risk_factor_tier_size: risk_factor_tier_size_2,
        );
}


#[test]
#[feature("safe_dispatcher")]
fn test_rf_update_valid_different_step_size() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);
    let mut spy = snforge_std::spy_events();

    let asset_dispatcher = IAssetsDispatcher { contract_address };
    let assets_manager_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_first_tier_boundary2 = 10_001;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tier_size_2 = 40_000;
    let risk_factor_tiers = array![1, 2, 3, 5, 10, 20, 40].span();
    let risk_factor_tiers_2 = array![1, 2, 3, 5, 10, 20, 40].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    assets_manager_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    assets_manager_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            risk_factor_first_tier_boundary: risk_factor_first_tier_boundary2,
            risk_factor_tier_size: risk_factor_tier_size_2,
        );

    let synthetic_config = asset_dispatcher.get_asset_config(asset_id: synthetic_id_1);
    let tiers = asset_dispatcher.get_risk_factor_tiers(asset_id: synthetic_id_1);
    for i in 0..risk_factor_tiers_2.len() {
        assert!(*tiers[i] == RiskFactorTrait::new(*risk_factor_tiers_2[i]));
    }
    assert!(synthetic_config.risk_factor_tier_size == risk_factor_tier_size_2);
    assert!(synthetic_config.risk_factor_first_tier_boundary == risk_factor_first_tier_boundary2);

    let events = spy.get_events().emitted_by(contract_address).events;

    assert_change_synthetic_event_with_expected(
        spied_event: events[1],
        asset_id: synthetic_id_1,
        risk_factor_tiers: risk_factor_tiers_2,
        risk_factor_first_tier_boundary: risk_factor_first_tier_boundary2,
        risk_factor_tier_size: risk_factor_tier_size_2,
        resolution_factor: resolution_factor,
        quorum: quorum,
    );
}


#[test]
#[feature("safe_dispatcher")]
#[should_panic(expected: 'INVALID_RF_VALUE')]
fn test_rf_update_invalid_different_step_size() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);

    let asset_dispatcher = IAssetsManagerDispatcher { contract_address };

    let synthetic_id_1 = SYNTHETIC_ASSET_ID_1();

    let risk_factor_first_tier_boundary = 10_000;
    let risk_factor_tier_size = 20_000;
    let risk_factor_tier_size_2 = 10_000;
    let risk_factor_tiers = array![1, 2, 3, 5, 10, 20, 40].span();
    let risk_factor_tiers_2 = array![1, 2, 3, 5, 10, 20, 40].span();
    let quorum = 1_u8;
    let resolution_factor = 2_000_000_000;

    // Add synthetic assets.
    cheat_caller_address_once(:contract_address, caller_address: cfg.app_governor);
    asset_dispatcher
        .add_synthetic_asset(
            asset_id: synthetic_id_1,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
            :resolution_factor,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    // Test:
    asset_dispatcher
        .update_asset_risk_factor(
            operator_nonce: 0,
            asset_id: synthetic_id_1,
            risk_factor_tiers: risk_factor_tiers_2,
            :risk_factor_first_tier_boundary,
            risk_factor_tier_size: risk_factor_tier_size_2,
        );
}


// Deactivate synthetic asset tests.

#[test]
fn test_successful_deactivate_synthetic_asset() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let mut spy = snforge_std::spy_events();

    // Setup parameters:
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;
    assert!(
        state.assets.asset_config.entry(synthetic_id).read().unwrap().status == AssetStatus::ACTIVE,
    );

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state.deactivate_synthetic(:synthetic_id);

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_deactivate_synthetic_asset_event_with_expected(
        spied_event: events[0], asset_id: synthetic_id,
    );

    // Check:
    assert!(
        state
            .assets
            .asset_config
            .entry(synthetic_id)
            .read()
            .unwrap()
            .status == AssetStatus::INACTIVE,
    );
}

#[test]
#[should_panic(expected: 'SYNTHETIC_NOT_EXISTS')]
fn test_deactivate_nonexistent_synthetic_asset() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    // Setup parameters:
    let synthetic_id = SYNTHETIC_ASSET_ID_2();

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state.deactivate_synthetic(:synthetic_id);
}


// Deposit tests.

#[test]
fn test_successful_deposit() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    let user_deposit_amount = DEPOSIT_AMOUNT.into() * cfg.collateral_cfg.quantum.into();
    init_position(cfg: @cfg, ref :state, :user);

    // Fund user.
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user.address, spender: test_address(), amount: user_deposit_amount);

    // Setup parameters:
    let expected_time = Time::now().add(delta: Time::days(1));
    start_cheat_block_timestamp_global(block_timestamp: expected_time.into());

    // Check before deposit:
    validate_balance(token_state, user.address, USER_INIT_BALANCE.try_into().unwrap());
    validate_balance(token_state, test_address(), CONTRACT_INIT_BALANCE.try_into().unwrap());
    let mut spy = snforge_std::spy_events();

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit(
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
    let deposit_hash = deposit_hash(
        token_address: token_state.address,
        depositor: user.address,
        position_id: user.position_id,
        quantized_amount: DEPOSIT_AMOUNT,
        salt: user.salt_counter,
    );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_deposit_event_with_expected(
        spied_event: events[0],
        position_id: user.position_id,
        depositing_address: user.address,
        collateral_id: cfg.collateral_cfg.collateral_id,
        quantized_amount: DEPOSIT_AMOUNT,
        unquantized_amount: DEPOSIT_AMOUNT * COLLATERAL_QUANTUM,
        deposit_request_hash: deposit_hash,
        salt: user.salt_counter,
    );

    // Check after deposit:
    validate_balance(
        token_state, user.address, (USER_INIT_BALANCE - user_deposit_amount).try_into().unwrap(),
    );
    validate_balance(
        token_state,
        test_address(),
        (CONTRACT_INIT_BALANCE + user_deposit_amount).try_into().unwrap(),
    );
    let status = state.deposits.get_deposit_status(:deposit_hash);
    if let DepositStatus::PENDING(timestamp) = status {
        assert!(timestamp == expected_time);
    } else {
        panic!("Deposit not found");
    }
}

#[test]
#[should_panic(expected: 'DEPOSIT_ALREADY_REGISTERED')]
fn test_deposit_already_registered() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    let user_deposit_amount = DEPOSIT_AMOUNT.into() * cfg.collateral_cfg.quantum.into();
    init_position(cfg: @cfg, ref :state, :user);

    // Fund user.
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user.address, spender: test_address(), amount: user_deposit_amount);

    // Setup parameters:
    let expected_time = Time::now().add(delta: Time::days(1));
    start_cheat_block_timestamp_global(block_timestamp: expected_time.into());

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit(
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
    state
        .deposit(
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
}

#[test]
fn test_successful_process_deposit() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    let user_deposit_amount = DEPOSIT_AMOUNT.into() * cfg.collateral_cfg.quantum.into();

    // Fund user.
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user.address, spender: test_address(), amount: user_deposit_amount);

    // Setup parameters:
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::seconds(1000)).into(),
    );

    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit(
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
    let deposit_hash = deposit_hash(
        token_address: token_state.address,
        depositor: user.address,
        position_id: user.position_id,
        quantized_amount: DEPOSIT_AMOUNT,
        salt: user.salt_counter,
    );
    let mut spy = snforge_std::spy_events();

    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .process_deposit(
            operator_nonce: state.get_operator_nonce(),
            depositor: user.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_deposit_processed_event_with_expected(
        spied_event: events[0],
        position_id: user.position_id,
        depositing_address: user.address,
        collateral_id: cfg.collateral_cfg.collateral_id,
        quantized_amount: DEPOSIT_AMOUNT,
        unquantized_amount: DEPOSIT_AMOUNT * COLLATERAL_QUANTUM,
        deposit_request_hash: deposit_hash,
        salt: user.salt_counter,
    );

    let status = state.deposits.get_deposit_status(:deposit_hash);
    assert!(status == DepositStatus::PROCESSED, "Deposit not processed");
}

// Cancel deposit tests.

#[test]
fn test_successful_cancel_deposit() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    let user_deposit_amount = DEPOSIT_AMOUNT.into() * cfg.collateral_cfg.quantum.into();

    // Fund user.
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user.address, spender: test_address(), amount: user_deposit_amount);

    // Setup parameters:
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::days(1)).into(),
    );
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit(
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
    let deposit_hash = deposit_hash(
        token_address: token_state.address,
        depositor: user.address,
        position_id: user.position_id,
        quantized_amount: DEPOSIT_AMOUNT,
        salt: user.salt_counter,
    );
    let mut spy = snforge_std::spy_events();

    // Check before cancel deposit:
    validate_balance(
        token_state, user.address, (USER_INIT_BALANCE - user_deposit_amount).try_into().unwrap(),
    );
    validate_balance(
        token_state,
        test_address(),
        (CONTRACT_INIT_BALANCE + user_deposit_amount).try_into().unwrap(),
    );

    // Test:
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::weeks(2)).into(),
    );
    state
        .cancel_deposit(
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_deposit_canceled_event_with_expected(
        spied_event: events[0],
        position_id: user.position_id,
        depositing_address: user.address,
        collateral_id: cfg.collateral_cfg.collateral_id,
        quantized_amount: DEPOSIT_AMOUNT,
        unquantized_amount: DEPOSIT_AMOUNT * COLLATERAL_QUANTUM,
        deposit_request_hash: deposit_hash,
        salt: user.salt_counter,
    );

    // Check after deposit cancellation:
    validate_balance(token_state, user.address, USER_INIT_BALANCE.try_into().unwrap());
    validate_balance(token_state, test_address(), CONTRACT_INIT_BALANCE.try_into().unwrap());
}

#[test]
fn test_successful_reject_deposit() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    let user_deposit_amount = DEPOSIT_AMOUNT.into() * cfg.collateral_cfg.quantum.into();

    // Fund user.
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user.address, spender: test_address(), amount: user_deposit_amount);

    // Setup parameters:
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::days(1)).into(),
    );
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit(
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
    let deposit_hash = deposit_hash(
        token_address: token_state.address,
        depositor: user.address,
        position_id: user.position_id,
        quantized_amount: DEPOSIT_AMOUNT,
        salt: user.salt_counter,
    );
    let mut spy = snforge_std::spy_events();

    // Check before cancel deposit:
    validate_balance(
        token_state, user.address, (USER_INIT_BALANCE - user_deposit_amount).try_into().unwrap(),
    );
    validate_balance(
        token_state,
        test_address(),
        (CONTRACT_INIT_BALANCE + user_deposit_amount).try_into().unwrap(),
    );

    // Test:
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::weeks(2)).into(),
    );

    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .reject_deposit(
            operator_nonce: state.get_operator_nonce(),
            depositor: user.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_deposit_canceled_event_with_expected(
        spied_event: events[0],
        position_id: user.position_id,
        depositing_address: user.address,
        collateral_id: cfg.collateral_cfg.collateral_id,
        quantized_amount: DEPOSIT_AMOUNT,
        unquantized_amount: DEPOSIT_AMOUNT * COLLATERAL_QUANTUM,
        deposit_request_hash: deposit_hash,
        salt: user.salt_counter,
    );

    // Check after deposit cancellation:
    validate_balance(token_state, user.address, USER_INIT_BALANCE.try_into().unwrap());
    validate_balance(token_state, test_address(), CONTRACT_INIT_BALANCE.try_into().unwrap());
}

#[test]
#[should_panic(expected: 'DEPOSIT_NOT_REGISTERED')]
fn test_cancel_non_registered_deposit() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position(cfg: @cfg, ref :state, :user);

    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .cancel_deposit(
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
}

#[test]
#[should_panic(expected: 'DEPOSIT_NOT_REGISTERED')]
fn test_cancel_deposit_different_hash() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    let user_deposit_amount = DEPOSIT_AMOUNT.into() * cfg.collateral_cfg.quantum.into();

    // Fund user.
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user.address, spender: test_address(), amount: user_deposit_amount);

    // Setup parameters:
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::days(1)).into(),
    );
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit(
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );

    state
        .cancel_deposit(
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter + 1,
        );
}

#[test]
#[should_panic(expected: 'DEPOSIT_ALREADY_PROCESSED')]
fn test_cancel_already_done_deposit() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    let user_deposit_amount = DEPOSIT_AMOUNT.into() * cfg.collateral_cfg.quantum.into();

    // Fund user.
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user.address, spender: test_address(), amount: user_deposit_amount);

    // Setup parameters:
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::seconds(1000)).into(),
    );

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit(
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );

    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .process_deposit(
            operator_nonce: state.get_operator_nonce(),
            depositor: user.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );

    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::weeks(2)).into(),
    );
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .cancel_deposit(
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
}

#[test]
#[should_panic(expected: 'DEPOSIT_ALREADY_CANCELED')]
fn test_double_cancel_deposit() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    let user_deposit_amount = DEPOSIT_AMOUNT.into() * cfg.collateral_cfg.quantum.into();

    // Fund user.
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user.address, spender: test_address(), amount: user_deposit_amount);

    // Setup parameters:
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::days(1)).into(),
    );

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit(
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );

    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::weeks(2)).into(),
    );
    state
        .cancel_deposit(
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
    state
        .cancel_deposit(
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
}

#[test]
#[should_panic(expected: 'DEPOSIT_NOT_CANCELABLE')]
fn test_cancel_deposit_before_cancellation_delay_passed() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    let user_deposit_amount = DEPOSIT_AMOUNT.into() * cfg.collateral_cfg.quantum.into();

    // Fund user.
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user.address, spender: test_address(), amount: user_deposit_amount);

    // Setup parameters:
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::days(1)).into(),
    );

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit(
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );

    state
        .cancel_deposit(
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
}

// Trade tests.

#[test]
fn test_successful_trade() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let user_a = Default::default();
    init_position(cfg: @cfg, ref :state, user: user_a);

    let user_b = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: user_b);

    // Test params:
    let BASE = 10;
    let QUOTE = -5;
    let FEE = 1;

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));

    let collateral_id = cfg.collateral_cfg.collateral_id;
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    let order_a = Order {
        position_id: user_a.position_id,
        salt: user_a.salt_counter,
        base_asset_id: synthetic_id,
        base_amount: BASE,
        quote_asset_id: collateral_id,
        quote_amount: QUOTE,
        fee_asset_id: collateral_id,
        fee_amount: FEE,
        expiration,
    };

    let order_b = Order {
        position_id: user_b.position_id,
        base_asset_id: synthetic_id,
        base_amount: -BASE,
        quote_asset_id: collateral_id,
        quote_amount: -QUOTE,
        fee_asset_id: collateral_id,
        fee_amount: FEE,
        expiration,
        salt: user_b.salt_counter,
    };

    let hash_a = order_a.get_message_hash(user_a.get_public_key());
    let hash_b = order_b.get_message_hash(user_b.get_public_key());
    let signature_a = user_a.sign_message(hash_a);
    let signature_b = user_b.sign_message(hash_b);
    let operator_nonce = state.get_operator_nonce();

    let mut spy = snforge_std::spy_events();
    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .trade(
            :operator_nonce,
            :signature_a,
            :signature_b,
            :order_a,
            :order_b,
            actual_amount_base_a: BASE,
            actual_amount_quote_a: QUOTE,
            actual_fee_a: FEE,
            actual_fee_b: FEE,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_trade_event_with_expected(
        spied_event: events[0],
        order_base_asset_id: synthetic_id,
        order_a_position_id: user_a.position_id,
        order_a_base_amount: BASE,
        order_a_quote_amount: QUOTE,
        fee_a_amount: FEE,
        :collateral_id,
        order_b_position_id: user_b.position_id,
        order_b_base_amount: -BASE,
        order_b_quote_amount: -QUOTE,
        fee_b_amount: FEE,
        actual_amount_base_a: BASE,
        actual_amount_quote_a: QUOTE,
        actual_fee_a: FEE,
        actual_fee_b: FEE,
        order_a_hash: hash_a,
        order_b_hash: hash_b,
    );

    // Check:
    let position_a = state.positions.get_position_snapshot(position_id: user_a.position_id);
    let user_a_collateral_balance = state
        .positions
        .get_collateral_provisional_balance(position: position_a, provisional_delta: Option::None);
    let user_a_synthetic_balance = state
        .positions
        .get_synthetic_balance(position: position_a, :synthetic_id);
    assert!(
        user_a_collateral_balance == (COLLATERAL_BALANCE_AMOUNT.into() - FEE.into() + QUOTE.into()),
    );
    assert!(user_a_synthetic_balance == (BASE).into());

    let position_b = state.positions.get_position_snapshot(position_id: user_b.position_id);
    let user_b_collateral_balance = state
        .positions
        .get_collateral_provisional_balance(position: position_b, provisional_delta: Option::None);
    let user_b_synthetic_balance = state
        .positions
        .get_synthetic_balance(position: position_b, :synthetic_id);
    assert!(
        user_b_collateral_balance == (COLLATERAL_BALANCE_AMOUNT.into() - FEE.into() - QUOTE.into()),
    );
    assert!(user_b_synthetic_balance == (-BASE).into());

    let position = state.positions.get_position_snapshot(position_id: FEE_POSITION);
    let fee_position_balance = state
        .positions
        .get_collateral_provisional_balance(:position, provisional_delta: Option::None);
    assert!(fee_position_balance == (FEE + FEE).into());
}

#[test]
#[should_panic(expected: 'INVALID_AMOUNT_SIGN')]
fn test_invalid_trade_same_base_signs() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let user_a = Default::default();
    init_position(cfg: @cfg, ref :state, user: user_a);

    let user_b = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: user_b);

    // Test params:
    let BASE = 10;
    let QUOTE = -5;
    let FEE = 1;

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));

    let collateral_id = cfg.collateral_cfg.collateral_id;
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    let order_a = Order {
        position_id: user_a.position_id,
        salt: user_a.salt_counter,
        base_asset_id: synthetic_id,
        base_amount: BASE,
        quote_asset_id: collateral_id,
        quote_amount: QUOTE,
        fee_asset_id: collateral_id,
        fee_amount: FEE,
        expiration,
    };

    // Wrong sign for base amount.
    let order_b = Order {
        position_id: user_b.position_id,
        salt: user_b.salt_counter,
        base_asset_id: synthetic_id,
        base_amount: BASE,
        quote_asset_id: collateral_id,
        quote_amount: -QUOTE,
        fee_asset_id: collateral_id,
        fee_amount: FEE,
        expiration,
    };

    let signature_a = user_a.sign_message(order_a.get_message_hash(user_a.get_public_key()));
    let signature_b = user_b.sign_message(order_b.get_message_hash(user_b.get_public_key()));
    let operator_nonce = state.get_operator_nonce();

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .trade(
            :operator_nonce,
            :signature_a,
            :signature_b,
            :order_a,
            :order_b,
            actual_amount_base_a: BASE,
            actual_amount_quote_a: QUOTE,
            actual_fee_a: FEE,
            actual_fee_b: FEE,
        );
}

#[test]
fn test_successful_withdraw_request_with_public_key() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    let recipient = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());

    // Setup parameters:
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::days(1)).into(),
    );
    let expiration = Time::now().add(delta: Time::days(1));

    let withdraw_args = WithdrawArgs {
        position_id: user.position_id,
        salt: user.salt_counter,
        expiration,
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount: WITHDRAW_AMOUNT,
        recipient: recipient.address,
    };
    let msg_hash = withdraw_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(message: msg_hash);

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .withdraw_request(
            :signature,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );

    // Check:
    let status = state.request_approvals.get_request_status(request_hash: msg_hash);
    assert!(status == RequestStatus::PENDING);
}

// Forced withdraw tests.

#[test]
fn test_successful_forced_withdraw_request() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);
    let dispatcher = ICoreDispatcher { contract_address };
    let position_dispatcher = IPositionsDispatcher { contract_address };

    let user: User = Default::default();
    let recipient = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());

    // Create a position.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    position_dispatcher
        .new_position(
            operator_nonce: 0,
            position_id: user.position_id,
            owner_public_key: user.get_public_key(),
            owner_account: Zero::zero(),
            owner_protection_enabled: true,
        );

    // Fund user with premium cost
    let premium_cost = PREMIUM_COST;
    let premium_amount: u128 = premium_cost.into() * cfg.collateral_cfg.quantum.into();
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user.address, spender: contract_address, amount: premium_amount);

    // Check user balance before forced withdraw request
    validate_balance(token_state, user.address, USER_INIT_BALANCE.try_into().unwrap());

    // Get sequencer address and check its balance before
    let sequencer_address = get_block_info().sequencer_address;
    let sequencer_balance_before = token_state.balance_of(sequencer_address);

    // Setup parameters:
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::days(1)).into(),
    );
    let expiration = Time::now().add(delta: Time::days(1));

    let withdraw_args = WithdrawArgs {
        position_id: user.position_id,
        salt: user.salt_counter,
        expiration,
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount: WITHDRAW_AMOUNT,
        recipient: recipient.address,
    };
    let withdraw_args_hash = withdraw_args.get_message_hash(public_key: user.get_public_key());
    let forced_withdraw_args = ForcedWithdrawArgs { withdraw_args_hash };
    let forced_msg_hash = forced_withdraw_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(message: forced_msg_hash);

    let mut spy = snforge_std::spy_events();
    // Test:

    cheat_caller_address_once(:contract_address, caller_address: user.address);
    dispatcher
        .forced_withdraw_request(
            :signature,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );

    // Catch the event.
    let spied_event = spy.get_events().emitted_by(contract_address).events[0];
    assert_forced_withdraw_request_event_with_expected(
        :spied_event,
        position_id: withdraw_args.position_id,
        recipient: withdraw_args.recipient,
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount: withdraw_args.amount,
        expiration: withdraw_args.expiration,
        forced_withdraw_request_hash: forced_msg_hash,
        salt: withdraw_args.salt,
    );

    // Check that premium cost was transferred from user
    validate_balance(
        token_state, user.address, (USER_INIT_BALANCE - premium_amount).try_into().unwrap(),
    );

    // Check that premium cost was transferred to sequencer address
    let sequencer_balance_after = token_state.balance_of(sequencer_address);
    assert_eq!(sequencer_balance_after, sequencer_balance_before + premium_amount);
}

#[test]
fn test_successful_forced_withdraw_operator_executes() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);
    let dispatcher = ICoreDispatcher { contract_address };
    let position_dispatcher = IPositionsDispatcher { contract_address };
    let deposit_dispatcher = IDepositDispatcher { contract_address };

    let user: User = Default::default();
    let recipient = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());

    // Create a position.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    position_dispatcher
        .new_position(
            operator_nonce: 0,
            position_id: user.position_id,
            owner_public_key: user.get_public_key(),
            owner_account: Zero::zero(),
            owner_protection_enabled: true,
        );

    // Deposit collateral for user
    let deposit_amount = 1000_u64;
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state
        .approve(
            owner: user.address,
            spender: contract_address,
            amount: deposit_amount.into() * cfg.collateral_cfg.quantum.into(),
        );

    cheat_caller_address_once(:contract_address, caller_address: user.address);
    deposit_dispatcher
        .deposit_asset(
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: deposit_amount,
            salt: user.salt_counter,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    deposit_dispatcher
        .process_deposit(
            operator_nonce: 1,
            depositor: user.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: deposit_amount,
            salt: user.salt_counter,
        );

    // Check user balance before forced withdraw request
    validate_balance(
        token_state,
        user.address,
        (USER_INIT_BALANCE - (deposit_amount * cfg.collateral_cfg.quantum).into())
            .try_into()
            .unwrap(),
    );

    // Fund user with premium cost
    let premium_cost = PREMIUM_COST;
    let premium_amount = premium_cost.into() * cfg.collateral_cfg.quantum.into();
    token_state.approve(owner: user.address, spender: contract_address, amount: premium_amount);

    // Get sequencer address and check its balance before
    let sequencer_address = get_block_info().sequencer_address;
    let sequencer_balance_before = token_state.balance_of(sequencer_address);

    // Setup parameters:
    let request_time = Time::now();
    start_cheat_block_timestamp_global(block_timestamp: request_time.into());
    // Set expiration far enough in the future to remain valid after forced action timeout
    let forced_action_timelock = FORCED_ACTION_TIMELOCK;
    let expiration = request_time
        .add(delta: TimeDelta { seconds: forced_action_timelock })
        .add(delta: Time::days(1));

    let withdraw_args = WithdrawArgs {
        position_id: user.position_id,
        salt: user.salt_counter + 1,
        expiration,
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount: WITHDRAW_AMOUNT,
        recipient: recipient.address,
    };
    let withdraw_args_hash = withdraw_args.get_message_hash(public_key: user.get_public_key());
    let forced_withdraw_args = ForcedWithdrawArgs { withdraw_args_hash };
    let forced_msg_hash = forced_withdraw_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(message: forced_msg_hash);

    // Request forced withdraw
    cheat_caller_address_once(:contract_address, caller_address: user.address);
    dispatcher
        .forced_withdraw_request(
            :signature,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );

    // Check that premium cost was transferred from user
    validate_balance(
        token_state,
        user.address,
        (USER_INIT_BALANCE - (deposit_amount * cfg.collateral_cfg.quantum).into() - premium_amount)
            .try_into()
            .unwrap(),
    );

    // Check that premium cost was transferred to sequencer address
    let sequencer_balance_after_request = token_state.balance_of(sequencer_address);
    assert_eq!(sequencer_balance_after_request, sequencer_balance_before + premium_amount);

    // Wait for forced action timeout
    let execute_time = request_time.add(delta: TimeDelta { seconds: 2 });
    start_cheat_block_timestamp_global(block_timestamp: execute_time.into());

    // Check position balance before forced withdraw
    let position_data_before = position_dispatcher.get_position_assets(user.position_id);
    let collateral_balance_before = position_data_before.collateral_balance;

    // Test: Operator process the withdrawal.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    dispatcher
        .withdraw(
            operator_nonce: 2,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );

    // Check after forced withdraw:
    let position_data_after = position_dispatcher.get_position_assets(user.position_id);
    let collateral_balance_after = position_data_after.collateral_balance;
    assert_eq!(collateral_balance_after, collateral_balance_before - withdraw_args.amount.into());
    validate_balance(
        token_state, recipient.address, (WITHDRAW_AMOUNT * COLLATERAL_QUANTUM).try_into().unwrap(),
    );
}

#[test]
fn test_successful_forced_withdraw_user_executes() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);
    let dispatcher = ICoreDispatcher { contract_address };
    let position_dispatcher = IPositionsDispatcher { contract_address };
    let deposit_dispatcher = IDepositDispatcher { contract_address };

    let user: User = Default::default();
    let recipient = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());

    // Create a position.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    position_dispatcher
        .new_position(
            operator_nonce: 0,
            position_id: user.position_id,
            owner_public_key: user.get_public_key(),
            owner_account: Zero::zero(),
            owner_protection_enabled: true,
        );

    // Deposit collateral for user
    let deposit_amount = 1000_u64;
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state
        .approve(
            owner: user.address,
            spender: contract_address,
            amount: deposit_amount.into() * cfg.collateral_cfg.quantum.into(),
        );

    cheat_caller_address_once(:contract_address, caller_address: user.address);
    deposit_dispatcher
        .deposit_asset(
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: deposit_amount,
            salt: user.salt_counter,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    deposit_dispatcher
        .process_deposit(
            operator_nonce: 1,
            depositor: user.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: deposit_amount,
            salt: user.salt_counter,
        );

    // Check user balance before forced withdraw request
    validate_balance(
        token_state,
        user.address,
        (USER_INIT_BALANCE - (deposit_amount * cfg.collateral_cfg.quantum).into())
            .try_into()
            .unwrap(),
    );

    // Fund user with premium cost
    let premium_cost = PREMIUM_COST;
    let premium_amount = premium_cost.into() * cfg.collateral_cfg.quantum.into();
    token_state.approve(owner: user.address, spender: contract_address, amount: premium_amount);

    // Get sequencer address and check its balance before
    let sequencer_address = get_block_info().sequencer_address;
    let sequencer_balance_before = token_state.balance_of(sequencer_address);

    // Setup parameters:
    let request_time = Time::now();
    start_cheat_block_timestamp_global(block_timestamp: request_time.into());
    // Set expiration far enough in the future to remain valid after forced action timeout
    let forced_action_timelock = FORCED_ACTION_TIMELOCK;
    let expiration = request_time
        .add(delta: TimeDelta { seconds: forced_action_timelock })
        .add(delta: Time::days(1));

    let withdraw_args = WithdrawArgs {
        position_id: user.position_id,
        salt: user.salt_counter + 1,
        expiration,
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount: WITHDRAW_AMOUNT,
        recipient: recipient.address,
    };
    let withdraw_args_hash = withdraw_args.get_message_hash(public_key: user.get_public_key());
    let forced_withdraw_args = ForcedWithdrawArgs { withdraw_args_hash };
    let forced_msg_hash = forced_withdraw_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(message: forced_msg_hash);

    // Request forced withdraw
    cheat_caller_address_once(:contract_address, caller_address: user.address);
    dispatcher
        .forced_withdraw_request(
            :signature,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );

    // Check that premium cost was transferred from user
    validate_balance(
        token_state,
        user.address,
        (USER_INIT_BALANCE - (deposit_amount * cfg.collateral_cfg.quantum).into() - premium_amount)
            .try_into()
            .unwrap(),
    );

    // Check that premium cost was transferred to sequencer address
    let sequencer_balance_after_request = token_state.balance_of(sequencer_address);
    assert_eq!(sequencer_balance_after_request, sequencer_balance_before + premium_amount);

    // Wait for forced action timeout
    let forced_action_timelock = FORCED_ACTION_TIMELOCK;
    let execute_time = request_time.add(delta: TimeDelta { seconds: forced_action_timelock });
    start_cheat_block_timestamp_global(block_timestamp: execute_time.into());

    // Check position balance before forced withdraw
    let position_data_before = position_dispatcher.get_position_assets(user.position_id);
    let collateral_balance_before = position_data_before.collateral_balance;

    let mut spy = snforge_std::spy_events();
    // Test: User executes forced withdraw after timeout
    cheat_caller_address_once(:contract_address, caller_address: user.address);
    dispatcher
        .forced_withdraw(
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );

    // Catch the event.
    let spied_event = spy.get_events().emitted_by(contract_address).events[0];
    assert_forced_withdraw_event_with_expected(
        :spied_event,
        position_id: withdraw_args.position_id,
        recipient: withdraw_args.recipient,
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount: withdraw_args.amount,
        expiration: withdraw_args.expiration,
        forced_withdraw_request_hash: forced_msg_hash,
        salt: withdraw_args.salt,
    );

    // Check after forced withdraw:
    let position_data_after = position_dispatcher.get_position_assets(user.position_id);
    let collateral_balance_after = position_data_after.collateral_balance;
    assert_eq!(collateral_balance_after, collateral_balance_before - withdraw_args.amount.into());
    validate_balance(
        token_state, recipient.address, (WITHDRAW_AMOUNT * COLLATERAL_QUANTUM).try_into().unwrap(),
    );
}

#[test]
#[should_panic(expected: 'FORCED_WAIT_REQUIRED')]
fn test_forced_withdraw_before_timeout() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);
    let dispatcher = ICoreDispatcher { contract_address };
    let position_dispatcher = IPositionsDispatcher { contract_address };

    let user: User = Default::default();
    let recipient = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());

    // Create a position.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    position_dispatcher
        .new_position(
            operator_nonce: 0,
            position_id: user.position_id,
            owner_public_key: user.get_public_key(),
            owner_account: Zero::zero(),
            owner_protection_enabled: true,
        );

    // Fund user with premium cost
    let premium_cost = PREMIUM_COST;
    let premium_amount = premium_cost.into() * cfg.collateral_cfg.quantum.into();
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user.address, spender: contract_address, amount: premium_amount);

    // Check user balance before forced withdraw request
    validate_balance(token_state, user.address, USER_INIT_BALANCE.try_into().unwrap());

    // Get sequencer address and check its balance before
    let sequencer_address = get_block_info().sequencer_address;
    let sequencer_balance_before = token_state.balance_of(sequencer_address);

    // Setup parameters:
    let request_time = Time::now();
    start_cheat_block_timestamp_global(block_timestamp: request_time.into());
    // Set expiration far enough in the future to remain valid after forced action timeout
    let forced_action_timelock = FORCED_ACTION_TIMELOCK;
    let expiration = request_time
        .add(delta: TimeDelta { seconds: forced_action_timelock })
        .add(delta: Time::days(1));

    let withdraw_args = WithdrawArgs {
        position_id: user.position_id,
        salt: user.salt_counter,
        expiration,
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount: WITHDRAW_AMOUNT,
        recipient: recipient.address,
    };
    let withdraw_args_hash = withdraw_args.get_message_hash(public_key: user.get_public_key());
    let forced_withdraw_args = ForcedWithdrawArgs { withdraw_args_hash };
    let forced_msg_hash = forced_withdraw_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(message: forced_msg_hash);

    // Request forced withdraw
    cheat_caller_address_once(:contract_address, caller_address: user.address);
    dispatcher
        .forced_withdraw_request(
            :signature,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );

    // Check that premium cost was transferred from user
    validate_balance(
        token_state, user.address, (USER_INIT_BALANCE - premium_amount).try_into().unwrap(),
    );

    // Check that premium cost was transferred to sequencer address
    let sequencer_balance_after = token_state.balance_of(sequencer_address);
    assert_eq!(sequencer_balance_after, sequencer_balance_before + premium_amount);

    // Try to execute before timeout (just before timeout)
    let forced_action_timelock = FORCED_ACTION_TIMELOCK;
    let execute_time = request_time.add(delta: TimeDelta { seconds: forced_action_timelock - 1 });
    start_cheat_block_timestamp_global(block_timestamp: execute_time.into());

    // Test:
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    dispatcher
        .forced_withdraw(
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );
}

#[test]
#[should_panic(expected: 'REQUEST_ALREADY_PROCESSED')]
fn test_forced_withdraw_after_operator_processed_withdraw() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);
    let dispatcher = ICoreDispatcher { contract_address };
    let position_dispatcher = IPositionsDispatcher { contract_address };
    let deposit_dispatcher = IDepositDispatcher { contract_address };

    let user: User = Default::default();
    let recipient = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());

    // Create a position.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    position_dispatcher
        .new_position(
            operator_nonce: 0,
            position_id: user.position_id,
            owner_public_key: user.get_public_key(),
            owner_account: Zero::zero(),
            owner_protection_enabled: true,
        );

    // Deposit collateral for user
    let deposit_amount = 1000_u64;
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state
        .approve(
            owner: user.address,
            spender: contract_address,
            amount: deposit_amount.into() * cfg.collateral_cfg.quantum.into(),
        );

    cheat_caller_address_once(:contract_address, caller_address: user.address);
    deposit_dispatcher
        .deposit_asset(
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: deposit_amount,
            salt: user.salt_counter,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    deposit_dispatcher
        .process_deposit(
            operator_nonce: 1,
            depositor: user.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: deposit_amount,
            salt: user.salt_counter,
        );

    // Fund user with premium cost
    let premium_cost = PREMIUM_COST;
    let premium_amount = premium_cost.into() * cfg.collateral_cfg.quantum.into();
    token_state.approve(owner: user.address, spender: contract_address, amount: premium_amount);

    // Setup parameters:
    let request_time = Time::now();
    start_cheat_block_timestamp_global(block_timestamp: request_time.into());
    let expiration = request_time
        .add(delta: TimeDelta { seconds: FORCED_ACTION_TIMELOCK })
        .add(delta: Time::days(1));

    let withdraw_args = WithdrawArgs {
        position_id: user.position_id,
        salt: user.salt_counter + 1,
        expiration,
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount: WITHDRAW_AMOUNT,
        recipient: recipient.address,
    };
    let withdraw_args_hash = withdraw_args.get_message_hash(public_key: user.get_public_key());
    let forced_withdraw_args = ForcedWithdrawArgs { withdraw_args_hash };
    let forced_msg_hash = forced_withdraw_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(message: forced_msg_hash);

    // Request forced withdraw
    cheat_caller_address_once(:contract_address, caller_address: user.address);
    dispatcher
        .forced_withdraw_request(
            :signature,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );

    // Operator successfully processes the withdraw.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    dispatcher
        .withdraw(
            operator_nonce: 2,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );

    // User tries to force withdraw after operator already processed the withdraw request.
    let user_execute_time = request_time.add(delta: TimeDelta { seconds: FORCED_ACTION_TIMELOCK });
    start_cheat_block_timestamp_global(block_timestamp: user_execute_time.into());
    cheat_caller_address_once(:contract_address, caller_address: user.address);
    dispatcher
        .forced_withdraw(
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );
}

#[test]
#[should_panic(expected: 'REQUEST_ALREADY_PROCESSED')]
fn test_withdraw_after_user_forced_withdraw_executed() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let contract_address = init_by_dispatcher(cfg: @cfg, token_state: @token_state);
    let dispatcher = ICoreDispatcher { contract_address };
    let position_dispatcher = IPositionsDispatcher { contract_address };
    let deposit_dispatcher = IDepositDispatcher { contract_address };

    let user: User = Default::default();
    let recipient = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());

    // Create a position.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    position_dispatcher
        .new_position(
            operator_nonce: 0,
            position_id: user.position_id,
            owner_public_key: user.get_public_key(),
            owner_account: Zero::zero(),
            owner_protection_enabled: true,
        );

    // Deposit collateral for user
    let deposit_amount = 1000_u64;
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state
        .approve(
            owner: user.address,
            spender: contract_address,
            amount: deposit_amount.into() * cfg.collateral_cfg.quantum.into(),
        );

    cheat_caller_address_once(:contract_address, caller_address: user.address);
    deposit_dispatcher
        .deposit_asset(
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: deposit_amount,
            salt: user.salt_counter,
        );

    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    deposit_dispatcher
        .process_deposit(
            operator_nonce: 1,
            depositor: user.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: deposit_amount,
            salt: user.salt_counter,
        );

    // Fund user with premium cost
    let premium_cost = PREMIUM_COST;
    let premium_amount = premium_cost.into() * cfg.collateral_cfg.quantum.into();
    token_state.approve(owner: user.address, spender: contract_address, amount: premium_amount);

    // Setup parameters:
    let request_time = Time::now();
    start_cheat_block_timestamp_global(block_timestamp: request_time.into());
    let forced_action_timelock = FORCED_ACTION_TIMELOCK;
    let expiration = request_time
        .add(delta: TimeDelta { seconds: forced_action_timelock })
        .add(delta: Time::days(1));

    let withdraw_args = WithdrawArgs {
        position_id: user.position_id,
        salt: user.salt_counter + 1,
        expiration,
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount: WITHDRAW_AMOUNT,
        recipient: recipient.address,
    };
    let withdraw_args_hash = withdraw_args.get_message_hash(public_key: user.get_public_key());
    let forced_withdraw_args = ForcedWithdrawArgs { withdraw_args_hash };
    let forced_msg_hash = forced_withdraw_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(message: forced_msg_hash);

    // Request forced withdraw
    cheat_caller_address_once(:contract_address, caller_address: user.address);
    dispatcher
        .forced_withdraw_request(
            :signature,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );

    // Wait for forced action timeout and let user execute forced withdraw successfully.
    let execute_time = request_time.add(delta: TimeDelta { seconds: forced_action_timelock });
    start_cheat_block_timestamp_global(block_timestamp: execute_time.into());
    cheat_caller_address_once(:contract_address, caller_address: user.address);
    dispatcher
        .forced_withdraw(
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );

    // Sending funding tick, as funding was expired.
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    IAssetsDispatcher { contract_address }
        .funding_tick(operator_nonce: 2, funding_ticks: array![].span(), timestamp: Time::now());

    // Operator tries to process withdraw after user already executed forced withdraw.
    let operator_execute_time = execute_time.add(delta: TimeDelta { seconds: 1 });
    start_cheat_block_timestamp_global(block_timestamp: operator_execute_time.into());
    cheat_caller_address_once(:contract_address, caller_address: cfg.operator);
    dispatcher
        .withdraw(
            operator_nonce: 3,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );
}

#[test]
#[should_panic(expected: 'INVALID_ZERO_AMOUNT')]
fn test_forced_withdraw_request_zero_amount() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    let recipient = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));

    let withdraw_args = WithdrawArgs {
        position_id: user.position_id,
        salt: user.salt_counter,
        expiration,
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount: 0,
        recipient: recipient.address,
    };
    let withdraw_args_hash = withdraw_args.get_message_hash(public_key: user.get_public_key());
    let forced_withdraw_args = ForcedWithdrawArgs { withdraw_args_hash };
    let forced_msg_hash = forced_withdraw_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(message: forced_msg_hash);

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .forced_withdraw_request(
            :signature,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );
}

// Deleverage tests.

#[test]
fn test_successful_deleverage() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let deleveraged = Default::default();
    init_position(cfg: @cfg, ref :state, user: deleveraged);
    add_synthetic_to_position(
        ref :state,
        synthetic_id: cfg.synthetic_cfg.synthetic_id,
        position_id: deleveraged.position_id,
        // To make the position deleveragable, the total value must be negative, which requires a
        // negative synthetic balance.
        balance: -2 * SYNTHETIC_BALANCE_AMOUNT,
    );

    let deleverager = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: deleverager);
    add_synthetic_to_position(
        ref :state,
        synthetic_id: cfg.synthetic_cfg.synthetic_id,
        position_id: deleverager.position_id,
        balance: SYNTHETIC_BALANCE_AMOUNT,
    );

    // Test params:
    let operator_nonce = state.get_operator_nonce();
    // For a fair deleverage, the TV/TR ratio of the deleveraged position should remain the same
    // before and after the deleverage. This is the reasoning behind the choice
    // of QUOTE and BASE.
    let BASE = 10;
    let QUOTE = -500;

    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    // State change:
    //                            TV                            TR                          TV/TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // Deleveraged before:     2000-40*100=-2000              40*100*0.5=2000                 -1
    // Deleveraged after:    (2000-500)+(-40+10)*100=-1500           1500                     -1
    // Deleverager before:     2000+20*100=4000               20*100*0.5=1000                 4
    // Deleverager after:    (2000+500)+(20-10)*100=3500            1500                      7/3

    let mut spy = snforge_std::spy_events();
    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .deleverage(
            :operator_nonce,
            deleveraged_position_id: deleveraged.position_id,
            deleverager_position_id: deleverager.position_id,
            base_asset_id: synthetic_id,
            deleveraged_base_amount: BASE,
            deleveraged_quote_amount: QUOTE,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_deleverage_event_with_expected(
        spied_event: events[0],
        deleveraged_position_id: deleveraged.position_id,
        deleverager_position_id: deleverager.position_id,
        base_asset_id: synthetic_id,
        collateral_id: cfg.collateral_cfg.collateral_id,
        deleveraged_base_amount: BASE,
        deleveraged_quote_amount: QUOTE,
    );

    // Check:
    let deleveraged_position = state
        .positions
        .get_position_snapshot(position_id: deleveraged.position_id);
    let deleverager_position = state
        .positions
        .get_position_snapshot(position_id: deleverager.position_id);

    let deleveraged_collateral_balance = state
        .positions
        .get_collateral_provisional_balance(
            position: deleveraged_position, provisional_delta: Option::None,
        );
    let deleveraged_synthetic_balance = state
        .positions
        .get_synthetic_balance(position: deleveraged_position, :synthetic_id);
    assert!(deleveraged_collateral_balance == (COLLATERAL_BALANCE_AMOUNT + QUOTE).into());
    assert!(deleveraged_synthetic_balance == (-2 * SYNTHETIC_BALANCE_AMOUNT + BASE).into());

    let deleverager_collateral_balance = state
        .positions
        .get_collateral_provisional_balance(
            position: deleverager_position, provisional_delta: Option::None,
        );
    let deleverager_synthetic_balance = state
        .positions
        .get_synthetic_balance(position: deleverager_position, :synthetic_id);
    assert!(deleverager_collateral_balance == (COLLATERAL_BALANCE_AMOUNT - QUOTE).into());
    assert!(deleverager_synthetic_balance == (SYNTHETIC_BALANCE_AMOUNT - BASE).into());
}

#[test]
fn test_successful_liquidate() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let liquidator = Default::default();
    init_position(cfg: @cfg, ref :state, user: liquidator);
    let liquidated = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: liquidated);
    add_synthetic_to_position(
        ref :state,
        synthetic_id: cfg.synthetic_cfg.synthetic_id,
        position_id: liquidated.position_id,
        balance: -SYNTHETIC_BALANCE_AMOUNT,
    );

    // Test params:
    let BASE = 10;
    let QUOTE = -5;
    let INSURANCE_FEE = 1;
    let FEE = 2;

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));
    let operator_nonce = state.get_operator_nonce();

    let collateral_id = cfg.collateral_cfg.collateral_id;
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    let order_liquidator = Order {
        position_id: liquidator.position_id,
        salt: liquidator.salt_counter,
        base_asset_id: synthetic_id,
        base_amount: -BASE,
        quote_asset_id: collateral_id,
        quote_amount: -QUOTE,
        fee_asset_id: collateral_id,
        fee_amount: FEE,
        expiration,
    };

    let liquidator_hash = order_liquidator.get_message_hash(liquidator.get_public_key());
    let liquidator_signature = liquidator.sign_message(liquidator_hash);

    let mut spy = snforge_std::spy_events();
    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .liquidate(
            :operator_nonce,
            :liquidator_signature,
            liquidated_position_id: liquidated.position_id,
            liquidator_order: order_liquidator,
            actual_amount_base_liquidated: BASE,
            actual_amount_quote_liquidated: QUOTE,
            actual_liquidator_fee: FEE,
            liquidated_fee_amount: INSURANCE_FEE,
        );
    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_liquidate_event_with_expected(
        spied_event: events[0],
        liquidated_position_id: liquidated.position_id,
        liquidator_order_position_id: liquidator.position_id,
        liquidator_order_base_asset_id: synthetic_id,
        liquidator_order_base_amount: -BASE,
        :collateral_id,
        liquidator_order_quote_amount: -QUOTE,
        liquidator_order_fee_amount: FEE,
        actual_amount_base_liquidated: BASE,
        actual_amount_quote_liquidated: QUOTE,
        actual_liquidator_fee: FEE,
        insurance_fund_fee_amount: INSURANCE_FEE,
        liquidator_order_hash: liquidator_hash,
    );

    // Check:
    let liquidated_position = state
        .positions
        .get_position_snapshot(position_id: liquidated.position_id);
    let liquidator_position = state
        .positions
        .get_position_snapshot(position_id: liquidator.position_id);

    let liquidated_collateral_balance = state
        .positions
        .get_collateral_provisional_balance(
            position: liquidated_position, provisional_delta: Option::None,
        );
    let liquidated_synthetic_balance = state
        .positions
        .get_synthetic_balance(position: liquidated_position, :synthetic_id);
    assert!(
        liquidated_collateral_balance == (COLLATERAL_BALANCE_AMOUNT.into()
            - INSURANCE_FEE.into()
            + QUOTE.into()),
    );
    assert!(liquidated_synthetic_balance == (-SYNTHETIC_BALANCE_AMOUNT + BASE).into());

    let liquidator_collateral_balance = state
        .positions
        .get_collateral_provisional_balance(
            position: liquidator_position, provisional_delta: Option::None,
        );
    let liquidator_synthetic_balance = state
        .positions
        .get_synthetic_balance(position: liquidator_position, :synthetic_id);
    assert!(
        liquidator_collateral_balance == (COLLATERAL_BALANCE_AMOUNT.into()
            - FEE.into()
            - QUOTE.into()),
    );
    assert!(liquidator_synthetic_balance == (-BASE).into());

    let fee_position = state.positions.get_position_snapshot(position_id: FEE_POSITION);
    let fee_position_balance = state
        .positions
        .get_collateral_provisional_balance(
            position: fee_position, provisional_delta: Option::None,
        );
    assert!(fee_position_balance == FEE.into());

    let insurance_fund_position = state
        .positions
        .get_position_snapshot(position_id: INSURANCE_FUND_POSITION);
    let insurance_position_balance = state
        .positions
        .get_collateral_provisional_balance(
            position: insurance_fund_position, provisional_delta: Option::None,
        );
    assert!(insurance_position_balance == INSURANCE_FEE.into());
}

// Test set public key.

#[test]
fn test_successful_set_public_key_request() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let mut user = Default::default();
    init_position_with_owner(cfg: @cfg, ref :state, :user);

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));

    let old_public_key = user.get_public_key();
    let new_key_pair = KEY_PAIR_2();
    user.set_public_key(new_key_pair);
    assert!(user.get_public_key() == new_key_pair.public_key);

    // Test change public key in perps:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    let set_public_key_args = SetPublicKeyArgs {
        position_id: user.position_id,
        old_public_key,
        new_public_key: user.get_public_key(),
        expiration,
    };
    let msg_hash = set_public_key_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(message: msg_hash);
    state
        .set_public_key_request(
            :signature,
            position_id: set_public_key_args.position_id,
            new_public_key: set_public_key_args.new_public_key,
            expiration: set_public_key_args.expiration,
        );

    // Check:
    let status = state.request_approvals.get_request_status(request_hash: msg_hash);
    assert!(status == RequestStatus::PENDING);
}

#[test]
fn test_successful_set_public_key() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let mut user = Default::default();
    init_position_with_owner(cfg: @cfg, ref :state, :user);

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));

    let old_public_key = user.get_public_key();
    let new_key_pair = KEY_PAIR_2();
    user.set_public_key(new_key_pair);
    assert!(user.get_public_key() == new_key_pair.public_key);

    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    let set_public_key_args = SetPublicKeyArgs {
        position_id: user.position_id,
        old_public_key,
        new_public_key: user.get_public_key(),
        expiration,
    };
    let msg_hash = set_public_key_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(message: msg_hash);
    let mut spy = snforge_std::spy_events();
    state
        .set_public_key_request(
            :signature,
            position_id: set_public_key_args.position_id,
            new_public_key: set_public_key_args.new_public_key,
            expiration: set_public_key_args.expiration,
        );

    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .set_public_key(
            operator_nonce: state.get_operator_nonce(),
            position_id: set_public_key_args.position_id,
            new_public_key: set_public_key_args.new_public_key,
            expiration: set_public_key_args.expiration,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_set_public_key_request_event_with_expected(
        spied_event: events[0],
        position_id: set_public_key_args.position_id,
        old_public_key: set_public_key_args.old_public_key,
        new_public_key: set_public_key_args.new_public_key,
        expiration: set_public_key_args.expiration,
        set_public_key_request_hash: msg_hash,
    );
    assert_set_public_key_event_with_expected(
        spied_event: events[1],
        position_id: set_public_key_args.position_id,
        old_public_key: set_public_key_args.old_public_key,
        new_public_key: set_public_key_args.new_public_key,
        set_public_key_request_hash: msg_hash,
    );

    // Check:
    assert!(
        user
            .get_public_key() == state
            .positions
            .get_position_snapshot(position_id: user.position_id)
            .owner_public_key
            .read(),
    );
}

#[test]
#[should_panic(expected: 'REQUEST_NOT_REGISTERED')]
fn test_set_public_key_no_request() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let mut user = Default::default();
    init_position_with_owner(cfg: @cfg, ref :state, :user);

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));

    let old_public_key = user.get_public_key();
    let new_key_pair = KEY_PAIR_2();
    user.set_public_key(new_key_pair);

    let set_public_key_args = SetPublicKeyArgs {
        position_id: user.position_id,
        old_public_key,
        new_public_key: user.get_public_key(),
        expiration,
    };
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .set_public_key(
            operator_nonce: state.get_operator_nonce(),
            position_id: set_public_key_args.position_id,
            new_public_key: set_public_key_args.new_public_key,
            expiration: set_public_key_args.expiration,
        );
}

#[test]
#[should_panic(expected: 'CALLER_IS_NOT_OWNER_ACCOUNT')]
fn test_invalid_set_public_key_request_wrong_owner() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let no_position_owner = Default::default();
    init_position_with_owner(cfg: @cfg, ref :state, user: no_position_owner);
    let position_owner = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position_with_owner(cfg: @cfg, ref :state, user: position_owner);

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));

    // Test change public key in perps:
    let set_public_key_args = SetPublicKeyArgs {
        position_id: position_owner.position_id,
        old_public_key: position_owner.get_public_key(),
        new_public_key: no_position_owner.get_public_key(),
        expiration,
    };
    let msg_hash = set_public_key_args
        .get_message_hash(public_key: no_position_owner.get_public_key());
    let signature = no_position_owner.sign_message(message: msg_hash);
    cheat_caller_address_once(
        contract_address: test_address(), caller_address: no_position_owner.address,
    );
    state
        .set_public_key_request(
            :signature,
            position_id: set_public_key_args.position_id,
            new_public_key: set_public_key_args.new_public_key,
            expiration: set_public_key_args.expiration,
        );
}

#[test]
#[should_panic(expected: 'POSITION_DOESNT_EXIST')]
fn test_set_public_key_request_position_not_exist() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user: User = Default::default();

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));
    let set_public_key_args = SetPublicKeyArgs {
        position_id: user.position_id,
        old_public_key: KEY_PAIR_2().public_key,
        new_public_key: user.get_public_key(),
        expiration,
    };

    // Test change public key in perps:
    let msg_hash = set_public_key_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(message: msg_hash);

    state
        .set_public_key_request(
            :signature,
            position_id: set_public_key_args.position_id,
            new_public_key: set_public_key_args.new_public_key,
            expiration: set_public_key_args.expiration,
        );
}

// Transfer tests.

#[test]
fn test_successful_transfer_request_using_public_key() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    let recipient = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: recipient);

    // Setup parameters:
    let expected_time = Time::now().add(delta: Time::days(1));
    start_cheat_block_timestamp_global(block_timestamp: expected_time.into());
    let expiration = expected_time.add(delta: Time::days(1));

    let transfer_args = TransferArgs {
        position_id: user.position_id,
        salt: user.salt_counter,
        recipient: recipient.position_id,
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount: TRANSFER_AMOUNT,
        expiration,
    };
    let msg_hash = transfer_args.get_message_hash(public_key: user.get_public_key());
    let signature = user.sign_message(msg_hash);

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .transfer_request(
            :signature,
            asset_id: cfg.collateral_cfg.collateral_id,
            recipient: transfer_args.recipient,
            position_id: transfer_args.position_id,
            amount: transfer_args.amount,
            expiration: transfer_args.expiration,
            salt: transfer_args.salt,
        );

    // Check:
    let status = state.request_approvals.get_request_status(request_hash: msg_hash);
    assert!(status == RequestStatus::PENDING);
}

#[test]
fn test_successful_transfer() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let sender = Default::default();
    init_position(cfg: @cfg, ref :state, user: sender);

    let recipient = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: recipient);

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));
    let collateral_id = cfg.collateral_cfg.collateral_id;
    let operator_nonce = state.get_operator_nonce();

    let transfer_args = TransferArgs {
        position_id: sender.position_id,
        recipient: recipient.position_id,
        salt: sender.salt_counter,
        expiration: expiration,
        collateral_id,
        amount: COLLATERAL_BALANCE_AMOUNT.abs().into(),
    };

    let mut spy = snforge_std::spy_events();
    let msg_hash = transfer_args.get_message_hash(sender.get_public_key());
    let sender_signature = sender.sign_message(msg_hash);
    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: sender.address);
    state
        .transfer_request(
            signature: sender_signature,
            asset_id: cfg.collateral_cfg.collateral_id,
            recipient: transfer_args.recipient,
            position_id: transfer_args.position_id,
            amount: transfer_args.amount,
            expiration: transfer_args.expiration,
            salt: transfer_args.salt,
        );
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .transfer(
            :operator_nonce,
            asset_id: cfg.collateral_cfg.collateral_id,
            recipient: transfer_args.recipient,
            position_id: transfer_args.position_id,
            amount: transfer_args.amount,
            expiration: transfer_args.expiration,
            salt: transfer_args.salt,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_transfer_request_event_with_expected(
        spied_event: events[0],
        position_id: transfer_args.position_id,
        recipient: transfer_args.recipient,
        :collateral_id,
        amount: transfer_args.amount,
        expiration: transfer_args.expiration,
        transfer_request_hash: msg_hash,
        salt: transfer_args.salt,
    );
    assert_transfer_event_with_expected(
        spied_event: events[1],
        position_id: transfer_args.position_id,
        recipient: transfer_args.recipient,
        :collateral_id,
        amount: transfer_args.amount,
        expiration: transfer_args.expiration,
        transfer_request_hash: msg_hash,
        salt: transfer_args.salt,
    );

    // Check:
    let sender_position = state.positions.get_position_snapshot(position_id: sender.position_id);
    let sender_collateral_balance = state
        .positions
        .get_collateral_provisional_balance(
            position: sender_position, provisional_delta: Option::None,
        );
    assert_eq!(sender_collateral_balance, 0_i64.into());

    let recipient_position = state
        .positions
        .get_position_snapshot(position_id: recipient.position_id);
    let recipient_collateral_balance = state
        .positions
        .get_collateral_provisional_balance(
            position: recipient_position, provisional_delta: Option::None,
        );
    assert_eq!(recipient_collateral_balance, (2 * COLLATERAL_BALANCE_AMOUNT).into());
}

#[test]
#[should_panic(expected: 'INVALID_ZERO_AMOUNT')]
fn test_invalid_transfer_request_amount_is_zero() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let sender = Default::default();
    init_position(cfg: @cfg, ref :state, user: sender);

    let recipient = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: recipient);

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));
    let collateral_id = cfg.collateral_cfg.collateral_id;

    let transfer_args = TransferArgs {
        position_id: sender.position_id,
        recipient: recipient.position_id,
        salt: sender.salt_counter,
        expiration: expiration,
        collateral_id,
        amount: 0,
    };

    let sender_signature = sender
        .sign_message(transfer_args.get_message_hash(sender.get_public_key()));

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: sender.address);
    state
        .transfer_request(
            signature: sender_signature,
            asset_id: cfg.collateral_cfg.collateral_id,
            recipient: transfer_args.recipient,
            position_id: transfer_args.position_id,
            amount: transfer_args.amount,
            expiration: transfer_args.expiration,
            salt: transfer_args.salt,
        );
}

// `validate_synthetic_price` tests.

#[test]
#[should_panic(expected: 'SYNTHETIC_EXPIRED_PRICE')]
fn test_validate_asset_prices_expired() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user: User = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    // Set the block timestamp to be after the price validation interval
    let now = Time::now().add(delta: Time::days(count: 2));
    start_cheat_block_timestamp_global(block_timestamp: now.into());
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .assets
        .funding_tick(
            operator_nonce: state.get_operator_nonce(),
            funding_ticks: array![
                FundingTick {
                    asset_id: cfg.synthetic_cfg.synthetic_id, funding_index: Zero::zero(),
                },
            ]
                .span(),
            timestamp: Time::now(),
        );
    // Setup parameters:
    let expiration = Time::now().add(Time::days(1));

    let withdraw_args = WithdrawArgs {
        position_id: user.position_id,
        salt: user.salt_counter,
        expiration,
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount: WITHDRAW_AMOUNT,
        recipient: user.address,
    };
    let hash = withdraw_args.get_message_hash(user.get_public_key());
    let signature = user.sign_message(hash);

    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .withdraw_request(
            :signature,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .withdraw(
            operator_nonce: state.get_operator_nonce(),
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );
}

#[test]
fn test_validate_asset_prices_pending_asset() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_pending_asset(cfg: @cfg, token_state: @token_state);
    let user: User = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    // Fund user.
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state
        .approve(
            owner: user.address,
            spender: test_address(),
            amount: DEPOSIT_AMOUNT.into() * cfg.collateral_cfg.quantum.into(),
        );
    // Set the block timestamp to be after the price validation interval
    let now = Time::now().add(delta: Time::days(count: 2));
    start_cheat_block_timestamp_global(block_timestamp: now.into());
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .assets
        .funding_tick(
            operator_nonce: state.get_operator_nonce(),
            funding_ticks: array![].span(),
            timestamp: Time::now(),
        );
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit(
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .process_deposit(
            operator_nonce: state.get_operator_nonce(),
            depositor: user.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
    // If no assertion error is thrown, the test passes
}

#[test]
fn test_validate_prices() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user: User = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    let old_time = Time::now();
    let mut new_time: u64 = Time::now().add(delta: state.get_max_price_interval()).into();
    let asset_name = 'ASSET_NAME';
    let oracle1_name = 'ORCL1';
    let oracle1 = Oracle { oracle_name: oracle1_name, asset_name, key_pair: KEY_PAIR_2() };
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: oracle1.key_pair.public_key,
            oracle_name: oracle1_name,
            :asset_name,
        );
    start_cheat_block_timestamp_global(block_timestamp: new_time);
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .assets
        .funding_tick(
            operator_nonce: state.get_operator_nonce(),
            funding_ticks: array![
                FundingTick {
                    asset_id: cfg.synthetic_cfg.synthetic_id, funding_index: Zero::zero(),
                },
            ]
                .span(),
            timestamp: Time::now(),
        );

    // Setup parameters:
    let expiration = Time::now().add(Time::days(3));

    let withdraw_args = WithdrawArgs {
        position_id: user.position_id,
        salt: user.salt_counter,
        expiration,
        collateral_id: cfg.collateral_cfg.collateral_id,
        amount: WITHDRAW_AMOUNT,
        recipient: user.address,
    };
    let hash = withdraw_args.get_message_hash(user.get_public_key());
    let signature = user.sign_message(hash);

    assert_eq!(state.assets.get_last_price_validation(), old_time);

    let oracle_old_time: u64 = Time::now().into();
    let oracle_price: u128 = ORACLE_PRICE;
    let operator_nonce = state.get_operator_nonce();
    state
        .price_tick(
            :operator_nonce,
            asset_id: cfg.synthetic_cfg.synthetic_id,
            :oracle_price,
            signed_prices: [
                oracle1
                    .get_signed_price(:oracle_price, timestamp: oracle_old_time.try_into().unwrap())
            ]
                .span(),
        );

    new_time = Time::now().add(delta: state.get_max_price_interval()).into();
    start_cheat_block_timestamp_global(block_timestamp: new_time);

    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .withdraw_request(
            :signature,
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .withdraw(
            operator_nonce: state.get_operator_nonce(),
            collateral_id: cfg.collateral_cfg.collateral_id,
            recipient: withdraw_args.recipient,
            position_id: withdraw_args.position_id,
            amount: withdraw_args.amount,
            expiration: withdraw_args.expiration,
            salt: withdraw_args.salt,
        );

    assert_eq!(state.assets.get_last_price_validation().into(), new_time);
}

#[test]
fn test_validate_prices_no_update_needed() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let user: User = Default::default();
    init_position(cfg: @cfg, ref :state, :user);
    // Fund user.
    token_state.fund(recipient: user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state
        .approve(
            owner: user.address,
            spender: test_address(),
            amount: DEPOSIT_AMOUNT.into() * cfg.collateral_cfg.quantum.into(),
        );
    let old_time = Time::now();
    assert!(state.assets.get_last_price_validation() == old_time);
    let new_time = Time::now().add(delta: Time::seconds(count: 1000));
    start_cheat_block_timestamp_global(block_timestamp: new_time.into());
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit(
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .process_deposit(
            operator_nonce: state.get_operator_nonce(),
            depositor: user.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );

    assert!(state.assets.get_last_price_validation() == old_time);
}

// `funding_tick` tests.

#[test]
fn test_funding_tick_basic() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let new_time = Time::now().add(Time::seconds(HOUR));
    start_cheat_block_timestamp_global(block_timestamp: new_time.into());

    let synthetic_id = cfg.synthetic_cfg.synthetic_id;
    // Funding index is 3.
    let new_funding_index = FundingIndex { value: 3 * FUNDING_SCALE };
    let funding_ticks: Span<FundingTick> = array![
        FundingTick { asset_id: synthetic_id, funding_index: new_funding_index },
    ]
        .span();

    // Test:

    // The funding index must be within the max funding rate:
    // |prev_funding_index-new_funding_index| = |0 - 3| = 3.
    // synthetic_price * max_funding_rate * time_diff = 100 * 3% per hour * 1 hour = 3.
    // 3 <= 3.
    let mut spy = snforge_std::spy_events();
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .funding_tick(
            operator_nonce: state.get_operator_nonce(), :funding_ticks, timestamp: Time::now(),
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_funding_tick_event_with_expected(
        spied_event: events[0], asset_id: synthetic_id, funding_index: new_funding_index,
    );

    // Check:
    assert!(state.assets.get_timely_data(synthetic_id).funding_index == new_funding_index);
}

#[test]
#[should_panic(
    expected: "INVALID_FUNDING_RATE synthetic_id: AssetId { value: 720515315941943725751128480342703114962297896757142150278960020243082094068 }",
)]
#[feature("safe_dispatcher")]
fn test_invalid_funding_rate() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let new_time = Time::now().add(Time::seconds(HOUR));
    start_cheat_block_timestamp_global(block_timestamp: new_time.into());

    let synthetic_id = cfg.synthetic_cfg.synthetic_id;
    // Funding index is 4.
    let new_funding_index = FundingIndex { value: 4 * FUNDING_SCALE };
    let funding_ticks: Span<FundingTick> = array![
        FundingTick { asset_id: synthetic_id, funding_index: new_funding_index },
    ]
        .span();

    // Test:

    // The funding index must be within the max funding rate:
    // |prev_funding_index-new_funding_index| = |0 - 4| = 4.
    // synthetic_price * max_funding_rate * time_diff = 100 * 3% per hour * 1 hour = 3.
    // 3 > 4.
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .funding_tick(
            operator_nonce: state.get_operator_nonce(), :funding_ticks, timestamp: Time::now(),
        );
}

#[test]
#[should_panic(expected: 'ASSET_NOT_EXISTS')]
fn test_funding_tick_collateral_asset() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let new_time = Time::now().add(Time::seconds(HOUR));
    start_cheat_block_timestamp_global(block_timestamp: new_time.into());

    let collateral_id = cfg.collateral_cfg.collateral_id;
    let new_funding_index = FundingIndex { value: FUNDING_SCALE };
    let funding_ticks: Span<FundingTick> = array![
        FundingTick { asset_id: collateral_id, funding_index: new_funding_index },
    ]
        .span();

    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .funding_tick(
            operator_nonce: state.get_operator_nonce(), :funding_ticks, timestamp: Time::now(),
        );
}

#[test]
#[should_panic(expected: 'NOT_SYNTHETIC')]
fn test_funding_tick_vault_share_asset() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    // Register a vault share collateral asset so that it exists in the assets config.
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_vault_collateral_asset(
            asset_id: cfg.vault_share_cfg.collateral_id,
            erc20_contract_address: cfg.vault_share_cfg.contract_address,
            quantum: cfg.vault_share_cfg.quantum,
            risk_factor_tiers: cfg.vault_share_cfg.risk_factor_tiers,
            risk_factor_first_tier_boundary: cfg.vault_share_cfg.risk_factor_first_tier_boundary,
            risk_factor_tier_size: cfg.vault_share_cfg.risk_factor_tier_size,
            quorum: 1_u8,
        );

    let new_time = Time::now().add(Time::seconds(HOUR));
    start_cheat_block_timestamp_global(block_timestamp: new_time.into());

    let vault_share_id = cfg.vault_share_cfg.collateral_id;
    let new_funding_index = FundingIndex { value: FUNDING_SCALE };
    let funding_ticks: Span<FundingTick> = array![
        FundingTick { asset_id: vault_share_id, funding_index: new_funding_index },
    ]
        .span();

    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .funding_tick(
            operator_nonce: state.get_operator_nonce(), :funding_ticks, timestamp: Time::now(),
        );
}

#[test]
#[should_panic(expected: 'INVALID_FUNDING_TICK_LEN')]
fn test_invalid_funding_len() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let new_time = Time::now().add(Time::seconds(10));
    start_cheat_block_timestamp_global(block_timestamp: new_time.into());

    let new_funding_index_1 = FundingIndex { value: 100 * FUNDING_SCALE };
    let new_funding_index_2 = FundingIndex { value: 3 * FUNDING_SCALE };
    let funding_ticks: Span<FundingTick> = array![
        FundingTick { asset_id: SYNTHETIC_ASSET_ID_1(), funding_index: new_funding_index_1 },
        FundingTick { asset_id: SYNTHETIC_ASSET_ID_2(), funding_index: new_funding_index_2 },
    ]
        .span();

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .funding_tick(
            operator_nonce: state.get_operator_nonce(), :funding_ticks, timestamp: Time::now(),
        );
}

// `price_tick` tests.

#[test]
fn test_price_tick_basic() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_pending_asset(cfg: @cfg, token_state: @token_state);
    let mut spy = snforge_std::spy_events();
    let asset_name = 'ASSET_NAME';
    let oracle1_name = 'ORCL1';
    let oracle1 = Oracle { oracle_name: oracle1_name, asset_name, key_pair: KEY_PAIR_1() };
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: oracle1.key_pair.public_key,
            oracle_name: oracle1_name,
            :asset_name,
        );
    let old_time: u64 = Time::now().into();
    let new_time = Time::now().add(delta: MAX_ORACLE_PRICE_VALIDITY);
    assert!(state.assets.get_num_of_active_synthetic_assets() == 0);
    start_cheat_block_timestamp_global(block_timestamp: new_time.into());
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    let oracle_price: u128 = ORACLE_PRICE;
    let operator_nonce = state.get_operator_nonce();
    state
        .price_tick(
            :operator_nonce,
            asset_id: synthetic_id,
            :oracle_price,
            signed_prices: [
                oracle1.get_signed_price(:oracle_price, timestamp: old_time.try_into().unwrap())
            ]
                .span(),
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_add_oracle_event_with_expected(
        spied_event: events[0],
        asset_id: synthetic_id,
        :asset_name,
        oracle_public_key: oracle1.key_pair.public_key,
        oracle_name: oracle1_name,
    );
    assert_asset_activated_event_with_expected(spied_event: events[1], asset_id: synthetic_id);
    assert_price_tick_event_with_expected(
        spied_event: events[2], asset_id: synthetic_id, price: PriceTrait::new(value: 100),
    );

    assert!(state.assets.get_asset_config(synthetic_id).status == AssetStatus::ACTIVE);
    assert!(state.assets.get_num_of_active_synthetic_assets() == 1);

    let data = state.assets.get_timely_data(synthetic_id);
    assert!(data.last_price_update == new_time);
    assert!(data.price.value() == 100 * PRICE_SCALE);
}

#[test]
fn test_price_tick_odd() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_pending_asset(cfg: @cfg, token_state: @token_state);
    let asset_name = 'ASSET_NAME';
    let oracle1_name = 'ORCL1';
    let oracle2_name = 'ORCL2';
    let oracle3_name = 'ORCL3';
    let oracle1 = Oracle { oracle_name: oracle1_name, asset_name, key_pair: KEY_PAIR_1() };
    let oracle2 = Oracle { oracle_name: oracle2_name, asset_name, key_pair: KEY_PAIR_2() };
    let oracle3 = Oracle { oracle_name: oracle3_name, asset_name, key_pair: KEY_PAIR_3() };
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: oracle1.key_pair.public_key,
            oracle_name: oracle1_name,
            :asset_name,
        );
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: oracle2.key_pair.public_key,
            oracle_name: oracle2_name,
            :asset_name,
        );
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: oracle3.key_pair.public_key,
            oracle_name: oracle3_name,
            :asset_name,
        );
    let old_time: u64 = Time::now().into();
    assert!(state.assets.get_num_of_active_synthetic_assets() == 0);
    let new_time = Time::now().add(delta: MAX_ORACLE_PRICE_VALIDITY);
    start_cheat_block_timestamp_global(block_timestamp: new_time.into());
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    let oracle_price: u128 = ORACLE_PRICE;
    let operator_nonce = state.get_operator_nonce();
    state
        .price_tick(
            :operator_nonce,
            asset_id: synthetic_id,
            :oracle_price,
            signed_prices: [
                oracle2.get_signed_price(:oracle_price, timestamp: old_time.try_into().unwrap()),
                oracle3
                    .get_signed_price(
                        oracle_price: oracle_price + 1, timestamp: old_time.try_into().unwrap(),
                    ),
                oracle1
                    .get_signed_price(
                        oracle_price: oracle_price - 1, timestamp: old_time.try_into().unwrap(),
                    ),
            ]
                .span(),
        );
    assert!(state.assets.get_asset_config(synthetic_id).status == AssetStatus::ACTIVE);
    assert!(state.assets.get_num_of_active_synthetic_assets() == 1);
    let data = state.assets.get_timely_data(synthetic_id);
    assert!(data.last_price_update == new_time);
    assert!(data.price.value() == 100 * PRICE_SCALE);
}

#[test]
fn test_price_tick_even() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_pending_asset(cfg: @cfg, token_state: @token_state);
    let asset_name = 'ASSET_NAME';
    let oracle1_name = 'ORCL1';
    let oracle3_name = 'ORCL3';
    let oracle1 = Oracle { oracle_name: oracle1_name, asset_name, key_pair: KEY_PAIR_1() };
    let oracle3 = Oracle { oracle_name: oracle3_name, asset_name, key_pair: KEY_PAIR_3() };
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: oracle1.key_pair.public_key,
            oracle_name: oracle1_name,
            :asset_name,
        );
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: oracle3.key_pair.public_key,
            oracle_name: oracle3_name,
            :asset_name,
        );
    let old_time: u64 = Time::now().into();
    assert!(state.assets.get_num_of_active_synthetic_assets() == 0);
    let new_time = Time::now().add(delta: MAX_ORACLE_PRICE_VALIDITY);
    start_cheat_block_timestamp_global(block_timestamp: new_time.into());
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    let oracle_price: u128 = ORACLE_PRICE;
    let operator_nonce = state.get_operator_nonce();
    state
        .price_tick(
            :operator_nonce,
            asset_id: synthetic_id,
            :oracle_price,
            signed_prices: [
                oracle3
                    .get_signed_price(
                        oracle_price: oracle_price + 1, timestamp: old_time.try_into().unwrap(),
                    ),
                oracle1
                    .get_signed_price(
                        oracle_price: oracle_price - 1, timestamp: old_time.try_into().unwrap(),
                    ),
            ]
                .span(),
        );
    assert!(state.assets.get_asset_config(synthetic_id).status == AssetStatus::ACTIVE);
    assert!(state.assets.get_num_of_active_synthetic_assets() == 1);

    let data = state.assets.get_timely_data(synthetic_id);
    assert!(data.last_price_update == new_time);
    assert!(data.price.value() == 100 * PRICE_SCALE);
}

#[test]
#[should_panic(expected: 'QUORUM_NOT_REACHED')]
fn test_price_tick_no_quorum() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    let operator_nonce = state.get_operator_nonce();
    state
        .price_tick(
            :operator_nonce,
            asset_id: cfg.synthetic_cfg.synthetic_id,
            oracle_price: Zero::zero(),
            signed_prices: [].span(),
        );
}

#[test]
#[should_panic(expected: 'SIGNED_PRICES_UNSORTED')]
fn test_price_tick_unsorted() {
    start_cheat_block_timestamp_global(block_timestamp: Time::now().add(Time::weeks(1)).into());
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_pending_asset(cfg: @cfg, token_state: @token_state);
    let asset_name = 'ASSET_NAME';
    let oracle1_name = 'ORCL1';
    let oracle2_name = 'ORCL2';
    let oracle1 = Oracle { oracle_name: oracle1_name, asset_name, key_pair: KEY_PAIR_1() };
    let oracle2 = Oracle { oracle_name: oracle2_name, asset_name, key_pair: KEY_PAIR_3() };
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: oracle1.key_pair.public_key,
            oracle_name: oracle1_name,
            :asset_name,
        );
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: oracle2.key_pair.public_key,
            oracle_name: oracle2_name,
            :asset_name,
        );
    let old_time: u64 = Time::now().into();
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    let oracle_price: u128 = ORACLE_PRICE;
    let operator_nonce = state.get_operator_nonce();
    state
        .price_tick(
            :operator_nonce,
            asset_id: synthetic_id,
            :oracle_price,
            signed_prices: [
                oracle1
                    .get_signed_price(
                        oracle_price: oracle_price - 1, timestamp: old_time.try_into().unwrap(),
                    ),
                oracle2
                    .get_signed_price(
                        oracle_price: oracle_price + 1, timestamp: old_time.try_into().unwrap(),
                    ),
            ]
                .span(),
        );
}

#[test]
#[should_panic(expected: 'INVALID_PRICE_TIMESTAMP')]
fn test_price_tick_old_oracle() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_pending_asset(cfg: @cfg, token_state: @token_state);
    let asset_name = 'ASSET_NAME';
    let oracle1_name = 'ORCL1';
    let oracle1 = Oracle { oracle_name: oracle1_name, asset_name, key_pair: KEY_PAIR_1() };
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: oracle1.key_pair.public_key,
            oracle_name: oracle1_name,
            :asset_name,
        );
    let old_time: u64 = Time::now().into();
    let new_time = Time::now().add(delta: MAX_ORACLE_PRICE_VALIDITY + Time::seconds(1));
    start_cheat_block_timestamp_global(block_timestamp: new_time.into());
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    let oracle_price = 1000;
    let operator_nonce = state.get_operator_nonce();
    state
        .price_tick(
            :operator_nonce,
            asset_id: synthetic_id,
            :oracle_price,
            signed_prices: [
                oracle1.get_signed_price(:oracle_price, timestamp: old_time.try_into().unwrap())
            ]
                .span(),
        );
}

#[test]
/// This test numbers were taken from an example of a real price tick that was sent to StarkEx.
fn test_price_tick_golden() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_pending_asset(cfg: @cfg, token_state: @token_state);
    let asset_name = 'PENGUUSDMARK\x00\x00\x00\x00';
    let oracle0_name = 'Stkai';
    let oracle1_name = 'Stork';
    let oracle2_name = 'StCrw';
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: 0x1f191d23b8825dcc3dba839b6a7155ea07ad0b42af76394097786aca0d9975c,
            oracle_name: oracle0_name,
            :asset_name,
        );
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: 0xcc85afe4ca87f9628370c432c447e569a01dc96d160015c8039959db8521c4,
            oracle_name: oracle1_name,
            :asset_name,
        );
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: 0x41dbe627aeab66504b837b3abd88ae2f58ba6d98ee7bbd7f226c4684d9e6225,
            oracle_name: oracle2_name,
            :asset_name,
        );

    let timestamp: u32 = 1737451956;
    let oracle_price = 23953641840000000;
    let signed_price0 = SignedPrice {
        signature: [
            0x23120d436ab1e115f883fd495206b80c9a9928f94df89c2bb63eb1997cc13d5,
            0x21469ce0da02bf1a5897077b238f536f78427f946dafde2b79884cf10131e74,
        ]
            .span(),
        signer_public_key: 0x1f191d23b8825dcc3dba839b6a7155ea07ad0b42af76394097786aca0d9975c,
        timestamp,
        oracle_price,
    };
    let signed_price1 = SignedPrice {
        signature: [
            0x6c4beab13946105513c157ca8498735af2c3ff0f75efe6e1d1747efcff8339f,
            0x94619200c9b03a647f6f29df52d2291e866b43e57dc1a8200deb5219c87b14,
        ]
            .span(),
        signer_public_key: 0xcc85afe4ca87f9628370c432c447e569a01dc96d160015c8039959db8521c4,
        timestamp,
        oracle_price,
    };
    let signed_price2 = SignedPrice {
        signature: [
            0x3aed46d0aff9d904faf5f76c2fb9f43c858e6f9e9c9bf99ca9fd4c1baa907b2,
            0x58523be606a55c57aedd5e030a349a478a22132b84d6f77e1e348a4991f5c80,
        ]
            .span(),
        signer_public_key: 0x41dbe627aeab66504b837b3abd88ae2f58ba6d98ee7bbd7f226c4684d9e6225,
        timestamp,
        oracle_price,
    };
    start_cheat_block_timestamp_global(
        block_timestamp: Timestamp { seconds: timestamp.into() + 1 }.into(),
    );
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    let operator_nonce = state.get_operator_nonce();
    state
        .price_tick(
            :operator_nonce,
            asset_id: synthetic_id,
            :oracle_price,
            signed_prices: [signed_price1, signed_price0, signed_price2].span(),
        );
    let data = state.assets.get_timely_data(synthetic_id);
    assert!(data.last_price_update == Time::now());
    assert!(data.price.value() == 6430);
}

// Add and remove oracle tests.

#[test]
fn test_successful_add_and_remove_oracle() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_pending_asset(cfg: @cfg, token_state: @token_state);

    let asset_name = 'ASSET_NAME';
    let oracle_name = 'ORCL';
    let key_pair = KEY_PAIR_1();
    // let oracle1 = Oracle { oracle_name, asset_name, key_pair };
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    // Test:
    let mut spy = snforge_std::spy_events();
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: key_pair.public_key,
            :oracle_name,
            :asset_name,
        );

    state.update_asset_quorum(asset_id: synthetic_id, quorum: 2);

    // Add another oracle for the same asset id.
    let asset_name = 'ASSET_NAME';
    let oracle_name = 'ORCL';
    let key_pair = KEY_PAIR_2();
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: key_pair.public_key,
            :oracle_name,
            :asset_name,
        );

    state.remove_oracle_from_asset(asset_id: synthetic_id, oracle_public_key: key_pair.public_key);

    let events = spy.get_events().emitted_by(test_address()).events;
    assert_update_asset_quorum_event_with_expected(
        spied_event: events[1], asset_id: synthetic_id, new_quorum: 2, old_quorum: 1,
    );
    assert_add_oracle_event_with_expected(
        spied_event: events[2],
        asset_id: synthetic_id,
        :asset_name,
        oracle_public_key: key_pair.public_key,
        :oracle_name,
    );
    assert_remove_oracle_event_with_expected(
        spied_event: events[3], asset_id: synthetic_id, oracle_public_key: key_pair.public_key,
    );
}

#[test]
#[should_panic(expected: 'ORACLE_NAME_TOO_LONG')]
fn test_add_oracle_name_too_long() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_pending_asset(cfg: @cfg, token_state: @token_state);

    let asset_name = 'ASSET_NAME';
    let oracle_name = 'LONG_ORACLE_NAME';
    let key_pair = KEY_PAIR_1();
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: key_pair.public_key,
            :oracle_name,
            :asset_name,
        );
}

#[test]
#[should_panic(expected: 'ASSET_NAME_TOO_LONG')]
fn test_add_oracle_asset_name_too_long() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_pending_asset(cfg: @cfg, token_state: @token_state);

    let asset_name = 'TOO_LONG_ASSET_NAME';
    let oracle_name = 'ORCL';
    let key_pair = KEY_PAIR_1();
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: key_pair.public_key,
            :oracle_name,
            :asset_name,
        );
}

#[test]
#[should_panic(expected: 'ORACLE_ALREADY_EXISTS')]
fn test_add_existed_oracle() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let asset_name = 'ASSET_NAME';
    let oracle_name = 'ORCL';
    let key_pair = KEY_PAIR_1();
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: key_pair.public_key,
            :oracle_name,
            :asset_name,
        );

    // Add the a new oracle with the same names, and different public key.
    let asset_name = 'SAME_ASSET_NAME';
    let oracle_name = 'ORCL2';

    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: synthetic_id,
            oracle_public_key: key_pair.public_key,
            :oracle_name,
            :asset_name,
        );
}

#[test]
#[should_panic(expected: 'ORACLE_NOT_EXISTS')]
fn test_successful_remove_nonexistent_oracle() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_pending_asset(cfg: @cfg, token_state: @token_state);

    // Parameters:
    let key_pair = KEY_PAIR_1();
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state.remove_oracle_from_asset(asset_id: synthetic_id, oracle_public_key: key_pair.public_key);
}

// #[test]
// #[should_panic(expected: 'MISMATCHED_RESOLUTION')]
// fn test_unsuccessful_add_vault_share_asset_mismatched_resolution() {
//     // Setup state, token:
//     let cfg: PerpetualsInitConfig = Default::default();
//     let token_state = cfg.collateral_cfg.token_cfg.deploy();
//     let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
//     let vault_share_state = cfg.vault_share_cfg.token_cfg.deploy();

//     // Setup test parameters:
//     let risk_factor_first_tier_boundary = MAX_U128;
//     let risk_factor_tier_size = 1;
//     let risk_factor_1 = array![10].span();

//     // Test:
//     cheat_caller_address_once(contract_address: test_address(), caller_address:
//     cfg.app_governor);
//     state
//         .add_vault_collateral_asset(
//             asset_id: cfg.vault_share_cfg.collateral_id,
//             erc20_contract_address: vault_share_state.address,
//             quantum: 10_000_000,
//             resolution_factor: 1_000_000_000,
//             risk_factor_tiers: risk_factor_1,
//             :risk_factor_first_tier_boundary,
//             :risk_factor_tier_size,
//             quorum: 1_u8,
//         );
// }

#[test]
#[should_panic(expected: 'INVALID_SHARE_QUANTUM')]
fn test_unsuccessful_add_vault_share_asset_zero_quantum() {
    // Setup state, token:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let vault_share_state = cfg.vault_share_cfg.token_cfg.deploy();

    // Setup test parameters:
    let risk_factor_first_tier_boundary = MAX_U128;
    let risk_factor_tier_size = 1;
    let risk_factor_1 = array![10].span();

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_vault_collateral_asset(
            asset_id: cfg.vault_share_cfg.collateral_id,
            erc20_contract_address: vault_share_state.address,
            quantum: 0,
            risk_factor_tiers: risk_factor_1,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            quorum: 1_u8,
        );
}

#[test]
#[should_panic(expected: 'ENTRYPOINT_NOT_FOUND')]
fn test_unsuccessful_add_vault_share_asset_not_erc20() {
    // Setup state, token:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    // Setup test parameters:
    let risk_factor_first_tier_boundary = MAX_U128;
    let risk_factor_tier_size = 1;
    let risk_factor_1 = array![10].span();

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_vault_collateral_asset(
            asset_id: cfg.vault_share_cfg.collateral_id,
            erc20_contract_address: test_address(),
            quantum: 1,
            risk_factor_tiers: risk_factor_1,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            quorum: 1_u8,
        );
}

#[test]
fn test_successful_add_vault_share_asset() {
    // Setup state, token:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let mut spy = snforge_std::spy_events();

    // VS has 10^18
    // quantum 10^12
    // resolution 10^(18-12) = 10^6

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_vault_collateral_asset(
            asset_id: cfg.vault_share_cfg.collateral_id,
            erc20_contract_address: cfg.vault_share_cfg.contract_address,
            quantum: cfg.vault_share_cfg.quantum,
            risk_factor_tiers: cfg.vault_share_cfg.risk_factor_tiers,
            risk_factor_first_tier_boundary: cfg.vault_share_cfg.risk_factor_first_tier_boundary,
            risk_factor_tier_size: cfg.vault_share_cfg.risk_factor_tier_size,
            quorum: 1_u8,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_add_spot_event_with_expected(
        spied_event: events[0],
        asset_id: cfg.vault_share_cfg.collateral_id,
        risk_factor_tiers: cfg.vault_share_cfg.risk_factor_tiers,
        risk_factor_first_tier_boundary: cfg.vault_share_cfg.risk_factor_first_tier_boundary,
        risk_factor_tier_size: cfg.vault_share_cfg.risk_factor_tier_size,
        resolution_factor: cfg.vault_share_cfg.resolution_factor,
        quorum: 1_u8,
        contract_address: cfg.vault_share_cfg.contract_address,
        quantum: 1,
    );

    let asset_config = state.assets.get_asset_config(cfg.vault_share_cfg.collateral_id);

    assert!(asset_config.resolution_factor == 1000000);
    assert!(
        asset_config
            .risk_factor_first_tier_boundary == cfg
            .vault_share_cfg
            .risk_factor_first_tier_boundary,
    );
    assert!(asset_config.risk_factor_tier_size == cfg.vault_share_cfg.risk_factor_tier_size);
    assert!(asset_config.quorum == 1_u8);
    assert!(asset_config.status == AssetStatus::PENDING);
}

#[test]
fn test_successful_add_spot_asset() {
    // Setup state, token:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);
    let mut spy = snforge_std::spy_events();

    // Setup test parameters:
    let spot_asset_id = SYNTHETIC_ASSET_ID_2();
    let risk_factor_first_tier_boundary = MAX_U128;
    let risk_factor_tier_size = 1;
    let risk_factor_tiers = array![10].span();
    let quorum = 1_u8;
    let resolution_factor = SYNTHETIC_RESOLUTION_FACTOR;
    let quantum = 12_u64;
    let erc20_contract_address = token_state.address;

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_spot_asset(
            asset_id: spot_asset_id,
            :erc20_contract_address,
            :quantum,
            :resolution_factor,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_add_spot_event_with_expected(
        spied_event: events[0],
        asset_id: spot_asset_id,
        risk_factor_tiers: risk_factor_tiers,
        :risk_factor_first_tier_boundary,
        :risk_factor_tier_size,
        :resolution_factor,
        :quorum,
        contract_address: erc20_contract_address,
        :quantum,
    );

    // Check:
    let asset_config = state.assets.get_asset_config(spot_asset_id);
    assert!(asset_config.resolution_factor == resolution_factor);
    assert!(asset_config.risk_factor_first_tier_boundary == risk_factor_first_tier_boundary);
    assert!(asset_config.risk_factor_tier_size == risk_factor_tier_size);
    assert!(asset_config.quorum == quorum);
    assert!(asset_config.quantum == quantum);
    assert!(asset_config.status == AssetStatus::PENDING);
}

#[test]
#[should_panic(expected: 'INVALID_SPOT_QUANTUM')]
fn test_unsuccessful_add_spot_asset_zero_quantum() {
    // Setup state, token:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    // Setup test parameters:
    let spot_asset_id = SYNTHETIC_ASSET_ID_2();
    let risk_factor_first_tier_boundary = MAX_U128;
    let risk_factor_tier_size = 1;
    let risk_factor_tiers = array![10].span();
    let quorum = 1_u8;
    let resolution_factor = SYNTHETIC_RESOLUTION_FACTOR;
    let quantum = 0_u64;
    let erc20_contract_address = token_state.address;

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_spot_asset(
            asset_id: spot_asset_id,
            :erc20_contract_address,
            :quantum,
            :resolution_factor,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
        );
}

#[test]
#[should_panic(expected: 'ASSET_ALREADY_EXISTS')]
fn test_unsuccessful_add_spot_asset_existing_asset() {
    // Setup state, token:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    // Use the existing synthetic asset id so that the asset is already registered.
    let spot_asset_id = cfg.synthetic_cfg.synthetic_id;
    let risk_factor_first_tier_boundary = MAX_U128;
    let risk_factor_tier_size = 1;
    let risk_factor_tiers = array![10].span();
    let quorum = 1_u8;
    let resolution_factor = SYNTHETIC_RESOLUTION_FACTOR;
    let quantum = 1_u64;
    let erc20_contract_address = token_state.address;

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_spot_asset(
            asset_id: spot_asset_id,
            :erc20_contract_address,
            :quantum,
            :resolution_factor,
            :risk_factor_tiers,
            :risk_factor_first_tier_boundary,
            :risk_factor_tier_size,
            :quorum,
        );
}
#[test]
fn test_successful_vault_token_deposit() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let mut state = setup_state_with_active_asset(
        cfg: @cfg, token_state: @cfg.collateral_cfg.token_cfg.deploy(),
    );
    let user = Default::default();
    let vault_share_state = cfg.vault_share_cfg.token_state;

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_vault_collateral_asset(
            asset_id: cfg.vault_share_cfg.collateral_id,
            erc20_contract_address: cfg.vault_share_cfg.contract_address,
            quantum: cfg.vault_share_cfg.quantum,
            risk_factor_tiers: cfg.vault_share_cfg.risk_factor_tiers,
            risk_factor_first_tier_boundary: cfg.vault_share_cfg.risk_factor_first_tier_boundary,
            risk_factor_tier_size: cfg.vault_share_cfg.risk_factor_tier_size,
            quorum: 1_u8,
        );

    let user_deposit_amount = DEPOSIT_AMOUNT.into() * cfg.vault_share_cfg.quantum.into();
    init_position(cfg: @cfg, ref :state, :user);

    let starting_user_balance = user_deposit_amount * 3;

    // Fund user.
    vault_share_state.fund(recipient: user.address, amount: starting_user_balance);
    vault_share_state
        .approve(owner: user.address, spender: test_address(), amount: user_deposit_amount);

    // Setup parameters:
    let expected_time = Time::now().add(delta: Time::days(1));
    start_cheat_block_timestamp_global(block_timestamp: expected_time.into());

    // Check before deposit:
    validate_balance(vault_share_state, user.address, starting_user_balance);
    validate_balance(vault_share_state, test_address(), 0);
    let mut spy = snforge_std::spy_events();

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit_asset(
            asset_id: cfg.vault_share_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
    let deposit_hash = deposit_hash(
        token_address: vault_share_state.address,
        depositor: user.address,
        position_id: user.position_id,
        quantized_amount: DEPOSIT_AMOUNT,
        salt: user.salt_counter,
    );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_deposit_event_with_expected(
        spied_event: events[0],
        position_id: user.position_id,
        depositing_address: user.address,
        collateral_id: cfg.vault_share_cfg.collateral_id,
        quantized_amount: DEPOSIT_AMOUNT,
        unquantized_amount: DEPOSIT_AMOUNT * cfg.vault_share_cfg.quantum,
        deposit_request_hash: deposit_hash,
        salt: user.salt_counter,
    );

    // Check after deposit:
    validate_balance(vault_share_state, user.address, starting_user_balance - user_deposit_amount);
    validate_balance(vault_share_state, test_address(), user_deposit_amount.try_into().unwrap());
    let status = state.deposits.get_deposit_status(:deposit_hash);
    if let DepositStatus::PENDING(timestamp) = status {
        println!("Deposit timestamp: {:?}, expected: {:?}", timestamp, expected_time);
        assert!(timestamp == expected_time);
    } else {
        panic!("Deposit not found");
    }
}


#[test]
#[should_panic(expected: 'ASSET_NOT_EXISTS')]
fn test_unsuccessful_vault_token_deposit_unregistered_asset() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let mut state = setup_state_with_active_asset(
        cfg: @cfg, token_state: @cfg.collateral_cfg.token_cfg.deploy(),
    );
    let user = Default::default();

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    init_position(cfg: @cfg, ref :state, :user);

    // Setup parameters:
    let expected_time = Time::now().add(delta: Time::days(1));
    start_cheat_block_timestamp_global(block_timestamp: expected_time.into());

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit_asset(
            asset_id: cfg.vault_share_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: 1,
            salt: user.salt_counter,
        );
}

#[test]
#[should_panic(expected: 'NOT_SPOT_ASSET')]
fn test_unsuccessful_vault_token_deposit_synthetic_asset() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let mut state = setup_state_with_active_asset(
        cfg: @cfg, token_state: @cfg.collateral_cfg.token_cfg.deploy(),
    );
    let user = Default::default();

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    init_position(cfg: @cfg, ref :state, :user);

    // Setup parameters:
    let expected_time = Time::now().add(delta: Time::days(1));
    start_cheat_block_timestamp_global(block_timestamp: expected_time.into());

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit_asset(
            asset_id: cfg.synthetic_cfg.synthetic_id,
            position_id: user.position_id,
            quantized_amount: 1,
            salt: user.salt_counter,
        );
}
#[test]
fn test_successful_vault_token_cancel_deposit() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let mut state = setup_state_with_active_asset(
        cfg: @cfg, token_state: @cfg.collateral_cfg.token_cfg.deploy(),
    );
    let user = Default::default();
    let vault_share_state = cfg.vault_share_cfg.token_state;

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_vault_collateral_asset(
            asset_id: cfg.vault_share_cfg.collateral_id,
            erc20_contract_address: cfg.vault_share_cfg.contract_address,
            quantum: cfg.vault_share_cfg.quantum,
            risk_factor_tiers: cfg.vault_share_cfg.risk_factor_tiers,
            risk_factor_first_tier_boundary: cfg.vault_share_cfg.risk_factor_first_tier_boundary,
            risk_factor_tier_size: cfg.vault_share_cfg.risk_factor_tier_size,
            quorum: 1_u8,
        );

    let user_deposit_amount = DEPOSIT_AMOUNT.into() * cfg.vault_share_cfg.quantum.into();
    init_position(cfg: @cfg, ref :state, :user);

    let starting_user_balance = user_deposit_amount * 3;

    // Fund user.
    vault_share_state.fund(recipient: user.address, amount: starting_user_balance);
    vault_share_state
        .approve(owner: user.address, spender: test_address(), amount: user_deposit_amount);

    // Setup parameters:
    let expected_time = Time::now().add(delta: Time::days(1));
    start_cheat_block_timestamp_global(block_timestamp: expected_time.into());

    // Check before deposit:
    validate_balance(vault_share_state, user.address, starting_user_balance);
    validate_balance(vault_share_state, test_address(), 0);
    let mut spy = snforge_std::spy_events();

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit_asset(
            asset_id: cfg.vault_share_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
    let deposit_hash = deposit_hash(
        token_address: vault_share_state.address,
        depositor: user.address,
        position_id: user.position_id,
        quantized_amount: DEPOSIT_AMOUNT,
        salt: user.salt_counter,
    );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_deposit_event_with_expected(
        spied_event: events[0],
        position_id: user.position_id,
        depositing_address: user.address,
        collateral_id: cfg.vault_share_cfg.collateral_id,
        quantized_amount: DEPOSIT_AMOUNT,
        unquantized_amount: DEPOSIT_AMOUNT * cfg.vault_share_cfg.quantum,
        deposit_request_hash: deposit_hash,
        salt: user.salt_counter,
    );

    // Check after deposit:
    validate_balance(vault_share_state, user.address, starting_user_balance - user_deposit_amount);
    validate_balance(vault_share_state, test_address(), user_deposit_amount.try_into().unwrap());

    // Test:
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::weeks(2)).into(),
    );
    state
        .cancel_deposit(
            asset_id: cfg.vault_share_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_deposit_canceled_event_with_expected(
        spied_event: events[1],
        position_id: user.position_id,
        depositing_address: user.address,
        collateral_id: cfg.vault_share_cfg.collateral_id,
        quantized_amount: DEPOSIT_AMOUNT,
        unquantized_amount: DEPOSIT_AMOUNT * cfg.vault_share_cfg.quantum,
        deposit_request_hash: deposit_hash,
        salt: user.salt_counter,
    );

    // Check after deposit cancellation:
    validate_balance(vault_share_state, user.address, starting_user_balance);
    validate_balance(vault_share_state, test_address(), 0);
}

#[test]
fn test_successful_vault_share_process_deposit() {
    // Setup state, token and user:
    let cfg: PerpetualsInitConfig = Default::default();
    let mut state = setup_state_with_active_asset(
        cfg: @cfg, token_state: @cfg.collateral_cfg.token_cfg.deploy(),
    );
    let user = Default::default();
    let vault_share_state = cfg.vault_share_cfg.token_state;

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_vault_collateral_asset(
            asset_id: cfg.vault_share_cfg.collateral_id,
            erc20_contract_address: cfg.vault_share_cfg.contract_address,
            quantum: cfg.vault_share_cfg.quantum,
            risk_factor_tiers: cfg.vault_share_cfg.risk_factor_tiers,
            risk_factor_first_tier_boundary: cfg.vault_share_cfg.risk_factor_first_tier_boundary,
            risk_factor_tier_size: cfg.vault_share_cfg.risk_factor_tier_size,
            quorum: 1_u8,
        );

    let user_deposit_amount = DEPOSIT_AMOUNT.into() * cfg.vault_share_cfg.quantum.into();
    init_position(cfg: @cfg, ref :state, :user);

    let starting_user_balance = user_deposit_amount * 3;

    // Fund user.
    vault_share_state.fund(recipient: user.address, amount: starting_user_balance);
    vault_share_state
        .approve(owner: user.address, spender: test_address(), amount: user_deposit_amount);

    let mut spy = snforge_std::spy_events();

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user.address);
    state
        .deposit_asset(
            asset_id: cfg.vault_share_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );
    let deposit_hash = deposit_hash(
        token_address: vault_share_state.address,
        depositor: user.address,
        position_id: user.position_id,
        quantized_amount: DEPOSIT_AMOUNT,
        salt: user.salt_counter,
    );

    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state
        .process_deposit(
            operator_nonce: state.get_operator_nonce(),
            depositor: user.address,
            asset_id: cfg.vault_share_cfg.collateral_id,
            position_id: user.position_id,
            quantized_amount: DEPOSIT_AMOUNT,
            salt: user.salt_counter,
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_deposit_processed_event_with_expected(
        spied_event: events[1],
        position_id: user.position_id,
        depositing_address: user.address,
        collateral_id: cfg.vault_share_cfg.collateral_id,
        quantized_amount: DEPOSIT_AMOUNT,
        unquantized_amount: DEPOSIT_AMOUNT * cfg.vault_share_cfg.quantum,
        deposit_request_hash: deposit_hash,
        salt: user.salt_counter,
    );

    let status = state.deposits.get_deposit_status(:deposit_hash);
    assert!(status == DepositStatus::PROCESSED, "Deposit not processed");

    validate_asset_balance(
        ref :state,
        position_id: user.position_id,
        asset_id: cfg.vault_share_cfg.collateral_id,
        expected_balance: DEPOSIT_AMOUNT.into(),
    );
}

#[test]
fn test_price_tick_vault_share_asset() {
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_pending_vault_share(cfg: @cfg, token_state: @token_state);

    let mut spy = snforge_std::spy_events();
    let asset_name = 'VAULT_SHARE';
    let oracle1_name = 'ORCL1';
    let oracle1 = Oracle { oracle_name: oracle1_name, asset_name, key_pair: KEY_PAIR_1() };
    let vault_share_id = cfg.vault_share_cfg.collateral_id;
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.app_governor);
    state
        .add_oracle_to_asset(
            asset_id: vault_share_id,
            oracle_public_key: oracle1.key_pair.public_key,
            oracle_name: oracle1_name,
            :asset_name,
        );
    let old_time: u64 = Time::now().into();
    let new_time = Time::now().add(delta: MAX_ORACLE_PRICE_VALIDITY);
    assert!(state.assets.get_num_of_active_synthetic_assets() == 0);
    start_cheat_block_timestamp_global(block_timestamp: new_time.into());
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    //one unit of vault share is priced at $12
    let oracle_price: u128 = 12 * 10_u128.pow(18);
    let operator_nonce = state.get_operator_nonce();
    state
        .price_tick(
            :operator_nonce,
            asset_id: vault_share_id,
            :oracle_price,
            signed_prices: [
                oracle1.get_signed_price(:oracle_price, timestamp: old_time.try_into().unwrap())
            ]
                .span(),
        );

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_add_oracle_event_with_expected(
        spied_event: events[0],
        asset_id: vault_share_id,
        :asset_name,
        oracle_public_key: oracle1.key_pair.public_key,
        oracle_name: oracle1_name,
    );

    let expected_price = convert_oracle_to_perps_price(
        oracle_price: oracle_price, resolution_factor: cfg.vault_share_cfg.resolution_factor,
    );
    assert_asset_activated_event_with_expected(spied_event: events[1], asset_id: vault_share_id);
    assert_price_tick_event_with_expected(
        spied_event: events[2], asset_id: vault_share_id, price: expected_price,
    );
    assert!(state.assets.get_asset_config(vault_share_id).status == AssetStatus::ACTIVE);
    //check synthetic count is not incremented
    assert!(state.assets.get_num_of_active_synthetic_assets() == 0);

    let data = state.assets.get_timely_data(vault_share_id);
    assert!(data.last_price_update == new_time);
    assert!(data.price.value() == expected_price.value());
}

// Forced trade tests.

#[test]
fn test_successful_forced_trade_request() {
    // Setup state, token and users:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let user_a = Default::default();
    init_position(cfg: @cfg, ref :state, user: user_a);

    let user_b = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: user_b);

    // Fund user_a for premium cost
    let premium_cost: u64 = PREMIUM_COST;
    let quantum: u64 = cfg.collateral_cfg.quantum;
    let premium_amount: u128 = premium_cost.into() * quantum.into();
    token_state.fund(recipient: user_a.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user_a.address, spender: test_address(), amount: premium_amount);

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));
    let collateral_id = cfg.collateral_cfg.collateral_id;
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    let order_a = Order {
        position_id: user_a.position_id,
        salt: user_a.salt_counter,
        base_asset_id: synthetic_id,
        base_amount: 10,
        quote_asset_id: collateral_id,
        quote_amount: -5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
    };

    let order_b = Order {
        position_id: user_b.position_id,
        base_asset_id: synthetic_id,
        base_amount: -10,
        quote_asset_id: collateral_id,
        quote_amount: 5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
        salt: user_b.salt_counter,
    };

    // Compute forced trade hash for signatures
    let forced_trade = ForcedTrade { order_a, order_b };
    let hash_a = forced_trade.get_message_hash(user_a.get_public_key());
    let signature_a = user_a.sign_message(hash_a);
    let hash_b = forced_trade.get_message_hash(user_b.get_public_key());
    let signature_b = user_b.sign_message(hash_b);

    let mut spy = snforge_std::spy_events();

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user_a.address);
    state.forced_trade_request(:signature_a, :signature_b, :order_a, :order_b);

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_forced_trade_request_event_with_expected(
        spied_event: events[0],
        order_a_position_id: user_a.position_id,
        order_a_base_asset_id: synthetic_id,
        order_a_base_amount: 10,
        order_a_quote_asset_id: collateral_id,
        order_a_quote_amount: -5,
        fee_a_asset_id: collateral_id,
        fee_a_amount: 0,
        order_b_position_id: user_b.position_id,
        order_b_base_asset_id: synthetic_id,
        order_b_base_amount: -10,
        order_b_quote_asset_id: collateral_id,
        order_b_quote_amount: 5,
        fee_b_asset_id: collateral_id,
        fee_b_amount: 0,
        order_a_hash: order_a.get_message_hash(user_a.get_public_key()),
        order_b_hash: order_b.get_message_hash(user_b.get_public_key()),
    );

    // Check premium was transferred
    validate_balance(
        token_state, user_a.address, (USER_INIT_BALANCE - premium_amount).try_into().unwrap(),
    );
}

#[test]
fn test_successful_forced_trade_after_timelock() {
    // Setup state, token and users:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let user_a = Default::default();
    init_position(cfg: @cfg, ref :state, user: user_a);
    add_synthetic_to_position(
        ref :state,
        synthetic_id: cfg.synthetic_cfg.synthetic_id,
        position_id: user_a.position_id,
        balance: SYNTHETIC_BALANCE_AMOUNT,
    );

    let user_b = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: user_b);

    // Fund user_a for premium cost
    let premium_cost: u64 = PREMIUM_COST;
    let quantum: u64 = cfg.collateral_cfg.quantum;
    let premium_amount: u128 = premium_cost.into() * quantum.into();
    token_state.fund(recipient: user_a.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user_a.address, spender: test_address(), amount: premium_amount);

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(FORCED_ACTION_TIMELOCK * 2));
    let collateral_id = cfg.collateral_cfg.collateral_id;
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    let order_a = Order {
        position_id: user_a.position_id,
        salt: user_a.salt_counter,
        base_asset_id: synthetic_id,
        base_amount: -10,
        quote_asset_id: collateral_id,
        quote_amount: 5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
    };

    let order_b = Order {
        position_id: user_b.position_id,
        base_asset_id: synthetic_id,
        base_amount: 10,
        quote_asset_id: collateral_id,
        quote_amount: -5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
        salt: user_b.salt_counter,
    };

    // Request forced trade
    let forced_trade = ForcedTrade { order_a, order_b };
    let hash_a = forced_trade.get_message_hash(user_a.get_public_key());
    let signature_a = user_a.sign_message(hash_a);
    let hash_b = forced_trade.get_message_hash(user_b.get_public_key());
    let signature_b = user_b.sign_message(hash_b);

    cheat_caller_address_once(contract_address: test_address(), caller_address: user_a.address);
    state.forced_trade_request(:signature_a, :signature_b, :order_a, :order_b);

    // Check premium was deducted from user_a's token balance
    validate_balance(
        token_state, user_a.address, (USER_INIT_BALANCE - premium_amount).try_into().unwrap(),
    );

    // Wait for timelock
    let timelock = FORCED_ACTION_TIMELOCK;
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::seconds(timelock)).into(),
    );

    let mut spy = snforge_std::spy_events();

    // Test: Execute forced trade after timelock
    cheat_caller_address_once(contract_address: test_address(), caller_address: user_b.address);
    state.forced_trade(:order_a, :order_b);

    // Catch the event.
    let events = spy.get_events().emitted_by(test_address()).events;
    assert_forced_trade_event_with_expected(
        // _execute_trade emits a Trade event first, then forced_trade emits ForcedTrade event
        spied_event: events[1],
        order_a_position_id: user_a.position_id,
        order_a_base_asset_id: synthetic_id,
        order_a_base_amount: -10,
        order_a_quote_asset_id: collateral_id,
        order_a_quote_amount: 5,
        fee_a_asset_id: collateral_id,
        fee_a_amount: 0,
        order_b_position_id: user_b.position_id,
        order_b_base_asset_id: synthetic_id,
        order_b_base_amount: 10,
        order_b_quote_asset_id: collateral_id,
        order_b_quote_amount: -5,
        fee_b_asset_id: collateral_id,
        fee_b_amount: 0,
        actual_amount_base_a: -10,
        actual_amount_quote_a: 5,
        order_a_hash: order_a.get_message_hash(user_a.get_public_key()),
        order_b_hash: order_b.get_message_hash(user_b.get_public_key()),
    );

    // Check balances
    let position_a = state.positions.get_position_snapshot(position_id: user_a.position_id);
    let user_a_synthetic_balance = state
        .positions
        .get_synthetic_balance(position: position_a, :synthetic_id);
    assert!(user_a_synthetic_balance == (SYNTHETIC_BALANCE_AMOUNT - 10).into());

    let user_a_collateral_balance = state
        .positions
        .get_collateral_provisional_balance(position: position_a, provisional_delta: Option::None);
    assert!(
        user_a_collateral_balance == (COLLATERAL_BALANCE_AMOUNT.into()
            + order_a.quote_amount.into()),
    );

    let position_b = state.positions.get_position_snapshot(position_id: user_b.position_id);
    let user_b_synthetic_balance = state
        .positions
        .get_synthetic_balance(position: position_b, :synthetic_id);
    assert!(user_b_synthetic_balance == BalanceTrait::new(value: 10));

    let user_b_collateral_balance = state
        .positions
        .get_collateral_provisional_balance(position: position_b, provisional_delta: Option::None);
    assert!(
        user_b_collateral_balance == (COLLATERAL_BALANCE_AMOUNT.into()
            + order_b.quote_amount.into()),
    );
}

#[test]
#[should_panic(expected: 'REQUEST_ALREADY_PROCESSED')]
fn test_forced_trade_user_after_operator_executed() {
    // Setup state, token and users:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let user_a = Default::default();
    init_position(cfg: @cfg, ref :state, user: user_a);
    add_synthetic_to_position(
        ref :state,
        synthetic_id: cfg.synthetic_cfg.synthetic_id,
        position_id: user_a.position_id,
        balance: SYNTHETIC_BALANCE_AMOUNT,
    );

    let user_b = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: user_b);

    // Fund user_a for premium cost
    let premium_cost: u64 = PREMIUM_COST;
    let quantum: u64 = cfg.collateral_cfg.quantum;
    let premium_amount: u128 = premium_cost.into() * quantum.into();
    token_state.fund(recipient: user_a.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user_a.address, spender: test_address(), amount: premium_amount);

    // Setup parameters with long expiration so it remains valid after timelock.
    let expiration = Time::now().add(delta: Time::days(FORCED_ACTION_TIMELOCK * 2));
    let collateral_id = cfg.collateral_cfg.collateral_id;
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    let order_a = Order {
        position_id: user_a.position_id,
        salt: user_a.salt_counter,
        base_asset_id: synthetic_id,
        base_amount: -10,
        quote_asset_id: collateral_id,
        quote_amount: 5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
    };

    let order_b = Order {
        position_id: user_b.position_id,
        base_asset_id: synthetic_id,
        base_amount: 10,
        quote_asset_id: collateral_id,
        quote_amount: -5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
        salt: user_b.salt_counter,
    };

    // Request forced trade
    let forced_trade = ForcedTrade { order_a, order_b };
    let hash_a = forced_trade.get_message_hash(user_a.get_public_key());
    let signature_a = user_a.sign_message(hash_a);
    let hash_b = forced_trade.get_message_hash(user_b.get_public_key());
    let signature_b = user_b.sign_message(hash_b);

    cheat_caller_address_once(contract_address: test_address(), caller_address: user_a.address);
    state.forced_trade_request(:signature_a, :signature_b, :order_a, :order_b);

    // Operator executes forced trade first (allowed before timelock).
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state.forced_trade(:order_a, :order_b);

    // After timelock the user tries to execute the same forced trade again.
    let timelock = FORCED_ACTION_TIMELOCK;
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::seconds(timelock)).into(),
    );
    cheat_caller_address_once(contract_address: test_address(), caller_address: user_b.address);
    state.forced_trade(:order_a, :order_b);
}

#[test]
fn test_successful_forced_trade_by_operator_before_timelock() {
    // Setup state, token and users:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let user_a = Default::default();
    init_position(cfg: @cfg, ref :state, user: user_a);
    add_synthetic_to_position(
        ref :state,
        synthetic_id: cfg.synthetic_cfg.synthetic_id,
        position_id: user_a.position_id,
        balance: SYNTHETIC_BALANCE_AMOUNT,
    );

    let user_b = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: user_b);

    // Fund user_a for premium cost
    let premium_cost: u64 = PREMIUM_COST;
    let quantum: u64 = cfg.collateral_cfg.quantum;
    let premium_amount: u128 = premium_cost.into() * quantum.into();
    token_state.fund(recipient: user_a.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user_a.address, spender: test_address(), amount: premium_amount);

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));
    let collateral_id = cfg.collateral_cfg.collateral_id;
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    let order_a = Order {
        position_id: user_a.position_id,
        salt: user_a.salt_counter,
        base_asset_id: synthetic_id,
        base_amount: -10,
        quote_asset_id: collateral_id,
        quote_amount: 5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
    };

    let order_b = Order {
        position_id: user_b.position_id,
        base_asset_id: synthetic_id,
        base_amount: 10,
        quote_asset_id: collateral_id,
        quote_amount: -5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
        salt: user_b.salt_counter,
    };

    // Request forced trade
    let forced_trade = ForcedTrade { order_a, order_b };
    let hash_a = forced_trade.get_message_hash(user_a.get_public_key());
    let signature_a = user_a.sign_message(hash_a);
    let hash_b = forced_trade.get_message_hash(user_b.get_public_key());
    let signature_b = user_b.sign_message(hash_b);

    cheat_caller_address_once(contract_address: test_address(), caller_address: user_a.address);
    state.forced_trade_request(:signature_a, :signature_b, :order_a, :order_b);

    // Test: Operator can execute before timelock
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state.forced_trade(:order_a, :order_b);
}

#[test]
#[should_panic(expected: 'REQUEST_ALREADY_PROCESSED')]
fn test_forced_trade_operator_after_user_executed() {
    // Setup state, token and users:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let user_a = Default::default();
    init_position(cfg: @cfg, ref :state, user: user_a);
    add_synthetic_to_position(
        ref :state,
        synthetic_id: cfg.synthetic_cfg.synthetic_id,
        position_id: user_a.position_id,
        balance: SYNTHETIC_BALANCE_AMOUNT,
    );

    let user_b = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: user_b);

    // Fund user_a for premium cost
    let premium_cost: u64 = PREMIUM_COST;
    let quantum: u64 = cfg.collateral_cfg.quantum;
    let premium_amount: u128 = premium_cost.into() * quantum.into();
    token_state.fund(recipient: user_a.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user_a.address, spender: test_address(), amount: premium_amount);

    // Setup parameters with long expiration so it remains valid after timelock.
    let expiration = Time::now().add(delta: Time::days(FORCED_ACTION_TIMELOCK * 2));
    let collateral_id = cfg.collateral_cfg.collateral_id;
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    let order_a = Order {
        position_id: user_a.position_id,
        salt: user_a.salt_counter,
        base_asset_id: synthetic_id,
        base_amount: -10,
        quote_asset_id: collateral_id,
        quote_amount: 5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
    };

    let order_b = Order {
        position_id: user_b.position_id,
        base_asset_id: synthetic_id,
        base_amount: 10,
        quote_asset_id: collateral_id,
        quote_amount: -5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
        salt: user_b.salt_counter,
    };

    // Request forced trade
    let forced_trade = ForcedTrade { order_a, order_b };
    let hash_a = forced_trade.get_message_hash(user_a.get_public_key());
    let signature_a = user_a.sign_message(hash_a);
    let hash_b = forced_trade.get_message_hash(user_b.get_public_key());
    let signature_b = user_b.sign_message(hash_b);

    cheat_caller_address_once(contract_address: test_address(), caller_address: user_a.address);
    state.forced_trade_request(:signature_a, :signature_b, :order_a, :order_b);

    // Non-operator user executes forced trade after timelock.
    let timelock = FORCED_ACTION_TIMELOCK;
    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::seconds(timelock)).into(),
    );
    cheat_caller_address_once(contract_address: test_address(), caller_address: user_b.address);
    state.forced_trade(:order_a, :order_b);

    // Operator tries to execute the same forced trade again.
    cheat_caller_address_once(contract_address: test_address(), caller_address: cfg.operator);
    state.forced_trade(:order_a, :order_b);
}

#[test]
#[should_panic(expected: 'FORCED_WAIT_REQUIRED')]
fn test_forced_trade_before_timelock_non_operator() {
    // Setup state, token and users:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let user_a = Default::default();
    init_position(cfg: @cfg, ref :state, user: user_a);

    let user_b = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: user_b);

    // Fund user_a for premium cost
    let premium_cost: u64 = PREMIUM_COST;
    let quantum: u64 = cfg.collateral_cfg.quantum;
    let premium_amount: u128 = premium_cost.into() * quantum.into();
    token_state.fund(recipient: user_a.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    token_state.approve(owner: user_a.address, spender: test_address(), amount: premium_amount);

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));
    let collateral_id = cfg.collateral_cfg.collateral_id;
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    let order_a = Order {
        position_id: user_a.position_id,
        salt: user_a.salt_counter,
        base_asset_id: synthetic_id,
        base_amount: -10,
        quote_asset_id: collateral_id,
        quote_amount: 5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
    };

    let order_b = Order {
        position_id: user_b.position_id,
        base_asset_id: synthetic_id,
        base_amount: 10,
        quote_asset_id: collateral_id,
        quote_amount: -5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
        salt: user_b.salt_counter,
    };

    // Request forced trade
    let forced_trade = ForcedTrade { order_a, order_b };
    let hash_a = forced_trade.get_message_hash(user_a.get_public_key());
    let signature_a = user_a.sign_message(hash_a);
    let hash_b = forced_trade.get_message_hash(user_b.get_public_key());
    let signature_b = user_b.sign_message(hash_b);

    cheat_caller_address_once(contract_address: test_address(), caller_address: user_a.address);
    state.forced_trade_request(:signature_a, :signature_b, :order_a, :order_b);

    // Test: Try to execute before timelock (non-operator)
    cheat_caller_address_once(contract_address: test_address(), caller_address: user_b.address);
    state.forced_trade(:order_a, :order_b);
}

#[test]
#[should_panic(expected: 'ERC20: insufficient balance')]
fn test_forced_trade_request_insufficient_premium() {
    // Setup state, token and users:
    let cfg: PerpetualsInitConfig = Default::default();
    let token_state = cfg.collateral_cfg.token_cfg.deploy();
    let mut state = setup_state_with_active_asset(cfg: @cfg, token_state: @token_state);

    let user_a = Default::default();
    init_position(cfg: @cfg, ref :state, user: user_a);

    let user_b = UserTrait::new(position_id: POSITION_ID_200, key_pair: KEY_PAIR_2());
    init_position(cfg: @cfg, ref :state, user: user_b);

    // Don't fund user_a or approve insufficient amount
    let quantum = cfg.collateral_cfg.quantum;
    let insufficient_amount = (PREMIUM_COST - 1).into() * quantum.into();
    token_state.fund(recipient: user_a.address, amount: insufficient_amount);
    token_state
        .approve(
            owner: user_a.address, spender: test_address(), amount: (PREMIUM_COST * quantum).into(),
        );

    // Setup parameters:
    let expiration = Time::now().add(delta: Time::days(1));
    let collateral_id = cfg.collateral_cfg.collateral_id;
    let synthetic_id = cfg.synthetic_cfg.synthetic_id;

    let order_a = Order {
        position_id: user_a.position_id,
        salt: user_a.salt_counter,
        base_asset_id: synthetic_id,
        base_amount: 10,
        quote_asset_id: collateral_id,
        quote_amount: -5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
    };

    let order_b = Order {
        position_id: user_b.position_id,
        base_asset_id: synthetic_id,
        base_amount: -10,
        quote_asset_id: collateral_id,
        quote_amount: 5,
        fee_asset_id: collateral_id,
        fee_amount: 0,
        expiration,
        salt: user_b.salt_counter,
    };

    let forced_trade = ForcedTrade { order_a, order_b };
    let hash_a = forced_trade.get_message_hash(user_a.get_public_key());
    let signature_a = user_a.sign_message(hash_a);
    let hash_b = forced_trade.get_message_hash(user_b.get_public_key());
    let signature_b = user_b.sign_message(hash_b);

    // Test:
    cheat_caller_address_once(contract_address: test_address(), caller_address: user_a.address);
    state.forced_trade_request(:signature_a, :signature_b, :order_a, :order_b);
}
