use openzeppelin::interfaces::erc20::IERC20DispatcherTrait;
use perpetuals::tests::constants::*;
use perpetuals::tests::flow_tests::infra::*;
use perpetuals::tests::flow_tests::perps_tests_facade::*;
use perpetuals::tests::test_utils::assert_with_error;
use snforge_std::start_cheat_block_timestamp_global;
use starkware_utils::time::time::Time;


#[test]
fn test_protocol_vault_deposit_vault_shares() {
    let mut state: FlowTestBase = FlowTestBaseTrait::new();
    let vault_user = state.new_user_with_position();
    let receiving_user = state.new_user_with_position();
    let vault_init_deposit = state
        .facade
        .deposit(vault_user.account, vault_user.position_id, 5000_u64);
    state.facade.process_deposit(vault_init_deposit);
    let vault_config = state.facade.register_vault_share_spot_asset(vault_user);
    let init_shares = vault_config
        .deployed_vault
        .erc20
        .balance_of(vault_config.deployed_vault.owning_account.address);

    assert_with_error(
        init_shares == 5000_u256, format!("Unexpected initial shares: {}", init_shares),
    );

    let deposit_info = state
        .facade
        .deposit_spot(
            vault_config.deployed_vault.owning_account,
            vault_config.asset_id,
            receiving_user.position_id,
            1000,
        );

    state.facade.process_deposit(deposit_info);

    let balance: i64 = state
        .facade
        .get_position_asset_balance(receiving_user.position_id, vault_config.asset_id)
        .into();

    assert_with_error(
        balance == 1000_i64.into(),
        format!("Unexpected balance: {}, expected: {}", balance, 1000_i64),
    );
}

#[test]
fn test_protocol_vault_cancel_deposit_vault_shares() {
    let mut state: FlowTestBase = FlowTestBaseTrait::new();
    let vault_user = state.new_user_with_position();
    let receiving_user = state.new_user_with_position();
    let deposit_info = state.facade.deposit(vault_user.account, vault_user.position_id, 5000_u64);
    state.facade.process_deposit(deposit_info);
    let vault_config = state.facade.register_vault_share_spot_asset(vault_user);

    let deposit_info = state
        .facade
        .deposit_spot(
            vault_config.deployed_vault.owning_account,
            vault_config.asset_id,
            receiving_user.position_id,
            1000,
        );

    start_cheat_block_timestamp_global(
        block_timestamp: Time::now().add(delta: Time::weeks(2)).into(),
    );
    state.facade.cancel_deposit(deposit_info);
    let balance: i64 = state
        .facade
        .get_position_asset_balance(receiving_user.position_id, vault_config.asset_id)
        .into();
    assert_with_error(
        balance == 0_i64.into(), format!("Unexpected balance: {}, expected: {}", balance, 0_i64),
    );
}
