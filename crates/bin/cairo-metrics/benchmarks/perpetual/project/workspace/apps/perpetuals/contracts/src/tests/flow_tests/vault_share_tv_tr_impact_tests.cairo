use perpetuals::tests::constants::*;
use perpetuals::tests::flow_tests::infra::*;
use perpetuals::tests::flow_tests::perps_tests_facade::*;


#[test]
fn test_shares_should_contribute_zero_until_activated() {
    let mut state: FlowTestBase = FlowTestBaseTrait::new();
    let vault_user = state.new_user_with_position();
    let depositing_user = state.new_user_with_position();
    let vault_init_deposit = state
        .facade
        .deposit(vault_user.account, vault_user.position_id, 5000_u64);
    state.facade.process_deposit(vault_init_deposit);
    let vault_config = state.facade.register_vault_share_spot_asset(vault_user);

    state
        .facade
        .process_deposit(
            state.facade.deposit(depositing_user.account, depositing_user.position_id, 1000_u64),
        );

    state
        .facade
        .validate_total_value(position_id: depositing_user.position_id, expected_total_value: 1000);

    state
        .facade
        .process_deposit(
            state
                .facade
                .deposit_into_vault(
                    vault: vault_config,
                    amount_to_invest: 1000,
                    min_shares_to_receive: 500,
                    :depositing_user,
                    receiving_user: depositing_user,
                ),
        );

    state
        .facade
        .validate_total_value(position_id: depositing_user.position_id, expected_total_value: 0);
}

#[test]
fn position_should_be_usable_with_inactive_vault_shares() {
    let mut state: FlowTestBase = FlowTestBaseTrait::new();
    let vault_user = state.new_user_with_position();
    let depositing_user = state.new_user_with_position();
    let receiving_user = state.new_user_with_position();
    let vault_init_deposit = state
        .facade
        .deposit(vault_user.account, vault_user.position_id, 5000_u64);
    state.facade.process_deposit(vault_init_deposit);
    let vault_config = state.facade.register_vault_share_spot_asset(vault_user);

    state
        .facade
        .process_deposit(
            state.facade.deposit(depositing_user.account, depositing_user.position_id, 2000_u64),
        );

    state
        .facade
        .validate_total_value(position_id: depositing_user.position_id, expected_total_value: 2000);

    state
        .facade
        .process_deposit(
            state
                .facade
                .deposit_into_vault(
                    vault: vault_config,
                    amount_to_invest: 1000,
                    min_shares_to_receive: 500,
                    :depositing_user,
                    receiving_user: depositing_user,
                ),
        );

    state
        .facade
        .transfer(
            state
                .facade
                .transfer_request(sender: depositing_user, recipient: receiving_user, amount: 200),
        );

    state
        .facade
        .validate_total_value(position_id: depositing_user.position_id, expected_total_value: 800);

    state
        .facade
        .validate_total_value(position_id: receiving_user.position_id, expected_total_value: 200);
}

#[test]
fn test_shares_should_contribute_to_tv_tr_after_activation() {
    let mut state: FlowTestBase = FlowTestBaseTrait::new();
    let vault_user = state.new_user_with_position();
    let depositing_user = state.new_user_with_position();
    let vault_init_deposit = state
        .facade
        .deposit(vault_user.account, vault_user.position_id, 5000_u64);
    state.facade.process_deposit(vault_init_deposit);
    let vault_config = state.facade.register_vault_share_spot_asset(vault_user);

    state
        .facade
        .process_deposit(
            state.facade.deposit(depositing_user.account, depositing_user.position_id, 1000_u64),
        );

    state
        .facade
        .validate_total_value(position_id: depositing_user.position_id, expected_total_value: 1000);

    state
        .facade
        .process_deposit(
            state
                .facade
                .deposit_into_vault(
                    vault: vault_config,
                    amount_to_invest: 1000,
                    min_shares_to_receive: 500,
                    :depositing_user,
                    receiving_user: depositing_user,
                ),
        );

    state.facade.price_tick(@vault_config.asset_info, 12);

    state
        .facade
        .validate_total_value(
            position_id: depositing_user.position_id, expected_total_value: 12 * 1000,
        );

    state
        .facade
        .validate_total_risk(
            position_id: depositing_user.position_id, expected_total_risk: 1200 // 10%
        );
}

