use core::num::traits::Zero;
use openzeppelin::interfaces::erc20::{IERC20Dispatcher, IERC20DispatcherTrait};
use openzeppelin::interfaces::erc4626::{IERC4626Dispatcher, IERC4626DispatcherTrait};
use perpetuals::core::components::deposit::interface::{IDepositDispatcher, IDepositDispatcherTrait};
use perpetuals::core::components::positions::interface::{
    IPositionsDispatcher, IPositionsDispatcherTrait,
};
use perpetuals::core::components::snip::SNIP12MetadataImpl;
use perpetuals::core::core::Core::InternalCoreFunctions;
use perpetuals::core::types::position::PositionId;
use perpetuals::tests::constants::*;
use perpetuals::tests::test_utils::{
    PerpetualsInitConfig, User, UserTrait, deploy_account, init_by_dispatcher,
};
use snforge_std::signature::stark_curve::{StarkCurveKeyPairImpl, StarkCurveSignerImpl};
use snforge_std::{ContractClassTrait, DeclareResultTrait};
use starknet::ContractAddress;
use starkware_utils::math::abs::Abs;
use starkware_utils::storage::iterable_map::*;
use starkware_utils_testing::test_utils::{
    Deployable, TokenState, TokenTrait, cheat_caller_address_once,
};
use vault::interface::{IProtocolVaultDispatcher, IProtocolVaultDispatcherTrait};


#[derive(Drop)]
pub struct DeployedVault {
    pub contract_address: ContractAddress,
    pub erc20: IERC20Dispatcher,
    pub erc4626: IERC4626Dispatcher,
    pub protocol_vault: IProtocolVaultDispatcher,
}

pub fn deploy_protocol_vault_with_dispatcher(
    perps_address: ContractAddress,
    vault_position_id: PositionId,
    usdc_token_state: TokenState,
    initial_receiver: ContractAddress,
) -> DeployedVault {
    let owning_account = deploy_account(StarkCurveKeyPairImpl::generate());
    usdc_token_state.fund(owning_account, 1_000_000_000_u128);
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
    initial_receiver.serialize(ref calldata);
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
    DeployedVault { contract_address: contract_address, erc20, erc4626, protocol_vault }
}

#[test]
#[feature("safe_dispatcher")]
fn test_protocol_vault_initialisation_logic() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let usdc_token_state = cfg.collateral_cfg.token_cfg.deploy();
    let perps_contract_address = init_by_dispatcher(cfg: @cfg, token_state: @usdc_token_state);

    let deposit_dispatcher = IDepositDispatcher { contract_address: perps_contract_address };
    let position_dispatcher = IPositionsDispatcher { contract_address: perps_contract_address };

    let vault_user: User = Default::default();
    let depositing_user = UserTrait::new(
        position_id: PositionId { value: 21 }, key_pair: KEY_PAIR_1(),
    );

    cheat_caller_address_once(
        contract_address: perps_contract_address, caller_address: cfg.operator,
    );
    position_dispatcher
        .new_position(
            operator_nonce: 0,
            position_id: vault_user.position_id,
            owner_public_key: vault_user.get_public_key(),
            owner_account: Zero::zero(),
            owner_protection_enabled: true,
        );

    cheat_caller_address_once(
        contract_address: perps_contract_address, caller_address: cfg.operator,
    );
    position_dispatcher
        .new_position(
            operator_nonce: 1,
            position_id: depositing_user.position_id,
            owner_public_key: depositing_user.get_public_key(),
            owner_account: Zero::zero(),
            owner_protection_enabled: true,
        );

    // Deposit money for users.
    let VAULT_DEPOSIT_AMOUNT = 1000_u64;
    usdc_token_state
        .fund(recipient: vault_user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    usdc_token_state
        .approve(
            owner: vault_user.address,
            spender: perps_contract_address,
            amount: VAULT_DEPOSIT_AMOUNT.into() * cfg.collateral_cfg.quantum.into(),
        );

    usdc_token_state
        .fund(recipient: depositing_user.address, amount: USER_INIT_BALANCE.try_into().unwrap());
    usdc_token_state
        .approve(
            owner: depositing_user.address,
            spender: perps_contract_address,
            amount: VAULT_DEPOSIT_AMOUNT.into() * cfg.collateral_cfg.quantum.into(),
        );
    // deposit into vault
    cheat_caller_address_once(
        contract_address: perps_contract_address, caller_address: vault_user.address,
    );
    deposit_dispatcher
        .deposit_asset(
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: vault_user.position_id,
            quantized_amount: VAULT_DEPOSIT_AMOUNT,
            salt: vault_user.salt_counter,
        );
    cheat_caller_address_once(
        contract_address: perps_contract_address, caller_address: cfg.operator,
    );
    deposit_dispatcher
        .process_deposit(
            operator_nonce: 2,
            depositor: vault_user.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: vault_user.position_id,
            quantized_amount: VAULT_DEPOSIT_AMOUNT,
            salt: vault_user.salt_counter,
        );

    // deposit into user position
    cheat_caller_address_once(
        contract_address: perps_contract_address, caller_address: depositing_user.address,
    );
    deposit_dispatcher
        .deposit_asset(
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: depositing_user.position_id,
            quantized_amount: VAULT_DEPOSIT_AMOUNT,
            salt: depositing_user.salt_counter,
        );

    cheat_caller_address_once(
        contract_address: perps_contract_address, caller_address: cfg.operator,
    );
    deposit_dispatcher
        .process_deposit(
            operator_nonce: 3,
            depositor: depositing_user.address,
            asset_id: cfg.collateral_cfg.collateral_id,
            position_id: depositing_user.position_id,
            quantized_amount: VAULT_DEPOSIT_AMOUNT,
            salt: depositing_user.salt_counter,
        );

    let deployed_vault = deploy_protocol_vault_with_dispatcher(
        perps_address: perps_contract_address,
        vault_position_id: vault_user.position_id,
        usdc_token_state: usdc_token_state,
        initial_receiver: vault_user.address,
    );

    //state setup complete
    // check owning vault is set correctly
    assert_eq!(
        deployed_vault.protocol_vault.get_owning_position_id(), vault_user.position_id.value.into(),
    );

    //check total assets == TV of vault position
    let total_assets = deployed_vault.erc4626.total_assets();
    let tv_tr_of_vault = position_dispatcher.get_position_tv_tr(vault_user.position_id);
    assert_eq!(total_assets, tv_tr_of_vault.total_value.abs().into());

    let balance_of_perps_contract_before = usdc_token_state
        .balance_of(account: perps_contract_address);

    //simulate perps contract approving a transfer
    usdc_token_state
        .approve(
            owner: perps_contract_address,
            spender: deployed_vault.contract_address,
            amount: 500_u128,
        );
    cheat_caller_address_once(
        contract_address: deployed_vault.contract_address, caller_address: perps_contract_address,
    );
    //simulate the perps contract calling deposit
    let shares_minted = deployed_vault
        .erc4626
        .deposit(assets: 500_u256, receiver: perps_contract_address);

    // as there is TV = VAULT_DEPOSIT_AMOUNT and share count = VAULT_DEPOSIT_AMOUNT
    // depositing 500 assets should mint 500 shares
    assert_eq!(shares_minted, 500_u256);

    let balance_of_perps_contract_after = usdc_token_state
        .balance_of(account: perps_contract_address);

    // the vault should send back the same amount of tokens it received
    assert_eq!(balance_of_perps_contract_before, balance_of_perps_contract_after);

    //the perps contract should receive the minted shares
    let balance_of_vault_shares = deployed_vault.erc20.balance_of(perps_contract_address);
    assert_eq!(balance_of_vault_shares, shares_minted);
}


#[test]
#[feature("safe_dispatcher")]
#[should_panic(expected: 'POSITION_DOESNT_EXIST')]
fn test_protocol_vault_fails_when_position_does_not_exist() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let usdc_token_state = cfg.collateral_cfg.token_cfg.deploy();
    let perps_contract_address = init_by_dispatcher(cfg: @cfg, token_state: @usdc_token_state);
    let vault_user: User = Default::default();

    deploy_protocol_vault_with_dispatcher(
        perps_address: perps_contract_address,
        vault_position_id: vault_user.position_id,
        usdc_token_state: usdc_token_state,
        initial_receiver: vault_user.address,
    );
}

#[test]
#[feature("safe_dispatcher")]
#[should_panic(expected: 'INITIAL_ASSETS_MUST_BE_POSITIVE')]
fn test_protocol_vault_fails_when_position_has_zero_tv() {
    // Setup:
    let cfg: PerpetualsInitConfig = Default::default();
    let usdc_token_state = cfg.collateral_cfg.token_cfg.deploy();
    let perps_contract_address = init_by_dispatcher(cfg: @cfg, token_state: @usdc_token_state);
    let vault_user: User = Default::default();
    let position_dispatcher = IPositionsDispatcher { contract_address: perps_contract_address };

    cheat_caller_address_once(
        contract_address: perps_contract_address, caller_address: cfg.operator,
    );
    position_dispatcher
        .new_position(
            operator_nonce: 0,
            position_id: vault_user.position_id,
            owner_public_key: vault_user.get_public_key(),
            owner_account: Zero::zero(),
            owner_protection_enabled: true,
        );

    deploy_protocol_vault_with_dispatcher(
        perps_address: perps_contract_address,
        vault_position_id: vault_user.position_id,
        usdc_token_state: usdc_token_state,
        initial_receiver: vault_user.address,
    );
}
