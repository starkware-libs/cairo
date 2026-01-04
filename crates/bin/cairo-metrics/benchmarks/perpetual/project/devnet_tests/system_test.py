import pytest
import pytest_asyncio
from starknet_py.cairo.felt import encode_shortstring
from devnet_tests.perpetuals_test_utils import PerpetualsTestUtils


@pytest_asyncio.fixture(autouse=True)
async def upgade_test_utils(test_utils: PerpetualsTestUtils):
    """Upgrade the perpetuals contract to the latest version and register the external components."""

    # Fund the accounts
    await test_utils.fund_account(
        test_utils.known_accounts["upgrade_governor"].address, 10_000_000_000 * 10**18
    )

    # Upgrade the perpetuals contract to the latest version
    await test_utils.upgrade_perpetuals_contract()

    # Register and activate the external components
    await test_utils.register_and_activate_external_component("perpetuals_VaultsManager", "VAULTS")
    await test_utils.register_and_activate_external_component(
        "perpetuals_WithdrawalManager", "WITHDRAWALS"
    )
    await test_utils.register_and_activate_external_component(
        "perpetuals_TransferManager", "TRANSFERS"
    )
    await test_utils.register_and_activate_external_component(
        "perpetuals_LiquidationManager", "LIQUIDATIONS"
    )
    await test_utils.register_and_activate_external_component(
        "perpetuals_DeleverageManager", "DELEVERAGES"
    )
    await test_utils.register_and_activate_external_component(
        "perpetuals_DepositManager", "DEPOSITS"
    )
    await test_utils.register_and_activate_external_component("perpetuals_AssetsManager", "ASSETS")


@pytest.mark.asyncio
async def test_helper_functions(test_utils: PerpetualsTestUtils):
    """Test helper functions in PerpetualsTestUtils."""

    # Test that we can access the contracts
    assert test_utils.known_contracts["operator"] is not None
    assert test_utils.known_contracts["app_governor"] is not None

    # Test new_account
    account = await test_utils.new_account()
    assert account is not None

    # Test helper functions with the created account
    assert test_utils.get_account_address(account) == account.address
    assert test_utils.get_account_public_key(account) == account.signer.public_key

    # Test get_operator_nonce
    nonce = await test_utils.get_operator_nonce()
    assert nonce >= 0

    # Test new_position
    position_id = await test_utils.new_position(account)
    assert position_id > 0
    assert test_utils.get_account_position_id(account) == position_id


@pytest.mark.asyncio
async def test_view_functions(test_utils: PerpetualsTestUtils):
    """Test view functions in the perpetuals contract."""

    # Test get_operator_nonce
    nonce = await test_utils.get_operator_nonce()
    assert nonce >= 0

    # Test get_collateral_asset_id
    collateral_asset_id = await test_utils.get_collateral_asset_id()
    assert collateral_asset_id == 1

    # Test get_base_collateral_token_contract
    token_contract = await test_utils.get_base_collateral_token_contract()
    assert token_contract > 0

    # Test get_num_of_active_synthetic_assets
    num_assets = await test_utils.get_num_of_active_synthetic_assets()
    assert num_assets == 76

    # Create account and position for position-related view functions
    account = await test_utils.new_account()
    position_id = await test_utils.new_position(account)

    # Test get_position_total_value
    position_tv = await test_utils.get_position_total_value(position_id)
    assert position_tv == 0


@pytest.mark.asyncio
async def test_deposit_withdraw(test_utils: PerpetualsTestUtils):
    """Test deposit and withdraw functionality."""

    # Create account and position
    account = await test_utils.new_account()
    position_id = await test_utils.new_position(account)

    # Test deposit
    deposit_amount = 10
    await test_utils.deposit(account, deposit_amount)

    # Verify position total value is equal to the deposit amount
    tv_after_deposit = await test_utils.get_position_total_value(position_id)
    assert tv_after_deposit == 10

    # Test withdraw
    withdraw_amount = 5
    await test_utils.withdraw(account, withdraw_amount)

    # Verify position total value decreased by withdraw amount
    tv_after_withdraw = await test_utils.get_position_total_value(position_id)
    assert tv_after_withdraw == 5


@pytest.mark.asyncio
async def test_asset_management(test_utils: PerpetualsTestUtils):
    """Test asset management functions."""

    num_assets_before = await test_utils.get_num_of_active_synthetic_assets()

    # Test add_synthetic_asset
    risk_factor_tiers = [100, 200, 400]
    risk_factor_first_tier_boundary = 100
    risk_factor_tier_size = 100
    quorum = 1
    resolution_factor = 100
    asset_id = await test_utils.add_synthetic_asset(
        risk_factor_tiers,
        risk_factor_first_tier_boundary,
        risk_factor_tier_size,
        quorum,
        resolution_factor,
    )
    assert asset_id > 0

    # Test add_oracle_to_asset
    oracle_account = await test_utils.new_account()
    await test_utils.add_oracle_to_asset(
        asset_id,
        test_utils.get_account_public_key(oracle_account),
        encode_shortstring("ORCL"),
        encode_shortstring("ASSET_NAME"),
    )

    # Test price_tick
    oracle_price = 100000000
    timestamp = test_utils.now_timestamp
    signed_price = test_utils.create_signed_price(
        oracle_account,
        oracle_price,
        timestamp,
        encode_shortstring("ASSET_NAME"),
        encode_shortstring("ORCL"),
    )
    await test_utils.price_tick(asset_id, oracle_price, [signed_price])

    # Verify the number of active synthetic assets increased
    num_assets_after = await test_utils.get_num_of_active_synthetic_assets()
    assert num_assets_after == num_assets_before + 1

    # Test funding_tick
    # Get timely data to verify funding index exists
    timely_data = await test_utils.get_asset_timely_data(asset_id)
    initial_funding_index = timely_data["funding_index"]["value"]

    # Execute funding tick with a diff for the new asset
    await test_utils.funding_tick({asset_id: 1024})

    # Verify funding index was updated
    timely_data_after = await test_utils.get_asset_timely_data(asset_id)
    final_funding_index = timely_data_after["funding_index"]["value"]
    assert final_funding_index - initial_funding_index == 1024


@pytest.mark.asyncio
async def test_trade(test_utils: PerpetualsTestUtils):
    deposit_amount = 10_000_000
    # Create two accounts and positions
    account_a = await test_utils.new_account()
    position_id_a = await test_utils.new_position(account_a)
    await test_utils.deposit(account_a, deposit_amount)

    account_b = await test_utils.new_account()
    position_id_b = await test_utils.new_position(account_b)
    await test_utils.deposit(account_b, deposit_amount)

    # Get initial position values
    tv_a_before = await test_utils.get_position_total_value(position_id_a)
    tv_b_before = await test_utils.get_position_total_value(position_id_b)

    # Test create_order
    base_asset_id = 0x47524153532D310000000000000000
    base_amount_a = 37
    quote_amount_a = -5303580
    fee_amount = 0
    expiration = test_utils.now_timestamp + 1000000000
    order_a = await test_utils.create_order(
        position_id_a, base_asset_id, base_amount_a, quote_amount_a, fee_amount, expiration
    )
    order_b = await test_utils.create_order(
        position_id_b, base_asset_id, -base_amount_a, -quote_amount_a, fee_amount, expiration
    )

    # Execute trade
    actual_fee_a = 0
    actual_fee_b = 0
    await test_utils.trade(
        account_a,
        account_b,
        order_a,
        order_b,
        base_amount_a,
        quote_amount_a,
        actual_fee_a,
        actual_fee_b,
    )

    # Verify position values changed
    tv_a_after = await test_utils.get_position_total_value(position_id_a)
    tv_b_after = await test_utils.get_position_total_value(position_id_b)
    assert tv_a_after != tv_a_before
    assert tv_b_after != tv_b_before
