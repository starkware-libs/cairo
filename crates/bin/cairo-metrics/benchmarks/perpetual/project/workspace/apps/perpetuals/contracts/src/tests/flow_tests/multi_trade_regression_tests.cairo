use perpetuals::core::components::positions::interface::{
    IPositionsDispatcher, IPositionsDispatcherTrait,
};
use perpetuals::core::interface::{ICoreDispatcher, ICoreDispatcherTrait, Settlement};
use perpetuals::core::types::asset::AssetIdTrait;
use perpetuals::core::types::order::Order;
use perpetuals::core::types::position::PositionId;
use snforge_std::DeclareResultTrait;
use starknet::ContractAddress;
use starkware_utils::components::replaceability::interface::{
    IReplaceableDispatcher, IReplaceableDispatcherTrait, ImplementationData,
};
use starkware_utils::time::time::Timestamp;
use starkware_utils_testing::test_utils::cheat_caller_address_once;

//=============================================================================
// REGRESSION TESTS FOR CONTRACT UPGRADE
//=============================================================================
//
// This file contains regression tests that validate the upgrade from the original
// contract implementation to the optimized version. The primary goal is to ensure
// that the upgrade does not break the existing calculations and business logic.
//
// Test Structure:
// ---------------
// 1. test_mainnet_data_three_trades_original_code():
//    - Tests the behavior BEFORE the upgrade using the original contract code
//    - Executes three sequential trades using real mainnet data
//    - Verifies position total_value (tv) and total_risk (tr) calculations
//
// 2. test_mainnet_data_three_trades_upgraded_code():
//    - Tests the behavior AFTER the upgrade using the new optimized contract code
//    - Executes the same three sequential trades with identical parameters
//    - Verifies that tv_tr calculations produce identical results to the original
//
// 3. test_mainnet_data_multi_trade_upgraded_code():
//    - Tests the new multi_trade functionality introduced in the upgrade
//    - Executes all three trades in a single multi_trade call
//    - Verifies that the final results match the sequential trade approach
//
// Data Source:
// ------------
// All test data (addresses, transaction parameters, expected values) are taken
// from real mainnet deployment at block 1978107. The expected tv_tr values were
// captured before optimization methods were applied.
// Tx url:
// https://voyager.online/tx/0x277c2183c663ec87dd629f95dfdb763201d30d46ab11d9b89498712a482df36
//
//=============================================================================

const CONTRACT_ADDRESS: ContractAddress =
    0x062da0780fae50d68cecaa5a051606dc21217ba290969b302db4dd99d2e9b470
    .try_into()
    .unwrap();

const OPERATOR_ADDRESS: ContractAddress =
    0x048ddc53f41523d2a6b40c3dff7f69f4bbac799cd8b2e3fc50d3de1d4119441f
    .try_into()
    .unwrap();

const UPGRADE_ADMIN: ContractAddress =
    0x0522e5ba327bfbd85138b29bde060a5340a460706b00ae2e10e6d2a16fbf8c57
    .try_into()
    .unwrap();

const POSITION_ID_0x31357: PositionId = PositionId { value: 0x31357 };
const POSITION_ID_0x1f4: PositionId = PositionId { value: 0x1f4 };
const POSITION_ID_0x19905: PositionId = PositionId { value: 0x19905 };
const POSITION_ID_0x1f9: PositionId = PositionId { value: 0x1f9 };

const OPERATOR_NONCE: u64 = 0x590178;

// Trades:

fn SETTLEMENT_1() -> Settlement {
    Settlement {
        signature_a: array![
            0x79efd35a94a5d01bc32f2d77a1d02148039d9ba2bff6fbfd9038d403107fb37,
            0x331559dfad6df7627fb6ad6e6bf4c0609f9250bd56b96c3f33b359d335a1fb0,
        ]
            .span(),
        signature_b: array![
            0x517e3a8def3b9e71e1653c224cef7d7f289f4e961babdb19b5f2997a7f0423b,
            0xb6d637a145ca0c11a026f6b040d64842ba6eaac5bd0be0a01b8f0fc02e4e9a,
        ]
            .span(),
        order_a: Order {
            position_id: POSITION_ID_0x31357,
            base_asset_id: AssetIdTrait::new(0x47524153532d310000000000000000),
            base_amount: 370000,
            quote_asset_id: AssetIdTrait::new(0x1),
            quote_amount: -53035800000,
            fee_asset_id: AssetIdTrait::new(0x1),
            fee_amount: 11933055,
            expiration: Timestamp { seconds: 0x69411c56 },
            salt: 0x1c9012f3,
        },
        order_b: Order {
            position_id: POSITION_ID_0x1f4,
            base_asset_id: AssetIdTrait::new(0x47524153532d310000000000000000),
            base_amount: -250340_i64,
            quote_asset_id: AssetIdTrait::new(0x1),
            quote_amount: 0x42dddc5d0.try_into().unwrap(),
            fee_asset_id: AssetIdTrait::new(0x1),
            fee_amount: 0x88f161.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68ca6cd4 },
            salt: 0x43cb5fd4,
        },
        actual_amount_base_a: 0x181a.try_into().unwrap(),
        actual_amount_quote_a: -442389000_i64,
        actual_fee_a: 0x184d1.try_into().unwrap(),
        actual_fee_b: 0,
    }
}

fn SETTLEMENT_2() -> Settlement {
    Settlement {
        signature_a: array![
            0x19b1c7ac98001e7088afab360d8fa836bf64ef8cf6a39f4809c9d0da7746422,
            0x4aa37e0fdfd590fd065d11da324cd85cfbb37125c134a9c050d25f30cc2d0a0,
        ]
            .span(),
        signature_b: array![
            0x9394f56dc7346d0986e43ae77df98b3ff4d4aa89da12d3f06f6809ceb55bd2,
            0x385aa2e614c1ac9b110da393bcbbf56a8e64f6fc38b3fd2e6dcad9c00dffcf5,
        ]
            .span(),
        order_a: Order {
            position_id: POSITION_ID_0x19905,
            base_asset_id: AssetIdTrait::new(0x5345492d3000000000000000000000),
            base_amount: 0x212.try_into().unwrap(),
            quote_asset_id: AssetIdTrait::new(0x1),
            quote_amount: -150853900_i64,
            fee_asset_id: AssetIdTrait::new(0x1),
            fee_amount: 0, // 0xb6157f
            expiration: Timestamp { seconds: 0x68ca7aad },
            salt: 0xfc98573b,
        },
        order_b: Order {
            position_id: POSITION_ID_0x1f9,
            base_asset_id: AssetIdTrait::new(0x5345492d3000000000000000000000),
            base_amount: -330_i64,
            quote_asset_id: AssetIdTrait::new(0x1),
            quote_amount: 0x5971c74.try_into().unwrap(),
            fee_asset_id: AssetIdTrait::new(0x1),
            fee_amount: 0xb72f.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68ca6cd8 },
            salt: 0x20023f2e,
        },
        actual_amount_base_a: 0x14a.try_into().unwrap(),
        actual_amount_quote_a: -93927900_i64,
        actual_fee_a: 0,
        actual_fee_b: 0x5bb9.try_into().unwrap(),
    }
}

fn SETTLEMENT_3() -> Settlement {
    Settlement {
        signature_a: array![
            0x46c193585dbb7636bd548999a92d8f079ebb2f5a7a32e7c7c83fd78f0282373,
            0x5e00816fad9e842e51c86d58b0d6c8e62ecb9c0aeedc5add38e5ee00898ec4e,
        ]
            .span(),
        signature_b: array![
            0x1a5946390580cdef0031142b2314268ee6ae444ccb5afcc9ac8bc9d79a13a15,
            0x6d15be97d8c3955f2337a38c963fdff07f17364d0b6437d4de28fe6c46d1a91,
        ]
            .span(),
        order_a: Order {
            position_id: POSITION_ID_0x1f4,
            base_asset_id: AssetIdTrait::new(0x4f4e444f2d31000000000000000000),
            base_amount: 0xead76.try_into().unwrap(),
            quote_asset_id: AssetIdTrait::new(0x1),
            quote_amount: -92795457700_i64,
            fee_asset_id: AssetIdTrait::new(0x1),
            fee_amount: 0x2c3f921.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68ca6cd9 },
            salt: 0x2d07228f,
        },
        order_b: Order {
            position_id: POSITION_ID_0x1f9,
            base_asset_id: AssetIdTrait::new(0x4f4e444f2d31000000000000000000),
            base_amount: -260_i64,
            quote_asset_id: AssetIdTrait::new(0x1),
            quote_amount: 0x17e0288.try_into().unwrap(),
            fee_asset_id: AssetIdTrait::new(0x1),
            fee_amount: 0x30e6.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68ca6cda },
            salt: 0x3f211f2d,
        },
        actual_amount_base_a: 0x104.try_into().unwrap(),
        actual_amount_quote_a: -25082200_i64,
        actual_fee_a: 0,
        actual_fee_b: 0x187e.try_into().unwrap(),
    }
}

// State of the positions:

const INIT_TV_POSITION_A: i128 = 5528075951;
const INIT_TR_POSITION_A: u128 = 995123884;

const INIT_TV_POSITION_B: i128 = 3150202117392;
const INIT_TR_POSITION_B: u128 = 453149775459;

const STAGE_1_TV_POSITION_A: i128 = 5528183512;
const STAGE_1_TR_POSITION_A: u128 = 1017253689;

const STAGE_1_TV_POSITION_B: i128 = 3150201910294;
const STAGE_1_TR_POSITION_B: u128 = 453194035068;

const STAGE_2_TV_POSITION_A: i128 = 3684910683;
const STAGE_2_TR_POSITION_A: u128 = 64512271;

const STAGE_2_TV_POSITION_B: i128 = 2743004169;
const STAGE_2_TR_POSITION_B: u128 = 251421683;

const STAGE_3_TV_POSITION_A: i128 = 3150201860393;
const STAGE_3_TR_POSITION_A: u128 = 453193284099;

const STAGE_3_TV_POSITION_B: i128 = 2743047800;
const STAGE_3_TR_POSITION_B: u128 = 252172652;

fn replace_to_new_implementation() {
    let replaceability_dispatcher = IReplaceableDispatcher { contract_address: CONTRACT_ADDRESS };

    // Declare the new code.
    let core_contract = snforge_std::declare("Core").unwrap().contract_class();
    let new_class_hash = core_contract.class_hash;

    // Create the implementation data
    let implementation_data = ImplementationData {
        impl_hash: *new_class_hash, eic_data: None, final: false,
    };

    cheat_caller_address_once(contract_address: CONTRACT_ADDRESS, caller_address: UPGRADE_ADMIN);
    replaceability_dispatcher.add_new_implementation(implementation_data);

    cheat_caller_address_once(contract_address: CONTRACT_ADDRESS, caller_address: UPGRADE_ADMIN);
    replaceability_dispatcher.replace_to(implementation_data);
}

#[test]
#[fork(url: "https://rpc.pathfinder.equilibrium.co/mainnet/rpc/v0_10", block_number: 1978107)]
fn test_mainnet_data_three_trades_original_code() {
    // Setup:
    let core_dispatcher = ICoreDispatcher { contract_address: CONTRACT_ADDRESS };
    let positions_dispatcher = IPositionsDispatcher { contract_address: CONTRACT_ADDRESS };

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x31357);
    assert_eq!(tv_tr.total_value, INIT_TV_POSITION_A);
    assert_eq!(tv_tr.total_risk, INIT_TR_POSITION_A);

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x1f4);
    assert_eq!(tv_tr.total_value, INIT_TV_POSITION_B);
    assert_eq!(tv_tr.total_risk, INIT_TR_POSITION_B);

    // Start trading.
    let settlement_1 = SETTLEMENT_1();
    let settlement_2 = SETTLEMENT_2();
    let settlement_3 = SETTLEMENT_3();

    cheat_caller_address_once(contract_address: CONTRACT_ADDRESS, caller_address: OPERATOR_ADDRESS);
    core_dispatcher
        .trade(
            operator_nonce: OPERATOR_NONCE,
            signature_a: settlement_1.signature_a,
            signature_b: settlement_1.signature_b,
            order_a: settlement_1.order_a,
            order_b: settlement_1.order_b,
            actual_amount_base_a: settlement_1.actual_amount_base_a,
            actual_amount_quote_a: settlement_1.actual_amount_quote_a,
            actual_fee_a: settlement_1.actual_fee_a,
            actual_fee_b: settlement_1.actual_fee_b,
        );

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x31357);
    assert_eq!(tv_tr.total_value, STAGE_1_TV_POSITION_A);
    assert_eq!(tv_tr.total_risk, STAGE_1_TR_POSITION_A);

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x1f4);
    assert_eq!(tv_tr.total_value, STAGE_1_TV_POSITION_B);
    assert_eq!(tv_tr.total_risk, STAGE_1_TR_POSITION_B);

    // Trade 2:

    cheat_caller_address_once(contract_address: CONTRACT_ADDRESS, caller_address: OPERATOR_ADDRESS);
    core_dispatcher
        .trade(
            operator_nonce: OPERATOR_NONCE + 1,
            signature_a: settlement_2.signature_a,
            signature_b: settlement_2.signature_b,
            order_a: settlement_2.order_a,
            order_b: settlement_2.order_b,
            actual_amount_base_a: settlement_2.actual_amount_base_a,
            actual_amount_quote_a: settlement_2.actual_amount_quote_a,
            actual_fee_a: settlement_2.actual_fee_a,
            actual_fee_b: settlement_2.actual_fee_b,
        );

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x19905);
    assert_eq!(tv_tr.total_value, STAGE_2_TV_POSITION_A);
    assert_eq!(tv_tr.total_risk, STAGE_2_TR_POSITION_A);

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x1f9);
    assert_eq!(tv_tr.total_value, STAGE_2_TV_POSITION_B);
    assert_eq!(tv_tr.total_risk, STAGE_2_TR_POSITION_B);

    // Trade 3:

    cheat_caller_address_once(contract_address: CONTRACT_ADDRESS, caller_address: OPERATOR_ADDRESS);
    core_dispatcher
        .trade(
            operator_nonce: OPERATOR_NONCE + 2,
            signature_a: settlement_3.signature_a,
            signature_b: settlement_3.signature_b,
            order_a: settlement_3.order_a,
            order_b: settlement_3.order_b,
            actual_amount_base_a: settlement_3.actual_amount_base_a,
            actual_amount_quote_a: settlement_3.actual_amount_quote_a,
            actual_fee_a: settlement_3.actual_fee_a,
            actual_fee_b: settlement_3.actual_fee_b,
        );

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x1f4);
    assert_eq!(tv_tr.total_value, STAGE_3_TV_POSITION_A);
    assert_eq!(tv_tr.total_risk, STAGE_3_TR_POSITION_A);

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x1f9);
    assert_eq!(tv_tr.total_value, STAGE_3_TV_POSITION_B);
    assert_eq!(tv_tr.total_risk, STAGE_3_TR_POSITION_B);
}


#[test]
#[fork(url: "https://rpc.pathfinder.equilibrium.co/mainnet/rpc/v0_10", block_number: 1978107)]
fn test_mainnet_data_three_trades_upgraded_code() {
    // Setup:
    let core_dispatcher = ICoreDispatcher { contract_address: CONTRACT_ADDRESS };
    let positions_dispatcher = IPositionsDispatcher { contract_address: CONTRACT_ADDRESS };

    // Replace:
    replace_to_new_implementation();

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x31357);
    assert_eq!(tv_tr.total_value, INIT_TV_POSITION_A);
    assert_eq!(tv_tr.total_risk, INIT_TR_POSITION_A);

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x1f4);
    assert_eq!(tv_tr.total_value, INIT_TV_POSITION_B);
    assert_eq!(tv_tr.total_risk, INIT_TR_POSITION_B);

    // Start trading.
    let settlement_1 = SETTLEMENT_1();
    let settlement_2 = SETTLEMENT_2();
    let settlement_3 = SETTLEMENT_3();

    cheat_caller_address_once(contract_address: CONTRACT_ADDRESS, caller_address: OPERATOR_ADDRESS);
    core_dispatcher
        .trade(
            operator_nonce: OPERATOR_NONCE,
            signature_a: settlement_1.signature_a,
            signature_b: settlement_1.signature_b,
            order_a: settlement_1.order_a,
            order_b: settlement_1.order_b,
            actual_amount_base_a: settlement_1.actual_amount_base_a,
            actual_amount_quote_a: settlement_1.actual_amount_quote_a,
            actual_fee_a: settlement_1.actual_fee_a,
            actual_fee_b: settlement_1.actual_fee_b,
        );

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x31357);
    assert_eq!(tv_tr.total_value, STAGE_1_TV_POSITION_A);
    assert_eq!(tv_tr.total_risk, STAGE_1_TR_POSITION_A);

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x1f4);
    assert_eq!(tv_tr.total_value, STAGE_1_TV_POSITION_B);
    assert_eq!(tv_tr.total_risk, STAGE_1_TR_POSITION_B);

    // Trade 2:

    cheat_caller_address_once(contract_address: CONTRACT_ADDRESS, caller_address: OPERATOR_ADDRESS);
    core_dispatcher
        .trade(
            operator_nonce: OPERATOR_NONCE + 1,
            signature_a: settlement_2.signature_a,
            signature_b: settlement_2.signature_b,
            order_a: settlement_2.order_a,
            order_b: settlement_2.order_b,
            actual_amount_base_a: settlement_2.actual_amount_base_a,
            actual_amount_quote_a: settlement_2.actual_amount_quote_a,
            actual_fee_a: settlement_2.actual_fee_a,
            actual_fee_b: settlement_2.actual_fee_b,
        );

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x19905);
    assert_eq!(tv_tr.total_value, STAGE_2_TV_POSITION_A);
    assert_eq!(tv_tr.total_risk, STAGE_2_TR_POSITION_A);

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x1f9);
    assert_eq!(tv_tr.total_value, STAGE_2_TV_POSITION_B);
    assert_eq!(tv_tr.total_risk, STAGE_2_TR_POSITION_B);

    // Trade 3:

    cheat_caller_address_once(contract_address: CONTRACT_ADDRESS, caller_address: OPERATOR_ADDRESS);
    core_dispatcher
        .trade(
            operator_nonce: OPERATOR_NONCE + 2,
            signature_a: settlement_3.signature_a,
            signature_b: settlement_3.signature_b,
            order_a: settlement_3.order_a,
            order_b: settlement_3.order_b,
            actual_amount_base_a: settlement_3.actual_amount_base_a,
            actual_amount_quote_a: settlement_3.actual_amount_quote_a,
            actual_fee_a: settlement_3.actual_fee_a,
            actual_fee_b: settlement_3.actual_fee_b,
        );

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x1f4);
    assert_eq!(tv_tr.total_value, STAGE_3_TV_POSITION_A);
    assert_eq!(tv_tr.total_risk, STAGE_3_TR_POSITION_A);

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x1f9);
    assert_eq!(tv_tr.total_value, STAGE_3_TV_POSITION_B);
    assert_eq!(tv_tr.total_risk, STAGE_3_TR_POSITION_B);
}

#[test]
#[fork(url: "https://rpc.pathfinder.equilibrium.co/mainnet/rpc/v0_10", block_number: 1978107)]
fn test_mainnet_data_multi_trade_upgraded_code() {
    // Setup:
    let core_dispatcher = ICoreDispatcher { contract_address: CONTRACT_ADDRESS };
    let positions_dispatcher = IPositionsDispatcher { contract_address: CONTRACT_ADDRESS };

    // Replace:
    replace_to_new_implementation();

    // Check the initial state:
    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x31357);
    assert_eq!(tv_tr.total_value, INIT_TV_POSITION_A);
    assert_eq!(tv_tr.total_risk, INIT_TR_POSITION_A);

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x1f4);
    assert_eq!(tv_tr.total_value, INIT_TV_POSITION_B);
    assert_eq!(tv_tr.total_risk, INIT_TR_POSITION_B);

    // Execute the multi-trade:
    cheat_caller_address_once(contract_address: CONTRACT_ADDRESS, caller_address: OPERATOR_ADDRESS);
    core_dispatcher
        .multi_trade(
            operator_nonce: OPERATOR_NONCE,
            trades: array![SETTLEMENT_1(), SETTLEMENT_2(), SETTLEMENT_3()].span(),
        );

    // Check the final state of the 4 positions:

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x31357);
    assert_eq!(tv_tr.total_value, STAGE_1_TV_POSITION_A);
    assert_eq!(tv_tr.total_risk, STAGE_1_TR_POSITION_A);

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x1f4);
    assert_eq!(tv_tr.total_value, STAGE_3_TV_POSITION_A);
    assert_eq!(tv_tr.total_risk, STAGE_3_TR_POSITION_A);

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x19905);
    assert_eq!(tv_tr.total_value, STAGE_2_TV_POSITION_A);
    assert_eq!(tv_tr.total_risk, STAGE_2_TR_POSITION_A);

    let tv_tr = positions_dispatcher.get_position_tv_tr(position_id: POSITION_ID_0x1f9);
    assert_eq!(tv_tr.total_value, STAGE_3_TV_POSITION_B);
    assert_eq!(tv_tr.total_risk, STAGE_3_TR_POSITION_B);
}

