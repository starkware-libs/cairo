use perpetuals::core::core::Core::InternalCoreFunctions;
use perpetuals::core::interface::{ICoreDispatcher, ICoreDispatcherTrait, Settlement};
use perpetuals::core::types::order::Order;
use perpetuals::core::types::position::PositionId;
use perpetuals::tests::constants::*;
use snforge_std::DeclareResultTrait;
use snforge_std::signature::stark_curve::StarkCurveSignerImpl;
use starknet::ContractAddress;
use starkware_utils::components::replaceability::interface::{
    IReplaceableDispatcher, IReplaceableDispatcherTrait, ImplementationData,
};
use starkware_utils::storage::iterable_map::*;
use starkware_utils::time::time::Timestamp;
use starkware_utils_testing::test_utils::cheat_caller_address_once;
use crate::core::components::snip::SNIP12MetadataImpl;

// Performance test for Core contract multi-trade execution.
//
// This test file is designed to measure the gas usage of executing multiple
// trades in the Core contract. It runs a total of 12 pre-defined settlements,
// each containing two orders, in a single `multi_trade` call.
//
// The test reports L1 and L2 gas consumption, allowing benchmarking of contract
// performance and efficiency.

// Constants
const OPERATOR_ADDRESS: ContractAddress =
    0x048ddc53f41523d2a6b40c3dff7f69f4bbac799cd8b2e3fc50d3de1d4119441f
    .try_into()
    .unwrap();
const CONTRACT_ADDRESS: ContractAddress =
    0x062da0780fae50d68cecaa5a051606dc21217ba290969b302db4dd99d2e9b470
    .try_into()
    .unwrap();

const DEPLOYER_ADDRESS: ContractAddress =
    0x0522e5ba327bfbd85138b29bde060a5340a460706b00ae2e10e6d2a16fbf8c57
    .try_into()
    .unwrap();

const CURRENT_OPERATOR_NONCE: u64 = 3486792;


fn settlement_1() -> Settlement {
    Settlement {
        signature_a: array![
            0x71fedaf53734966afd09513c40f50036bb742c06eef36c41e2aaa0d74022f99,
            0x71be0b92594231bdf28698d9beec79f0a68daa103f5dc57347925c63f76643c,
        ]
            .span(),
        signature_b: array![
            0x60a98b1e87f6e434331dc3f194afbb1f9bfdcc24e6099533ee9a02efc765275,
            0x4ed771fdfb4e5941f8c5daafc698ca78644a19212262cca7235faa7390b4a98,
        ]
            .span(),
        order_a: Order {
            position_id: PositionId { value: 0x19f1b.try_into().unwrap() },
            base_asset_id: 0x485950452d33000000000000000000.into(),
            base_amount: 0x3a98.try_into().unwrap(),
            quote_asset_id: 0x1.into(),
            quote_amount: -1321410000_i64,
            fee_asset_id: 0x1.into(),
            fee_amount: 0x50a71.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x6933e295.try_into().unwrap() },
            salt: 0x50a705a3.into(),
        },
        order_b: Order {
            position_id: PositionId { value: 0x30d7e.try_into().unwrap() },
            base_asset_id: 0x485950452d33000000000000000000.into(),
            base_amount: -6760_i64,
            quote_asset_id: 0x1.into(),
            quote_amount: 0x11d0c058.try_into().unwrap(),
            fee_asset_id: 0x1.into(),
            fee_amount: 0xe983.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd405a.try_into().unwrap() },
            salt: 0x3a3590e6.into(),
        },
        actual_amount_base_a: 0xfa,
        actual_amount_quote_a: -11053750_i64,
        actual_fee_a: 0xacb,
        actual_fee_b: 0x0,
    }
}

fn settlement_2() -> Settlement {
    Settlement {
        signature_a: array![
            0x7ca1d5af750dad6cfc97ac20fe45ea2a4a7269e18fd8fe8fb9243b62fce6aa5,
            0x357038901909841f6ed4930a769442fa042033c1a89db6539ae0896fd875fcf,
        ]
            .span(),
        signature_b: array![
            0x453eaeac0fccb0084931b0dcfb88bc70cc9889158cb09a7bc6b2abc8ca3cd10,
            0x5cf4778b3fdf08ce858b98c1e836c4ed543319a00630da481ba29a75ffe0c10,
        ]
            .span(),
        order_a: Order {
            position_id: PositionId { value: 0x31303.try_into().unwrap() },
            base_asset_id: 0x4554482d3400000000000000000000.into(),
            base_amount: 0x48a8.try_into().unwrap(),
            quote_asset_id: 0x1.into(),
            quote_amount: -17686368000_i64,
            fee_asset_id: 0x1.into(),
            fee_amount: 0x3cb8a9.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x6933f115.try_into().unwrap() },
            salt: 0x5dca0f41.into(),
        },
        order_b: Order {
            position_id: PositionId { value: 0x30d7e.try_into().unwrap() },
            base_asset_id: 0x4554482d3400000000000000000000.into(),
            base_amount: -4220_i64,
            quote_asset_id: 0x1.into(),
            quote_amount: 0x780e5830.try_into().unwrap(),
            fee_asset_id: 0x1.into(),
            fee_amount: 0x6259a.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd406b.try_into().unwrap() },
            salt: 0x4dda9a9.into(),
        },
        actual_amount_base_a: 0x136,
        actual_amount_quote_a: -147963000_i64,
        actual_fee_a: 0x820b,
        actual_fee_b: 0x0,
    }
}

fn settlement_3() -> Settlement {
    Settlement {
        signature_a: array![
            0xb2d7c5904593c6c37a03015c803762f23525c89abaa21cf2c4bb6674563dc4,
            0x78e9490d089c20c2be23ec6d661682d804db8bb8ee0aa38a4ff04506b9d0228,
        ]
            .span(),
        signature_b: array![
            0x4a93a8c9c5b961732a64ae6e6d35eec73faad38117388a27aa445cbdbb89b71,
            0x415e7e909a133da5eacfdb255fea2868f0e96c46247a6717e69860fb3501e5f,
        ]
            .span(),
        order_a: Order {
            position_id: PositionId { value: 0x30d7e.try_into().unwrap() },
            base_asset_id: 0x4254432d3600000000000000000000.into(),
            base_amount: 0xa3c.try_into().unwrap(),
            quote_asset_id: 0x1.into(),
            quote_amount: -301100880_i64,
            fee_asset_id: 0x1.into(),
            fee_amount: 0xeb3d.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd406a.try_into().unwrap() },
            salt: 0x9b1f91a.into(),
        },
        order_b: Order {
            position_id: PositionId { value: 0x31303.try_into().unwrap() },
            base_asset_id: 0x4254432d3600000000000000000000.into(),
            base_amount: -64970_i64,
            quote_asset_id: 0x1.into(),
            quote_amount: 0xfdca.try_into().unwrap(),
            fee_asset_id: 0x1.into(),
            fee_amount: 0xf.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x6933f12e.try_into().unwrap() },
            salt: 0x9756924.into(),
        },
        actual_amount_base_a: 0x438,
        actual_amount_quote_a: -124117920_i64,
        actual_fee_a: 0x0,
        actual_fee_b: 0x6d16,
    }
}

fn settlement_4() -> Settlement {
    Settlement {
        signature_a: array![
            0x5736fc7eab84bd1e6abd49c56aee0549f1a673a457f8cae0492557374699664,
            0x75715f143a8fe53d3f34e683d114ea4e8e805bc08ddfc81e5520910a9a2cef0,
        ]
            .span(),
        signature_b: array![
            0x43eec775b9f5480933bd44cfcd348a2097ccd69dc5f1de9a1421ad66de26177,
            0x130cb5847bf80846c707e7535900c02809abdebf9362c58e9fc335ec05a2be1,
        ]
            .span(),
        order_a: Order {
            position_id: PositionId { value: 0x19cb3.try_into().unwrap() },
            base_asset_id: 0x5452554d502d310000000000000000.into(),
            base_amount: 0x2710.try_into().unwrap(),
            quote_asset_id: 0x1.into(),
            quote_amount: -17412000000_i64,
            fee_asset_id: 0x1.into(),
            fee_amount: 0x3bc784.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x6933f223.try_into().unwrap() },
            salt: 0x3918958b.into(),
        },
        order_b: Order {
            position_id: PositionId { value: 0x1f4.try_into().unwrap() },
            base_asset_id: 0x5452554d502d310000000000000000.into(),
            base_amount: -27810_i64,
            quote_asset_id: 0x1.into(),
            quote_amount: 0x5a46fe8d0.try_into().unwrap(),
            fee_asset_id: 0x1.into(),
            fee_amount: 0xb8e351.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd402d.try_into().unwrap() },
            salt: 0x7ba67460.into(),
        },
        actual_amount_base_a: 0xfa,
        actual_amount_quote_a: -217850000_i64,
        actual_fee_a: 0xbf78,
        actual_fee_b: 0x0,
    }
}

fn settlement_5() -> Settlement {
    Settlement {
        signature_a: array![
            0x07e41bed15c380c3108667edda28624032fbb3f4629abffbd20c4d9627d9339,
            0x2992d95aaaf842f5c4dd692530c99d2718d0fae9a1ae49057a0a9aee59c601d,
        ]
            .span(),
        signature_b: array![
            0x34424f9d844e76e6fa024371d174678944ae4c0651c00d6ce12d3be006894bf,
            0x72f359e6175463da2f97c0ef462828d046ad5008bba22d46993eaa9f2b2a83a,
        ]
            .span(),
        order_a: Order {
            position_id: PositionId { value: 0x19cb3.try_into().unwrap() },
            base_asset_id: 0x454e412d3000000000000000000000.into(),
            base_amount: 0x2710.try_into().unwrap(),
            quote_asset_id: 0x1.into(),
            quote_amount: -14219400000_i64,
            fee_asset_id: 0x1.into(),
            fee_amount: 0x30d185.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x6933f2eb.try_into().unwrap() },
            salt: 0x1b37382b.into(),
        },
        order_b: Order {
            position_id: PositionId { value: 0x30d7e.try_into().unwrap() },
            base_asset_id: 0x454e412d3000000000000000000000.into(),
            base_amount: -420_i64,
            quote_asset_id: 0x1.into(),
            quote_amount: 0x11cc6328.try_into().unwrap(),
            fee_asset_id: 0x1.into(),
            fee_amount: 0xe94a.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd406a.try_into().unwrap() },
            salt: 0x6894e2cc.into(),
        },
        actual_amount_base_a: 0xfa,
        actual_amount_quote_a: -177742500_i64,
        actual_fee_a: 0x9c38,
        actual_fee_b: 0x0,
    }
}

fn settlement_6() -> Settlement {
    Settlement {
        signature_a: array![
            0x0357f8bad89fac0d175a5e9c77b076345e82a96cec0ef19b477c1d14ff9df1f5,
            0x070395b018d69c8721a9da7dde732190070abee1007560205d34b6abb96e466,
        ]
            .span(),
        signature_b: array![
            0x144a30993fb087dc7704eeb4df2ec0dd7745c916515e8fef3b7cdbdf0902769,
            0x177266c9238757be2a12765c549d5297b44f5ed531e5d7a45964c459fcc1c21,
        ]
            .span(),
        order_a: Order {
            position_id: PositionId { value: 0x1f4.try_into().unwrap() },
            base_asset_id: 0x424e422d3400000000000000000000.into(),
            base_amount: 0x96.try_into().unwrap(),
            quote_asset_id: 0x1.into(),
            quote_amount: -13084650_i64,
            fee_asset_id: 0x1.into(),
            fee_amount: 0x198f.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd4012.try_into().unwrap() },
            salt: 0x328da974.into(),
        },
        order_b: Order {
            position_id: PositionId { value: 0x19cb3.try_into().unwrap() },
            base_asset_id: 0x424e422d3400000000000000000000.into(),
            base_amount: -140000_i64,
            quote_asset_id: 0x1.into(),
            quote_amount: 0x222e0.try_into().unwrap(),
            fee_asset_id: 0x1.into(),
            fee_amount: 0x20.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x6933f456.try_into().unwrap() },
            salt: 0x0ba3b07a.into(),
        },
        actual_amount_base_a: 0x96,
        actual_amount_quote_a: -13084650_i64,
        actual_fee_a: 0x0,
        actual_fee_b: 0xb80,
    }
}


fn settlement_7() -> Settlement {
    Settlement {
        signature_a: array![
            0x01458cfe61f5710e270b2543ea2cba99ae90e7d0dbef931ff15276248818d82a,
            0x009e91b106564240139de5f1a23ecce57bc9986b0ee37a6f08cb7ca9ccf7fe58,
        ]
            .span(),
        signature_b: array![
            0x144a30993fb087dc7704eeb4df2ec0dd7745c916515e8fef3b7cdbdf0902769,
            0x177266c9238757be2a12765c549d5297b44f5ed531e5d7a45964c459fcc1c21,
        ]
            .span(),
        order_a: Order {
            position_id: PositionId { value: 0x1f4.try_into().unwrap() },
            base_asset_id: 0x424e422d3400000000000000000000.into(),
            base_amount: 0x8c.try_into().unwrap(),
            quote_asset_id: 0x1.into(),
            quote_amount: -12210520_i64,
            fee_asset_id: 0x1.into(),
            fee_amount: 0x17da.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd4012.try_into().unwrap() },
            salt: 0x5daac4c0.into(),
        },
        order_b: Order {
            position_id: PositionId { value: 0x19cb3.try_into().unwrap() },
            base_asset_id: 0x424e422d3400000000000000000000.into(),
            base_amount: -140000_i64,
            quote_asset_id: 0x1.into(),
            quote_amount: 0x222e0.try_into().unwrap(),
            fee_asset_id: 0x1.into(),
            fee_amount: 0x20.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x6933f456.try_into().unwrap() },
            salt: 0x0ba3b07a.into(),
        },
        actual_amount_base_a: 0x8c,
        actual_amount_quote_a: -12210520_i64,
        actual_fee_a: 0x0,
        actual_fee_b: 0xabb,
    }
}

fn settlement_8() -> Settlement {
    Settlement {
        signature_a: array![
            0x0d8841227b8c16ed1702b780ff19dedc17f4dfe5e82f286798e8aa4113627e2,
            0x0641bb275fbd425eb1bb19ea8c04713a016625765898f14e433f77dee0bafdb1,
        ]
            .span(),
        signature_b: array![
            0x144a30993fb087dc7704eeb4df2ec0dd7745c916515e8fef3b7cdbdf0902769,
            0x177266c9238757be2a12765c549d5297b44f5ed531e5d7a45964c459fcc1c21,
        ]
            .span(),
        order_a: Order {
            position_id: PositionId { value: 0x1f4.try_into().unwrap() },
            base_asset_id: 0x424e422d3400000000000000000000.into(),
            base_amount: 0x78.try_into().unwrap(),
            quote_asset_id: 0x1.into(),
            quote_amount: -10464600_i64,
            fee_asset_id: 0x1.into(),
            fee_amount: 0x1471.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd4012.try_into().unwrap() },
            salt: 0x7a78a72e.into(),
        },
        order_b: Order {
            position_id: PositionId { value: 0x19cb3.try_into().unwrap() },
            base_asset_id: 0x424e422d3400000000000000000000.into(),
            base_amount: -140000_i64,
            quote_asset_id: 0x1.into(),
            quote_amount: 0x222e0.try_into().unwrap(),
            fee_asset_id: 0x1.into(),
            fee_amount: 0x20.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x6933f456.try_into().unwrap() },
            salt: 0x0ba3b07a.into(),
        },
        actual_amount_base_a: 0x78,
        actual_amount_quote_a: -10464600_i64,
        actual_fee_a: 0x0,
        actual_fee_b: 0x932,
    }
}

fn settlement_9() -> Settlement {
    Settlement {
        signature_a: array![
            0x39c797bc172b26b479d3fd9a1f49f48d347173a5609b39e95d52be7015c55ee,
            0x18fea8abc691a8ecc63a69ae487810beefda7c9a359760ff4d937683ebffede,
        ]
            .span(),
        signature_b: array![
            0x144a30993fb087dc7704eeb4df2ec0dd7745c916515e8fef3b7cdbdf0902769,
            0x177266c9238757be2a12765c549d5297b44f5ed531e5d7a45964c459fcc1c21,
        ]
            .span(),
        order_a: Order {
            position_id: PositionId { value: 0x1a55c.try_into().unwrap() },
            base_asset_id: 0x424e422d3400000000000000000000.into(),
            base_amount: 0x3ca.try_into().unwrap(),
            quote_asset_id: 0x1.into(),
            quote_amount: -84584970_i64,
            fee_asset_id: 0x1.into(),
            fee_amount: 0x0.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd4de9.try_into().unwrap() },
            salt: 0xe33f4f60.into(),
        },
        order_b: Order {
            position_id: PositionId { value: 0x19cb3.try_into().unwrap() },
            base_asset_id: 0x424e422d3400000000000000000000.into(),
            base_amount: -140000_i64,
            quote_asset_id: 0x1.into(),
            quote_amount: 0x222e0.try_into().unwrap(),
            fee_asset_id: 0x1.into(),
            fee_amount: 0x20.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x6933f456.try_into().unwrap() },
            salt: 0x0ba3b07a.into(),
        },
        actual_amount_base_a: 0x3ca,
        actual_amount_quote_a: -84584970_i64,
        actual_fee_a: 0x0,
        actual_fee_b: 0x4a57,
    }
}

fn settlement_10() -> Settlement {
    Settlement {
        signature_a: array![
            0x7c78d4aec7fa7050eae731e419a2b92073bc5cff23329037004fff22d8d0906,
            0x5ba95166f4cbc1db27d7a4133ee14b9acbe08aebc18b1e0ac410c4b945e7206,
        ]
            .span(),
        signature_b: array![
            0x144a30993fb087dc7704eeb4df2ec0dd7745c916515e8fef3b7cdbdf0902769,
            0x177266c9238757be2a12765c549d5297b44f5ed531e5d7a45964c459fcc1c21,
        ]
            .span(),
        order_a: Order {
            position_id: PositionId { value: 0x186b0.try_into().unwrap() },
            base_asset_id: 0x424e422d3400000000000000000000.into(),
            base_amount: 0xb392.try_into().unwrap(),
            quote_asset_id: 0x1.into(),
            quote_amount: -4008446090_i64,
            fee_asset_id: 0x1.into(),
            fee_amount: 0x1e9500.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd421e.try_into().unwrap() },
            salt: 0x795044b2.into(),
        },
        order_b: Order {
            position_id: PositionId { value: 0x19cb3.try_into().unwrap() },
            base_asset_id: 0x424e422d3400000000000000000000.into(),
            base_amount: -140000_i64,
            quote_asset_id: 0x1.into(),
            quote_amount: 0x222e0.try_into().unwrap(),
            fee_asset_id: 0x1.into(),
            fee_amount: 0x20.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x6933f456.try_into().unwrap() },
            salt: 0x0ba3b07a.into(),
        },
        actual_amount_base_a: 0x3b6,
        actual_amount_quote_a: -82837150_i64,
        actual_fee_a: 0x0,
        actual_fee_b: 0x48ce,
    }
}

fn settlement_11() -> Settlement {
    Settlement {
        signature_a: array![
            0xa948e156fc6da0325347f59945bd391b89c88f9331d6604d7cf7320457a754,
            0x2c33af82050d54489afa61a5e5be82103ce9add9af06807d1266aec9ac95da6,
        ]
            .span(),
        signature_b: array![
            0x4ba7bf04ebbbea4b71aa8a8cdd00744d4bdad1a04f274a1fb7b5ad6905e0b46,
            0x70840bc4c3080c29d9b7d8932f455f79fa33dd8f3fdf363eb5fe58a85b3fbbc,
        ]
            .span(),
        order_a: Order {
            position_id: PositionId { value: 0x1f9.try_into().unwrap() },
            base_asset_id: 0x4144412d3100000000000000000000.into(),
            base_amount: 0x10e.try_into().unwrap(),
            quote_asset_id: 0x1.into(),
            quote_amount: -24597000_i64,
            fee_asset_id: 0x1.into(),
            fee_amount: 0x300b.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd4030.try_into().unwrap() },
            salt: 0x75290385.into(),
        },
        order_b: Order {
            position_id: PositionId { value: 0x18829.try_into().unwrap() },
            base_asset_id: 0x4144412d3100000000000000000000.into(),
            base_amount: -1650_i64,
            quote_asset_id: 0x1.into(),
            quote_amount: 0x8f0d75c.try_into().unwrap(),
            fee_asset_id: 0x1.into(),
            fee_amount: 0x0.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd4dfe.try_into().unwrap() },
            salt: 0xe4cc404e.into(),
        },
        actual_amount_base_a: 0x10e,
        actual_amount_quote_a: -24545700_i64,
        actual_fee_a: 0x17f8,
        actual_fee_b: 0x0,
    }
}

fn settlement_12() -> Settlement {
    Settlement {
        signature_a: array![
            0x28d2e8fb1e9fde97bf6db1e432190486cfb2d7d1039993a2146cf24248573cf,
            0x7d5cc648ac7b183a0e9dad938fa60961cfc51312aba6d872d531ad23100bf5a,
        ]
            .span(),
        signature_b: array![
            0x486fa727c5fd8e06ea32675fa9a6f7d49540f2c78db8faaf43aa512823a7b19,
            0x1a355a412f62144fe9500ab42e07e2ad6892937abb689581b7fd613224b3399,
        ]
            .span(),
        order_a: Order {
            position_id: PositionId { value: 0x18829.try_into().unwrap() },
            base_asset_id: 0x5749462d3100000000000000000000.into(),
            base_amount: 0x69a.try_into().unwrap(),
            quote_asset_id: 0x1.into(),
            quote_amount: -149666400_i64,
            fee_asset_id: 0x1.into(),
            fee_amount: 0x0.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd4e07.try_into().unwrap() },
            salt: 0xd40cdf0b.into(),
        },
        order_b: Order {
            position_id: PositionId { value: 0x1f9.try_into().unwrap() },
            base_asset_id: 0x5749462d3100000000000000000000.into(),
            base_amount: -360_i64,
            quote_asset_id: 0x1.into(),
            quote_amount: 0x1e5c2b0.try_into().unwrap(),
            fee_asset_id: 0x1.into(),
            fee_amount: 0x3e2e.try_into().unwrap(),
            expiration: Timestamp { seconds: 0x68bd4032.try_into().unwrap() },
            salt: 0x2b1f3065.into(),
        },
        actual_amount_base_a: 0x168,
        actual_amount_quote_a: -31881600_i64,
        actual_fee_a: 0x0,
        actual_fee_b: 0x1f22,
    }
}

fn settlements() -> Span<Settlement> {
    array![
        settlement_1(), settlement_2(), settlement_3(), settlement_4(), settlement_5(),
        settlement_6(), settlement_7(), settlement_8(), settlement_9(), settlement_10(),
        settlement_11(), settlement_12(),
    ]
        .span()
}

fn replace_to_current_code() {
    let replacer = IReplaceableDispatcher { contract_address: CONTRACT_ADDRESS };
    let core_contract_class_hash = (*snforge_std::declare("Core").unwrap().contract_class())
        .class_hash;
    let implementation_data = ImplementationData {
        impl_hash: core_contract_class_hash, eic_data: Option::None, final: false,
    };
    cheat_caller_address_once(contract_address: CONTRACT_ADDRESS, caller_address: DEPLOYER_ADDRESS);
    replacer.add_new_implementation(:implementation_data);
    cheat_caller_address_once(contract_address: CONTRACT_ADDRESS, caller_address: DEPLOYER_ADDRESS);
    replacer.replace_to(:implementation_data);
}
/// tx: 0x07b042c11b78c947b958f5559f40feac97866bc8b1ecc9ec62818f1a1b177586 (12 trades)
/// block number: 1844545
#[test]
#[fork(url: "https://rpc.pathfinder.equilibrium.co/mainnet/rpc/v0_10", block_number: 1844544)]
fn test_performance() {
    replace_to_current_code();

    // Execute multi trade:
    let dispatcher = ICoreDispatcher { contract_address: CONTRACT_ADDRESS };
    let trades = settlements();

    cheat_caller_address_once(contract_address: CONTRACT_ADDRESS, caller_address: OPERATOR_ADDRESS);
    dispatcher.multi_trade(operator_nonce: CURRENT_OPERATOR_NONCE, :trades);
}
