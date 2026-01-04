// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (governance/src/multisig/storage_utils.cairo)

use core::integer::u128_safe_divmod;
use starknet::storage_access::StorePacking;

/// Helper struct for `MultisigComponent` that optimizes how transaction-related information
/// is stored, including the transaction's execution status and the block it was submitted in.
#[derive(Drop)]
pub struct TxInfo {
    pub is_executed: bool,
    pub submitted_block: u64,
}

/// Packs a `TxInfo` entity into a `u128` value.
///
/// The packing is done as follows:
/// - The boolean `is_executed` is stored as a single bit at the highest bit position (index 127).
/// - The `submitted_block` value occupies 64 bits in the range [63..126].
pub impl TxInfoStorePacking of StorePacking<TxInfo, u128> {
    fn pack(value: TxInfo) -> u128 {
        let TxInfo { is_executed, submitted_block } = value;
        let is_executed_value = if is_executed {
            1
        } else {
            0
        };
        submitted_block.into() * 2 + is_executed_value
    }

    fn unpack(value: u128) -> TxInfo {
        let (submitted_block, is_executed_value) = u128_safe_divmod(value, 2);
        let is_executed = is_executed_value == 1;
        TxInfo { is_executed, submitted_block: submitted_block.try_into().unwrap() }
    }
}

/// Helper struct for `MultisigComponent` that optimizes how the quorum
/// value and the total number of signers are stored.
#[derive(Drop)]
pub struct SignersInfo {
    pub quorum: u32,
    pub signers_count: u32,
}

const _2_POW_32: NonZero<u128> = 0x100000000;
const MAX_U32: NonZero<u128> = 0xffffffff;
const V2_SIGNAL_BIT: u128 = 0x10000000000000000;

/// Packs a `SignersInfo` entity into a `u128` value.
///
/// The packing is done as follows:
/// - `quorum` value occupies 32 bits in bit range [64..95].
/// - `signers_count` value occupies the highest 32 bits in bit range [96..127].
/// - The 63rd bit indicates the packing version: 0 for V1 (legacy) and 1 for V2 (current).
///
/// WARNING: A bug in the original `StorePacking` trait for `SignersInfo` multiplied the quorum
/// value by 0xffffffff instead of 0x100000000. This affected only the case where `signers_count`
/// was 0xffffffff, resulting in an off-by-one quorum value and a `signers_count` of 0 after
/// unpacking. The issue is resolved in V2 of `StorePacking`. For backward compatibility, V2 can
/// still correctly unpack values from both V1 and V2 by checking the 63rd bit (0 for V1, 1 for V2).
pub impl SignersInfoStorePackingV2 of StorePacking<SignersInfo, u128> {
    fn pack(value: SignersInfo) -> u128 {
        let SignersInfo { quorum, signers_count } = value;
        let quorum_val = quorum.into() * _2_POW_32.into();
        V2_SIGNAL_BIT + quorum_val + signers_count.into()
    }

    fn unpack(value: u128) -> SignersInfo {
        if value == 0 {
            return SignersInfo { quorum: 0, signers_count: 0 };
        }
        let is_packed_with_v1 = value < V2_SIGNAL_BIT;
        let (quorum, signers_count) = if is_packed_with_v1 {
            u128_safe_divmod(value, MAX_U32)
        } else {
            u128_safe_divmod(value - V2_SIGNAL_BIT, _2_POW_32)
        };
        SignersInfo {
            quorum: quorum.try_into().unwrap(), signers_count: signers_count.try_into().unwrap(),
        }
    }
}
