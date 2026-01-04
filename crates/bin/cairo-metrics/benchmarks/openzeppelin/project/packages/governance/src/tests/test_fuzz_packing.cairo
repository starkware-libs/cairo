use core::integer::u128_safe_divmod;
use core::num::traits::Bounded;
use starknet::storage_access::StorePacking;
use crate::multisig::storage_utils::{SignersInfo, SignersInfoStorePackingV2};

#[test]
#[fuzzer]
fn test_signers_info_pack_unpack_v2(quorum: u32, signers_count: u32) {
    let info = SignersInfo { quorum, signers_count };
    let packed_value = SignersInfoStorePackingV2::pack(info);
    let unpacked_info = SignersInfoStorePackingV2::unpack(packed_value);

    assert_eq!(unpacked_info.quorum, quorum);
    assert_eq!(unpacked_info.signers_count, signers_count);
}

#[test]
#[fuzzer]
fn test_signers_info_pack_with_v1_unpack_with_v2(quorum: u32, signers_count: u32) {
    if signers_count == Bounded::MAX {
        // Cannot properly unpack if packed with V1 and `signers_count` is max u32 value
        return;
    }
    let info = SignersInfo { quorum, signers_count };
    let packed_value = LegacySignersInfoStorePackingV1::pack(info);
    let unpacked_info = SignersInfoStorePackingV2::unpack(packed_value);

    assert_eq!(unpacked_info.quorum, quorum);
    assert_eq!(unpacked_info.signers_count, signers_count);
}

//
// Helpers
//

const MAX_U32: NonZero<u128> = 0xffffffff;

impl LegacySignersInfoStorePackingV1 of StorePacking<SignersInfo, u128> {
    fn pack(value: SignersInfo) -> u128 {
        let SignersInfo { quorum, signers_count } = value;
        quorum.into() * MAX_U32.into() + signers_count.into()
    }

    fn unpack(value: u128) -> SignersInfo {
        let (quorum, signers_count) = u128_safe_divmod(value, MAX_U32);
        SignersInfo {
            quorum: quorum.try_into().unwrap(), signers_count: signers_count.try_into().unwrap(),
        }
    }
}
