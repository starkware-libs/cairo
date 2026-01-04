// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (access/accesscontrol/account_role_info.cairo)

use starknet::storage_access::StorePacking;

/// Information about whether a role is active for an account or not.
#[derive(Copy, Drop, Serde, PartialEq, Debug)]
pub struct AccountRoleInfo {
    pub effective_from: u64,
    pub is_granted: bool,
}

/// Packs an AccountRoleInfo into a single felt252.
///
/// The packing is done as follows:
///
/// 1. `effective_from` is stored at range [187,250] (0-indexed starting from the most
///   significant bits).
/// 2. `is_granted` is stored at range [251, 251], following `effective_from`.
impl AccountRoleInfoStorePacking of StorePacking<AccountRoleInfo, felt252> {
    fn pack(value: AccountRoleInfo) -> felt252 {
        let AccountRoleInfo { effective_from, is_granted } = value;

        // shift-left to reach the corresponding positions
        let effective_from_val = effective_from.into() * 2;
        let is_granted_val = if is_granted {
            1
        } else {
            0
        };

        effective_from_val + is_granted_val
    }

    fn unpack(value: felt252) -> AccountRoleInfo {
        let value: u256 = value.into();
        let effective_from = value / 2;
        let is_granted = value % 2 == 1;

        AccountRoleInfo { effective_from: effective_from.try_into().unwrap(), is_granted }
    }
}

#[cfg(test)]
mod tests {
    use core::num::traits::Bounded;
    use super::{AccountRoleInfo, AccountRoleInfoStorePacking};

    #[test]
    fn test_pack_and_unpack() {
        let account_role_info = AccountRoleInfo { effective_from: 100, is_granted: true };
        let packed = AccountRoleInfoStorePacking::pack(account_role_info);
        let unpacked = AccountRoleInfoStorePacking::unpack(packed);
        assert_eq!(account_role_info, unpacked);
    }

    #[test]
    fn test_pack_and_unpack_big_values() {
        let account_role_info = AccountRoleInfo { effective_from: Bounded::MAX, is_granted: true };
        let packed = AccountRoleInfoStorePacking::pack(account_role_info);
        let unpacked = AccountRoleInfoStorePacking::unpack(packed);
        assert_eq!(account_role_info, unpacked);
    }
}
