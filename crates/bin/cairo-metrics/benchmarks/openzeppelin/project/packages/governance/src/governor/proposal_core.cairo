// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (governance/src/governor/proposal_core.cairo)

use core::traits::DivRem;
use starknet::ContractAddress;
use starknet::storage_access::StorePacking;

/// Proposal state.
#[derive(Copy, Drop, Serde, PartialEq, Debug)]
pub struct ProposalCore {
    pub proposer: ContractAddress,
    pub vote_start: u64,
    pub vote_duration: u64,
    pub executed: bool,
    pub canceled: bool,
    pub eta_seconds: u64,
}

const _2_POW_184: felt252 = 0x10000000000000000000000000000000000000000000000;
const _2_POW_120: felt252 = 0x1000000000000000000000000000000;
const _2_POW_56: felt252 = 0x100000000000000;
const _2_POW_55: felt252 = 0x80000000000000;
const _2_POW_54: felt252 = 0x40000000000000;

/// Packs a ProposalCore into a (felt252, felt252).
///
/// The packing is done as follows:
///
/// 1. The first felt of the tuple contains `proposer` serialized.
/// 2. The second felt of the tuple contains `vote_start`, `vote_duration`, `eta_seconds`,
/// `executed`, and `canceled` organized as:
///   - `vote_start` is stored at range [4,67] bits (0-indexed), taking the most significant usable
/// bits.
///   - `vote_duration` is stored at range [68, 131], following `vote_start`.
///   - `eta_seconds` is stored at range [132, 195], following `vote_duration`.
///   - `executed` is stored at range [133, 133], following `eta_seconds`.
///   - `canceled` is stored at range [134, 134], following `executed`.
///
/// NOTE: In the second felt252, the first four bits are skipped to avoid representation errors due
/// to `felt252` max value being a bit less than a 252 bits number max value
/// (https://docs.starknet.io/architecture-and-concepts/cryptography/#stark-field).
impl ProposalCoreStorePacking of StorePacking<ProposalCore, (felt252, felt252)> {
    fn pack(value: ProposalCore) -> (felt252, felt252) {
        let proposal = value;

        // shift-left to reach the corresponding positions
        let vote_start = proposal.vote_start.into() * _2_POW_184;
        let vote_duration = proposal.vote_duration.into() * _2_POW_120;
        let eta_seconds = proposal.eta_seconds.into() * _2_POW_56;
        let executed = proposal.executed.into() * _2_POW_55;
        let canceled = proposal.canceled.into() * _2_POW_54;

        let second_felt = vote_start + vote_duration + eta_seconds + executed + canceled;

        (proposal.proposer.into(), second_felt)
    }

    fn unpack(value: (felt252, felt252)) -> ProposalCore {
        let (proposer, second_felt) = value;
        let _2_POW_64: NonZero<u256> = 0x10000000000000000;

        // shift-right to extract the corresponding values
        let val: u256 = second_felt.into() / _2_POW_54.into();

        let (val, canceled) = DivRem::div_rem(val, 2);
        let (val, executed) = DivRem::div_rem(val, 2);
        let (val, eta_seconds) = DivRem::div_rem(val, _2_POW_64);
        let (val, vote_duration) = DivRem::div_rem(val, _2_POW_64);
        let (_, vote_start) = DivRem::div_rem(val, _2_POW_64);

        ProposalCore {
            proposer: proposer.try_into().unwrap(),
            vote_start: vote_start.try_into().unwrap(),
            vote_duration: vote_duration.try_into().unwrap(),
            executed: executed > 0,
            canceled: canceled > 0,
            eta_seconds: eta_seconds.try_into().unwrap(),
        }
    }
}

#[cfg(test)]
mod tests {
    use core::num::traits::Bounded;
    use openzeppelin_testing::constants::ALICE;
    use super::{ProposalCore, ProposalCoreStorePacking};

    #[test]
    fn test_pack_and_unpack() {
        let proposal = ProposalCore {
            proposer: ALICE,
            vote_start: 100,
            vote_duration: 200,
            executed: false,
            canceled: true,
            eta_seconds: 300,
        };
        let packed = ProposalCoreStorePacking::pack(proposal);
        let unpacked = ProposalCoreStorePacking::unpack(packed);
        assert_eq!(proposal, unpacked);
    }

    #[test]
    fn test_pack_and_unpack_big_values() {
        let proposal = ProposalCore {
            proposer: ALICE,
            vote_start: Bounded::MAX,
            vote_duration: Bounded::MAX,
            executed: true,
            canceled: true,
            eta_seconds: Bounded::MAX,
        };
        let packed = ProposalCoreStorePacking::pack(proposal);
        let unpacked = ProposalCoreStorePacking::unpack(packed);
        assert_eq!(proposal, unpacked);
    }
}
