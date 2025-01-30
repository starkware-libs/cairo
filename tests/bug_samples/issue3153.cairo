use core::integer::u256_from_felt252;
use starknet::{
    StorageBaseAddress, Store, SyscallResult, storage_address_from_base_and_offset,
    storage_read_syscall, storage_write_syscall,
};

impl U256TryIntoU64 of TryInto<u256, u64> {
    fn try_into(self: u256) -> Option<u64> {
        let intermediate: Option<felt252> = self.try_into();
        match intermediate {
            Some(felt) => felt.try_into(),
            None => None,
        }
    }
}

const MASK_64: u256 = 0xFFFFFFFFFFFFFFFF;
const MASK_160: u256 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF;

const TWO_POW_160: u256 = 0x10000000000000000000000000000000000000000;

#[derive(Copy, Drop, Serde)]
struct Proposal {
    proposer: felt252,
    last_updated_at: u64,
}

/// Pack the proposal fields into a single felt252.
/// * `proposer` - The proposer of the proposal.
/// * `last_updated_at` - The last time the proposal was updated.
fn pack_proposal_fields(proposer: felt252, last_updated_at: u64) -> felt252 {
    let mut packed = 0;
    packed = packed | proposer.into();
    packed = packed | (u256_from_felt252(last_updated_at.into()) * TWO_POW_160);

    packed.try_into().unwrap()
}

/// Unpack the proposal fields from a single felt252.
/// * `packed` - The packed proposal.
fn unpack_proposal_fields(packed: felt252) -> (felt252, u64) {
    let packed = packed.into();

    let proposer = (packed & MASK_160).try_into().unwrap();
    let last_updated_at: u64 = U256TryIntoU64::try_into(((packed / TWO_POW_160) & MASK_64))
        .unwrap();

    (proposer, last_updated_at)
}

impl ProposalStore of Store<Proposal> {
    fn read(address_domain: u32, base: StorageBaseAddress) -> SyscallResult<Proposal> {
        Self::read_at_offset(address_domain, base, 0)
    }

    fn write(address_domain: u32, base: StorageBaseAddress, value: Proposal) -> SyscallResult<()> {
        Self::write_at_offset(address_domain, base, 0, value)
    }
    fn read_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8,
    ) -> SyscallResult<Proposal> {
        let (proposer, last_updated_at) = unpack_proposal_fields(
            storage_read_syscall(
                address_domain, storage_address_from_base_and_offset(base, offset),
            )?,
        );
        Ok(Proposal { proposer, last_updated_at })
    }

    fn write_at_offset(
        address_domain: u32, base: StorageBaseAddress, offset: u8, value: Proposal,
    ) -> SyscallResult<()> {
        let packed = pack_proposal_fields(value.proposer, value.last_updated_at);
        storage_write_syscall(
            address_domain, storage_address_from_base_and_offset(base, offset), packed,
        )
    }

    fn size() -> u8 {
        1
    }
}

#[starknet::contract]
mod test_contract {
    use starknet::storage::Map;
    use super::Proposal;
    #[storage]
    struct Storage {
        _proposals: Map<u32, Proposal>,
        _single_proposal: Proposal,
    }

    #[external(v0)]
    fn reproduce(ref self: ContractState) {
        self._single_proposal.read();

        self._proposals.write(1, Proposal { proposer: 0, last_updated_at: 0 });
    }
}
