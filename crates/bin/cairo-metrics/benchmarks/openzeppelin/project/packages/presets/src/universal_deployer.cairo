// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (presets/src/universal_deployer.cairo)

/// # UniversalDeployerContract Preset
///
/// The Universal Deployer Contract is a standardized generic factory of Starknet contracts.
#[starknet::contract]
pub mod UniversalDeployer {
    use core::pedersen::pedersen;
    use openzeppelin_utils::interfaces::UniversalDeployerABI;
    use starknet::{ClassHash, ContractAddress, SyscallResultTrait, get_caller_address};

    #[storage]
    pub struct Storage {}

    #[event]
    #[derive(Drop, PartialEq, starknet::Event)]
    pub(crate) enum Event {
        ContractDeployed: ContractDeployed,
    }

    #[derive(Drop, PartialEq, starknet::Event)]
    pub(crate) struct ContractDeployed {
        pub(crate) address: ContractAddress,
        pub(crate) deployer: ContractAddress,
        pub(crate) not_from_zero: bool,
        pub(crate) class_hash: ClassHash,
        pub(crate) calldata: Span<felt252>,
        pub(crate) salt: felt252,
    }

    #[abi(embed_v0)]
    impl UniversalDeployerImpl of UniversalDeployerABI<ContractState> {
        /// Deploys a contract with the given class hash, salt, notFromZero flag, and calldata,
        /// returning the address of the deployed contract.
        ///
        /// Arguments:
        ///
        /// - `class_hash`: The class hash of the contract to deploy.
        /// - `salt`: The salt to use for the deployment.
        /// - `not_from_zero`: Used to determine if the deployer address should be used in the
        /// target address computation.
        /// - `calldata`: The calldata to pass to the target contract.
        ///
        /// Emits a `ContractDeployed` event.
        fn deploy_contract(
            ref self: ContractState,
            class_hash: ClassHash,
            salt: felt252,
            not_from_zero: bool,
            calldata: Span<felt252>,
        ) -> ContractAddress {
            let deployer: ContractAddress = get_caller_address();

            // If the contract must be deployed taking the deployer user address into account,
            // we need to add the caller address into the target address computation somehow, since
            // the actual deployer (executing the syscall) is the UDC, not the user account.
            let final_salt = if not_from_zero {
                pedersen(deployer.into(), salt)
            } else {
                salt
            };

            let deploy_from_zero = !not_from_zero;
            let (address, _) = starknet::syscalls::deploy_syscall(
                class_hash, final_salt, calldata, deploy_from_zero,
            )
                .unwrap_syscall();

            self
                .emit(
                    ContractDeployed {
                        address, deployer, not_from_zero, class_hash, calldata, salt,
                    },
                );

            return address;
        }

        /// CamelCase version of `deploy_contract`.
        fn deployContract(
            ref self: ContractState,
            classHash: ClassHash,
            salt: felt252,
            notFromZero: bool,
            calldata: Span<felt252>,
        ) -> ContractAddress {
            self.deploy_contract(classHash, salt, notFromZero, calldata)
        }
    }
}
