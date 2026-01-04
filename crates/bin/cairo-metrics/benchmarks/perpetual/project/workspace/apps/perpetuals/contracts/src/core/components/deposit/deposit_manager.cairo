use core::hash::{HashStateExTrait, HashStateTrait};
use core::pedersen::PedersenTrait;
use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::position::PositionId;
use starknet::ContractAddress;
use starkware_utils::signature::stark::HashType;
use starkware_utils::time::time::{TimeDelta, Timestamp};
use super::interface::DepositStatus;


#[starknet::interface]
pub trait IDepositExternal<TContractState> {
    fn deposit(
        ref self: TContractState,
        caller_address: ContractAddress,
        asset_id: AssetId,
        position_id: PositionId,
        quantized_amount: u64,
        now: Timestamp,
        salt: felt252,
    );
    fn cancel_deposit(
        ref self: TContractState,
        depositor: ContractAddress,
        asset_id: AssetId,
        position_id: PositionId,
        quantized_amount: u64,
        now: Timestamp,
        salt: felt252,
    );
    fn reject_deposit(
        ref self: TContractState,
        depositor: ContractAddress,
        asset_id: AssetId,
        position_id: PositionId,
        quantized_amount: u64,
        now: Timestamp,
        salt: felt252,
    );
    fn process_deposit(
        ref self: TContractState,
        depositor: ContractAddress,
        asset_id: AssetId,
        position_id: PositionId,
        quantized_amount: u64,
        salt: felt252,
    );
    fn get_deposit_status(self: @TContractState, deposit_hash: HashType) -> DepositStatus;
    fn get_cancel_delay(self: @TContractState) -> TimeDelta;
}

#[starknet::contract]
pub(crate) mod DepositManager {
    use core::num::traits::Zero;
    use core::panic_with_felt252;
    use openzeppelin::access::accesscontrol::AccessControlComponent;
    use openzeppelin::interfaces::erc20::{IERC20Dispatcher, IERC20DispatcherTrait};
    use openzeppelin::introspection::src5::SRC5Component;
    use perpetuals::core::components::assets::AssetsComponent;
    use perpetuals::core::components::assets::interface::IAssets;
    use perpetuals::core::components::deposit::Deposit as DepositComponent;
    use perpetuals::core::components::fulfillment::fulfillment::Fulfillement as FulfillmentComponent;
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent;
    use perpetuals::core::components::positions::Positions as PositionsComponent;
    use perpetuals::core::components::positions::Positions::InternalTrait as PositionsInternal;
    use perpetuals::core::types::asset::AssetId;
    use perpetuals::core::types::position::PositionId;
    use starknet::storage::{StorageMapReadAccess, StorageMapWriteAccess, StoragePointerReadAccess};
    use starknet::{ContractAddress, get_contract_address};
    use starkware_utils::components::pausable::PausableComponent;
    use starkware_utils::components::request_approvals::RequestApprovalsComponent;
    use starkware_utils::components::roles::RolesComponent;
    use starkware_utils::storage::iterable_map::{
        IterableMapIntoIterImpl, IterableMapReadAccessImpl, IterableMapWriteAccessImpl,
    };
    use starkware_utils::time::time::Time;
    use crate::core::components::deposit::events;
    use crate::core::components::external_components::interface::EXTERNAL_COMPONENT_DEPOSITS;
    use crate::core::components::external_components::named_component::ITypedComponent;
    use crate::core::components::snip::SNIP12MetadataImpl;
    use crate::core::components::vaults::vaults::{IVaults, Vaults as VaultsComponent};
    use crate::core::types::asset::synthetic::AssetType;
    use crate::core::types::position::PositionDiff;
    use super::super::errors;
    use super::{DepositStatus, HashType, IDepositExternal, TimeDelta, Timestamp, deposit_hash};


    #[event]
    #[derive(Drop, starknet::Event)]
    pub enum Event {
        #[flat]
        FulfillmentEvent: FulfillmentComponent::Event,
        #[flat]
        PausableEvent: PausableComponent::Event,
        #[flat]
        OperatorNonceEvent: OperatorNonceComponent::Event,
        #[flat]
        AssetsEvent: AssetsComponent::Event,
        #[flat]
        PositionsEvent: PositionsComponent::Event,
        #[flat]
        DepositEvent: DepositComponent::Event,
        #[flat]
        RequestApprovalsEvent: RequestApprovalsComponent::Event,
        #[flat]
        SRC5Event: SRC5Component::Event,
        #[flat]
        AccessControlEvent: AccessControlComponent::Event,
        #[flat]
        RolesEvent: RolesComponent::Event,
        #[flat]
        VaultsEvent: VaultsComponent::Event,
        Deposit: events::Deposit,
        DepositCanceled: events::DepositCanceled,
        DepositProcessed: events::DepositProcessed,
    }

    #[storage]
    pub struct Storage {
        #[substorage(v0)]
        accesscontrol: AccessControlComponent::Storage,
        #[substorage(v0)]
        operator_nonce: OperatorNonceComponent::Storage,
        #[substorage(v0)]
        pausable: PausableComponent::Storage,
        #[substorage(v0)]
        pub roles: RolesComponent::Storage,
        #[substorage(v0)]
        #[allow(starknet::colliding_storage_paths)]
        pub assets: AssetsComponent::Storage,
        #[substorage(v0)]
        pub deposits: DepositComponent::Storage,
        #[substorage(v0)]
        pub positions: PositionsComponent::Storage,
        #[substorage(v0)]
        pub fulfillment_tracking: FulfillmentComponent::Storage,
        #[substorage(v0)]
        src5: SRC5Component::Storage,
        #[substorage(v0)]
        pub request_approvals: RequestApprovalsComponent::Storage,
        #[substorage(v0)]
        pub vaults: VaultsComponent::Storage,
    }

    component!(path: FulfillmentComponent, storage: fulfillment_tracking, event: FulfillmentEvent);
    component!(path: PausableComponent, storage: pausable, event: PausableEvent);
    component!(path: OperatorNonceComponent, storage: operator_nonce, event: OperatorNonceEvent);
    component!(path: AssetsComponent, storage: assets, event: AssetsEvent);
    component!(path: PositionsComponent, storage: positions, event: PositionsEvent);
    component!(path: DepositComponent, storage: deposits, event: DepositEvent);
    component!(path: RolesComponent, storage: roles, event: RolesEvent);
    component!(path: SRC5Component, storage: src5, event: SRC5Event);
    component!(path: AccessControlComponent, storage: accesscontrol, event: AccessControlEvent);
    component!(
        path: RequestApprovalsComponent, storage: request_approvals, event: RequestApprovalsEvent,
    );

    component!(path: VaultsComponent, storage: vaults, event: VaultsEvent);

    #[abi(embed_v0)]
    impl TypedComponent of ITypedComponent<ContractState> {
        fn component_type(ref self: ContractState) -> felt252 {
            EXTERNAL_COMPONENT_DEPOSITS
        }
    }

    #[abi(embed_v0)]
    impl DepositsManagerImpl of IDepositExternal<ContractState> {
        fn deposit(
            ref self: ContractState,
            caller_address: ContractAddress,
            asset_id: AssetId,
            position_id: PositionId,
            quantized_amount: u64,
            now: Timestamp,
            salt: felt252,
        ) {
            self._deposit(:caller_address, :asset_id, :position_id, :quantized_amount, :now, :salt);
        }

        fn cancel_deposit(
            ref self: ContractState,
            depositor: ContractAddress,
            asset_id: AssetId,
            position_id: PositionId,
            quantized_amount: u64,
            now: Timestamp,
            salt: felt252,
        ) {
            self
                ._cancel_deposit(
                    depositor: depositor,
                    asset_id: asset_id,
                    position_id: position_id,
                    quantized_amount: quantized_amount,
                    salt: salt,
                    now: now,
                    cancel_delay: self.get_cancel_delay(),
                )
        }

        fn reject_deposit(
            ref self: ContractState,
            depositor: starknet::ContractAddress,
            asset_id: AssetId,
            position_id: PositionId,
            quantized_amount: u64,
            now: Timestamp,
            salt: felt252,
        ) {
            self
                ._cancel_deposit(
                    depositor: depositor,
                    asset_id: asset_id,
                    position_id: position_id,
                    quantized_amount: quantized_amount,
                    salt: salt,
                    now: now,
                    cancel_delay: TimeDelta { seconds: 0 },
                )
        }

        fn process_deposit(
            ref self: ContractState,
            depositor: starknet::ContractAddress,
            asset_id: AssetId,
            position_id: PositionId,
            quantized_amount: u64,
            salt: felt252,
        ) {
            let (token_contract, quantum, position_diff) = if (self
                .assets
                .get_collateral_id() == asset_id) {
                let token_contract = self.assets.get_base_collateral_token_contract();
                let quantum = self.assets.get_collateral_quantum();
                let position_diff = PositionDiff {
                    collateral_diff: quantized_amount.into(), asset_diff: Option::None,
                };
                (token_contract, quantum, position_diff)
            } else {
                let asset_config = self.assets.get_asset_config(asset_id);
                assert(asset_config.asset_type != AssetType::SYNTHETIC, 'NOT_SPOT_ASSET');
                let token_contract = IERC20Dispatcher {
                    contract_address: asset_config.token_contract.expect('NO_ERC20_CONFIGURED'),
                };
                let position_diff = PositionDiff {
                    collateral_diff: 0_i64.into(),
                    asset_diff: Option::Some((asset_id, quantized_amount.into())),
                };
                (token_contract, asset_config.quantum, position_diff)
            };

            let unquantized_amount: u256 = quantized_amount.into() * quantum.into();
            let deposit_hash = deposit_hash(
                token_address: token_contract.contract_address,
                :depositor,
                :position_id,
                :quantized_amount,
                :salt,
            );
            let deposit_status = self.get_deposit_status(:deposit_hash);
            match deposit_status {
                DepositStatus::NOT_REGISTERED => {
                    panic_with_felt252(errors::DEPOSIT_NOT_REGISTERED)
                },
                DepositStatus::PROCESSED => {
                    panic_with_felt252(errors::DEPOSIT_ALREADY_PROCESSED)
                },
                DepositStatus::CANCELED => { panic_with_felt252(errors::DEPOSIT_ALREADY_CANCELED) },
                DepositStatus::PENDING(_) => {},
            }
            self.deposits.registered_deposits.write(deposit_hash, DepositStatus::PROCESSED);
            self.positions.apply_diff(:position_id, :position_diff);
            self
                .emit(
                    events::DepositProcessed {
                        position_id,
                        depositing_address: depositor,
                        collateral_id: asset_id,
                        quantized_amount,
                        unquantized_amount,
                        deposit_request_hash: deposit_hash,
                        salt,
                    },
                );
        }

        fn get_deposit_status(self: @ContractState, deposit_hash: felt252) -> DepositStatus {
            return self._get_deposit_status(:deposit_hash);
        }

        fn get_cancel_delay(self: @ContractState) -> TimeDelta {
            self.deposits.cancel_delay.read()
        }
    }

    #[generate_trait]
    pub impl InternalFunctions of DepositManagerFunctions {
        fn _get_deposit_status(self: @ContractState, deposit_hash: HashType) -> DepositStatus {
            self.deposits.registered_deposits.read(deposit_hash)
        }

        fn _deposit(
            ref self: ContractState,
            caller_address: ContractAddress,
            asset_id: AssetId,
            position_id: PositionId,
            quantized_amount: u64,
            now: Timestamp,
            salt: felt252,
        ) {
            // check recipient position exists
            self.positions.get_position_snapshot(:position_id);
            assert(quantized_amount.is_non_zero(), errors::ZERO_AMOUNT);
            let (token_contract, quantum) = if (self.assets.get_collateral_id() == asset_id) {
                let token_contract = self.assets.get_base_collateral_token_contract();
                let quantum = self.assets.get_collateral_quantum();
                (token_contract, quantum)
            } else {
                let asset_config = self.assets.get_asset_config(asset_id);
                if (asset_config.asset_type == AssetType::VAULT_SHARE_COLLATERAL) {
                    assert(
                        !self.vaults.is_vault_position(position_id),
                        'DEPOSIT_VAULT_SHARES_INTO_VAULT',
                    );
                }
                assert(asset_config.asset_type != AssetType::SYNTHETIC, 'NOT_SPOT_ASSET');
                let token_contract = IERC20Dispatcher {
                    contract_address: asset_config.token_contract.expect('NO_ERC20_CONFIGURED'),
                };
                (token_contract, asset_config.quantum)
            };

            let deposit_hash = deposit_hash(
                token_address: token_contract.contract_address,
                depositor: caller_address,
                :position_id,
                :quantized_amount,
                :salt,
            );
            assert(
                self.get_deposit_status(:deposit_hash) == DepositStatus::NOT_REGISTERED,
                errors::DEPOSIT_ALREADY_REGISTERED,
            );
            self
                .deposits
                .registered_deposits
                .write(key: deposit_hash, value: DepositStatus::PENDING(now));

            let unquantized_amount = quantized_amount * quantum.into();

            assert(
                token_contract
                    .transfer_from(
                        sender: caller_address,
                        recipient: get_contract_address(),
                        amount: unquantized_amount.into(),
                    ),
                errors::TRANSFER_FAILED,
            );

            self
                .emit(
                    events::Deposit {
                        position_id,
                        depositing_address: caller_address,
                        collateral_id: asset_id,
                        quantized_amount,
                        unquantized_amount,
                        deposit_request_hash: deposit_hash,
                        salt,
                    },
                );
        }

        fn _cancel_deposit(
            ref self: ContractState,
            depositor: ContractAddress,
            asset_id: AssetId,
            position_id: PositionId,
            quantized_amount: u64,
            salt: felt252,
            now: Timestamp,
            cancel_delay: TimeDelta,
        ) {
            let (token_contract, quantum) = if (self.assets.get_collateral_id() == asset_id) {
                let token_contract = self.assets.get_base_collateral_token_contract();
                let quantum = self.assets.get_collateral_quantum();
                (token_contract, quantum)
            } else {
                let asset_config = self.assets.get_asset_config(asset_id);
                assert(asset_config.asset_type != AssetType::SYNTHETIC, 'NOT_SPOT_ASSET');
                let token_contract = IERC20Dispatcher {
                    contract_address: asset_config.token_contract.expect('NO_ERC20_CONFIGURED'),
                };
                (token_contract, asset_config.quantum)
            };

            let unquantized_amount: u256 = quantized_amount.into() * quantum.into();

            let deposit_hash = deposit_hash(
                token_address: token_contract.contract_address,
                :depositor,
                :position_id,
                :quantized_amount,
                :salt,
            );
            match self._get_deposit_status(:deposit_hash) {
                DepositStatus::PENDING(deposit_timestamp) => assert(
                    now > deposit_timestamp.add(cancel_delay), errors::DEPOSIT_NOT_CANCELABLE,
                ),
                DepositStatus::NOT_REGISTERED => panic_with_felt252(errors::DEPOSIT_NOT_REGISTERED),
                DepositStatus::PROCESSED => panic_with_felt252(errors::DEPOSIT_ALREADY_PROCESSED),
                DepositStatus::CANCELED => panic_with_felt252(errors::DEPOSIT_ALREADY_CANCELED),
            }

            self
                .deposits
                .registered_deposits
                .write(key: deposit_hash, value: DepositStatus::CANCELED);
            assert(
                token_contract.transfer(recipient: depositor, amount: unquantized_amount.into()),
                errors::TRANSFER_FAILED,
            );
            self
                .emit(
                    events::DepositCanceled {
                        position_id,
                        depositing_address: depositor,
                        collateral_id: asset_id,
                        quantized_amount,
                        unquantized_amount,
                        deposit_request_hash: deposit_hash,
                        salt,
                    },
                );
        }
    }
}

pub fn deposit_hash(
    token_address: ContractAddress,
    depositor: ContractAddress,
    position_id: PositionId,
    quantized_amount: u64,
    salt: felt252,
) -> HashType {
    PedersenTrait::new(base: token_address.into())
        .update_with(value: depositor)
        .update_with(value: position_id)
        .update_with(value: quantized_amount)
        .update_with(value: salt)
        .finalize()
}
