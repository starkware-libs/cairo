use crate::core::types::asset::AssetId;
use crate::core::types::position::PositionId;

const STORAGE_VERSION: u8 = 1;


#[starknet::interface]
pub trait IVaults<TContractState> {
    fn is_vault_position(ref self: TContractState, vault_position: PositionId) -> bool;
    fn is_vault_asset(ref self: TContractState, asset_id: AssetId) -> bool;
}

#[starknet::component]
pub mod Vaults {
    use core::num::traits::Zero;
    use core::panic_with_felt252;
    use openzeppelin::access::accesscontrol::AccessControlComponent;
    use openzeppelin::interfaces::erc4626::{IERC4626Dispatcher, IERC4626DispatcherTrait};
    use openzeppelin::introspection::src5::SRC5Component;
    use perpetuals::core::components::assets::AssetsComponent;
    use perpetuals::core::components::assets::interface::IAssets;
    use perpetuals::core::components::deposit::Deposit::InternalImpl as DepositInternal;
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent;
    use perpetuals::core::components::positions::Positions as PositionsComponent;
    use perpetuals::core::components::positions::Positions::InternalTrait as PositionsInternal;
    use perpetuals::core::components::vaults::types::VaultConfig;
    use perpetuals::core::types::asset::AssetId;
    use perpetuals::core::types::asset::synthetic::AssetType;
    use perpetuals::core::types::position::{PositionId, PositionTrait};
    use starknet::storage::{Map, StorageMapReadAccess, StorageMapWriteAccess};
    use starkware_utils::components::pausable::PausableComponent;
    use starkware_utils::components::request_approvals::RequestApprovalsComponent;
    use starkware_utils::components::roles::RolesComponent;
    use starkware_utils::signature::stark::Signature;
    use starkware_utils::storage::iterable_map::{
        IterableMapIntoIterImpl, IterableMapReadAccessImpl, IterableMapWriteAccessImpl,
    };
    use starkware_utils::time::time::validate_expiration;
    use vault::interface::{IProtocolVaultDispatcher, IProtocolVaultDispatcherTrait};
    use crate::core::components::snip::SNIP12MetadataImpl;
    use crate::core::components::vaults::events;
    use crate::core::types::vault::ConvertPositionToVault;
    use crate::core::utils::validate_signature;
    use super::{IVaults, STORAGE_VERSION};


    #[event]
    #[derive(Drop, PartialEq, starknet::Event)]
    pub enum Event {
        VaultOpened: events::VaultOpened,
    }

    #[storage]
    pub struct Storage {
        registered_vaults_by_asset: Map<AssetId, VaultConfig>,
        registered_vaults_by_position: Map<PositionId, VaultConfig>,
    }

    #[embeddable_as(VaultsImpl)]
    impl VaultsComponent<
        TContractState, +HasComponent<TContractState>, +Drop<TContractState>,
    > of IVaults<ComponentState<TContractState>> {
        fn is_vault_position(
            ref self: ComponentState<TContractState>, vault_position: PositionId,
        ) -> bool {
            return self.registered_vaults_by_position.read(vault_position).version != 0;
        }

        fn is_vault_asset(ref self: ComponentState<TContractState>, asset_id: AssetId) -> bool {
            return self.registered_vaults_by_asset.read(asset_id).version != 0;
        }
    }


    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        +Drop<TContractState>,
        +AccessControlComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl Assets: AssetsComponent::HasComponent<TContractState>,
        impl OperatorNonce: OperatorNonceComponent::HasComponent<TContractState>,
        impl Pausable: PausableComponent::HasComponent<TContractState>,
        impl Positions: PositionsComponent::HasComponent<TContractState>,
        impl Roles: RolesComponent::HasComponent<TContractState>,
        impl RequestApprovals: RequestApprovalsComponent::HasComponent<TContractState>,
    > of InternalTrait<TContractState> {
        fn get_vault_config_for_position(
            ref self: ComponentState<TContractState>, vault_position: PositionId,
        ) -> VaultConfig {
            let vault_config = self.registered_vaults_by_position.read(vault_position);
            assert(vault_config.version != 0, 'UNKNOWN_VAULT');
            vault_config
        }

        fn get_vault_config_for_asset(
            ref self: ComponentState<TContractState>, vault_asset_id: AssetId,
        ) -> VaultConfig {
            let vault_config = self.registered_vaults_by_asset.read(vault_asset_id);
            assert(vault_config.version != 0, 'UNKNOWN_VAULT');
            vault_config
        }

        fn activate_vault(
            ref self: ComponentState<TContractState>,
            operator_nonce: u64,
            order: ConvertPositionToVault,
            signature: Signature,
        ) {
            let vault_asset_id = order.vault_asset_id;
            let vault_position = order.position_to_convert;
            let expiration = order.expiration;
            validate_expiration(:expiration, err: 'ACTIVATE_ORDER_EXPIRED');

            let mut positions = get_dep_component_mut!(ref self, Positions);
            let assets = get_dep_component!(@self, Assets);
            let position_info = positions.get_position_snapshot(vault_position);

            /// Validations:

            validate_signature(
                public_key: position_info.get_owner_public_key(),
                message: order,
                signature: signature,
            );

            let existing_entry = self.registered_vaults_by_asset.read(vault_asset_id);
            assert(existing_entry.version == 0, 'VAULT_ALREADY_ACTIVATED');

            let existing_position_entry = self.registered_vaults_by_position.read(vault_position);
            assert(existing_position_entry.version == 0, 'VAULT_ALREADY_ACTIVATED');

            let asset_config = assets.get_asset_config(vault_asset_id);

            assert(asset_config.asset_type == AssetType::VAULT_SHARE_COLLATERAL, 'NOT_VAULT_SHARE');

            let erc4626_dispatcher = IERC4626Dispatcher {
                contract_address: asset_config.token_contract.expect('NOT_ERC4626'),
            };

            let vault_dispatcher = IProtocolVaultDispatcher {
                contract_address: asset_config.token_contract.expect('NOT_ERC4626'),
            };
            assert(
                vault_dispatcher.get_owning_position_id() == vault_position.value,
                'VAULT_OWNERSHIP_MISMATCH',
            );

            assert(
                vault_dispatcher.get_perps_contract() == starknet::get_contract_address(),
                'VAULT_PERPS_CONTRACT_MISMATCH',
            );

            assert(
                erc4626_dispatcher
                    .asset() == assets
                    .get_base_collateral_token_contract()
                    .contract_address,
                'VAULT_ASSET_MISMATCH',
            );

            for (asset_id, balance_info) in position_info.asset_balances {
                if balance_info.balance.is_zero() {
                    continue;
                }
                let asset_config = assets.get_asset_config(asset_id);

                if (asset_config.asset_type == AssetType::VAULT_SHARE_COLLATERAL) {
                    panic_with_felt252('VAULT_CANNOT_HOLD_VAULT');
                }
            }

            self
                .registered_vaults_by_asset
                .write(
                    vault_asset_id,
                    VaultConfig {
                        version: STORAGE_VERSION,
                        asset_id: vault_asset_id,
                        position_id: vault_position.value,
                    },
                );

            self
                .registered_vaults_by_position
                .write(
                    vault_position,
                    VaultConfig {
                        version: STORAGE_VERSION,
                        asset_id: vault_asset_id,
                        position_id: vault_position.value,
                    },
                );

            self
                .emit(
                    Event::VaultOpened(
                        events::VaultOpened {
                            position_id: vault_position, asset_id: vault_asset_id,
                        },
                    ),
                )
        }
    }
}
